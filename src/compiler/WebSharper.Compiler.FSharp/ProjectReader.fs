// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

module WebSharper.Compiler.FSharp.ProjectReader

open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata
open WebSharper.Compiler
open WebSharper.Compiler.NotResolved
open WebSharper.Compiler.CommandTools

module A = WebSharper.Compiler.AttributeReader
module QR = WebSharper.Compiler.QuotationReader

type FSIFD = FSharpImplementationFileDeclaration
type FSMFV = FSharpMemberOrFunctionOrValue

type private StartupCode = ResizeArray<Statement> * HashSet<string> 

type private N = NotResolvedMemberKind

type private SourceMemberOrEntity =
    | SourceMember of FSMFV * list<list<FSMFV>> * FSharpExpr
    | SourceEntity of FSharpEntity * ResizeArray<SourceMemberOrEntity>
    | SourceInterface of FSharpEntity 
    | InitAction of FSharpExpr

let annotForTypeOrFile name (annot: A.TypeAnnotation) =
    let mutable annot = annot
    if annot.JavaScriptTypesAndFiles |> List.contains name then
        if not annot.IsForcedNotJavaScript then 
            annot <- { annot with IsJavaScript = true }
    if annot.JavaScriptExportTypesAndFiles |> List.contains name then
        annot <- { annot with IsJavaScriptExport = true }
    annot

// fixes annotation for property setters, we don't want name coflicts
let fixMemberAnnot (getAnnot: _ -> A.MemberAnnotation) (x: FSharpEntity) (m: FSMFV) (a: A.MemberAnnotation) =
    if m.IsPropertySetterMethod then
        let po = 
            x.MembersFunctionsAndValues 
            |> Seq.tryFind (fun p -> p.IsProperty && p.HasSetterMethod && p.SetterMethod = m)
        match po with
        | Some p ->
            let pa = getAnnot p
            if pa.Kind = Some A.MemberKind.Stub then
                { a with Kind = pa.Kind; Name = pa.Name }
            else  
                let name =
                    match a.Name with
                    | Some n as an when an <> pa.Name || not p.HasGetterMethod -> Some n
                    | _ -> pa.Name |> Option.map (fun n -> if p.HasGetterMethod then "set_" + n else n)
                { a with Name = name }
        | _ -> a
    else a

let rec private collectClassAnnotations (d: Dictionary<FSharpEntity, TypeDefinition * A.TypeAnnotation>) (sr: CodeReader.SymbolReader) parentAnnot (cls: FSharpEntity) members =
    let thisDef = sr.ReadTypeDefinition cls
    let annot =
        sr.AttributeReader.GetTypeAnnot(parentAnnot |> annotForTypeOrFile thisDef.Value.FullName, cls.Attributes)
    d.Add(cls, (thisDef, annot))
    for m in members do
        match m with
        | SourceEntity (ent, nmembers) ->
            collectClassAnnotations d sr annot ent nmembers
        | _ -> ()

let private transformInterface (sr: CodeReader.SymbolReader) parentAnnot (intf: FSharpEntity) =
    let annot =
       sr.AttributeReader.GetTypeAnnot(parentAnnot, intf.Attributes)
    if annot.IsForcedNotJavaScript then None else
    let methodNames = ResizeArray()
    let def =
        match annot.ProxyOf with
        | Some d -> d
        | _ -> sr.ReadTypeDefinition intf

    for m in intf.MembersFunctionsAndValues do
        if not m.IsProperty then
            let mAnnot =
                sr.AttributeReader.GetMemberAnnot(annot, m.Attributes)
                |> fixMemberAnnot (fun a -> sr.AttributeReader.GetMemberAnnot(annot, a.Attributes)) intf m
            let md = 
                match sr.ReadMember m with
                | Member.Method (_, md) -> md
                | _ -> failwith "invalid interface member"
            methodNames.Add(md, mAnnot.Name)
    Some (def, 
        {
            StrongName = annot.Name 
            Extends = intf.DeclaredInterfaces |> Seq.map (fun i -> sr.ReadTypeDefinition i.TypeDefinition) |> List.ofSeq
            NotResolvedMethods = List.ofSeq methodNames 
        }
    )

let private isILClass (e: FSharpEntity) =
    e.IsClass || e.IsFSharpExceptionDeclaration || e.IsFSharpModule || e.IsFSharpRecord || e.IsFSharpUnion || e.IsValueType

let private isResourceType (sr: CodeReader.SymbolReader) (e: FSharpEntity) =
    e.AllInterfaces |> Seq.exists (fun i ->
        sr.ReadTypeDefinition i.TypeDefinition = Definitions.IResource
    )

let isAugmentedFSharpType (e: FSharpEntity) =
    e.IsFSharpRecord || e.IsFSharpExceptionDeclaration || (
        e.IsFSharpUnion 
        && not (
            e.Attributes |> Seq.exists (fun a ->
                a.AttributeType.FullName = "Microsoft.FSharp.Core.DefaultAugmentationAttribute"
                && not (snd a.ConstructorArguments.[0] :?> bool)
            )
        )
    )

let isAbstractClass (e: FSharpEntity) =
    e.Attributes |> Seq.exists (fun a ->
        a.AttributeType.FullName = "Microsoft.FSharp.Core.AbstractClassAttribute"
    )

let private transformInitAction (sc: Lazy<_ * StartupCode>) (comp: Compilation) (sr: CodeReader.SymbolReader) (annot: A.TypeAnnotation) a =
    if annot.IsJavaScript then
        let _, (statements, _) = sc.Value
        let env = CodeReader.Environment.New ([], [], comp, sr)  
        statements.Add (CodeReader.transformExpression env a |> ExprStatement)   

let rec private transformClass (sc: Lazy<_ * StartupCode>) (comp: Compilation) (ac: ArgCurrying.ResolveFuncArgs) (sr: CodeReader.SymbolReader) (classAnnots: Dictionary<FSharpEntity, TypeDefinition * A.TypeAnnotation>) (cls: FSharpEntity) (members: ResizeArray<SourceMemberOrEntity>) =
    let thisDef, annot = classAnnots.[cls]

    if isResourceType sr cls then
        if comp.HasGraph then
            let thisRes = comp.Graph.AddOrLookupNode(ResourceNode (thisDef, None))
            for req, po in annot.Requires do
                comp.Graph.AddEdge(thisRes, ResourceNode (req, po |> Option.map ParameterObject.OfObj))
        None
    else    
    
    let def, proxied =
        match annot.ProxyOf with
        | Some p -> 
            let warn msg =
                comp.AddWarning(Some (CodeReader.getRange cls.DeclarationLocation), SourceWarning msg)
            if cls.Accessibility.IsPublic then
                warn "Proxy type should not be public"
            let proxied =
                try
                    let t = Reflection.LoadTypeDefinition p
                    t.GetMembers(Reflection.AllPublicMethodsFlags) |> Seq.choose Reflection.ReadMember
                    |> Seq.collect (fun m ->
                        match m with
                        | Member.Override (_, me) -> [ Member.Method (true, me); m ]
                        | _ -> [ m ]
                    ) |> HashSet
                with _ ->
                    warn ("Proxy target type could not be loaded for signature verification: " + p.Value.FullName)
                    HashSet()
            p, Some proxied
        | _ -> thisDef, None

    if annot.IsJavaScriptExport then
        comp.AddJavaScriptExport (ExportNode (TypeNode def))

    let clsMembers = ResizeArray()
    
    let getUnresolved (mAnnot: A.MemberAnnotation) kind compiled curriedArgs expr = 
        let nr =
            {
                Kind = kind
                StrongName = mAnnot.Name
                Macros = mAnnot.Macros
                Generator = 
                    match mAnnot.Kind with
                    | Some (A.MemberKind.Generated (g, p)) -> Some (g, p)
                    | _ -> None
                Compiled = compiled 
                Pure = mAnnot.Pure
                Body = expr
                Requires = mAnnot.Requires
                FuncArgs = curriedArgs |> Option.map (fun (_, ca, _, _) -> ca)
                Args = 
                    match curriedArgs with 
                    | None -> [] 
                    | Some (_, _, ids, _) -> ids
                Warn = mAnnot.Warn
            }
        match curriedArgs with
        | Some (mem, ca, args, inst) ->
            ac.AddMember(mem, nr, args, inst)
        | _ -> ()
        nr

    let addMethod (mem: option<FSMFV * Member>) (mAnnot: A.MemberAnnotation) (mdef: Method) kind compiled curriedArgs expr =
        match proxied, mem with
        | Some ms, Some (mem, memdef) ->
            if not <| ms.Contains memdef then
                let candidates =
                    let n = mdef.Value.MethodName
                    match memdef with
                    | Member.Method (i, _) ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Method (mi, m) -> if mi = i && m.Value.MethodName = n then Some (string m.Value) else None
                            | Member.Override (_, m) -> if i && m.Value.MethodName = n then Some (string m.Value) else None 
                            | _ -> None 
                        )    
                    | Member.Override _ ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Method (true, m) -> if m.Value.MethodName = n then Some (string m.Value) else None
                            | Member.Override (_, m) -> if m.Value.MethodName = n then Some (string m.Value) else None 
                            | _ -> None 
                        )    
                    | Member.Implementation (intf, _) ->
                        ms |> Seq.choose (fun m ->
                            match m with
                            | Member.Implementation (i, m) -> if i = intf && m.Value.MethodName = n then Some (i.Value.FullName + string m.Value) else None 
                            | _ -> None 
                        )  
                    | _ -> Seq.empty  
                    |> List.ofSeq
                if List.isEmpty candidates then
                    if not (mem.Accessibility.IsPrivate || mem.Accessibility.IsInternal) then
                        let msg = "Proxy member do not match any member names of target class."
                        comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
                else 
                    let msg = sprintf "Proxy member do not match any member signatures of target class %s. Current: %s, candidates: %s" (string def.Value) (string mdef.Value) (String.concat ", " candidates)
                    comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
        | _ -> ()
        if mAnnot.IsJavaScriptExport then
            comp.AddJavaScriptExport (ExportNode (MethodNode (def, mdef)))
        clsMembers.Add (NotResolvedMember.Method (mdef, (getUnresolved mAnnot kind compiled curriedArgs expr)))
        
    let addConstructor (mem: option<FSMFV * Member>) (mAnnot: A.MemberAnnotation) (cdef: Constructor) kind compiled curriedArgs expr =
        match proxied, mem with
        | Some ms, Some (mem, memdef) ->
            if cdef.Value.CtorParameters.Length > 0 && not (ms.Contains memdef) then
                let candidates = 
                    ms |> Seq.choose (function Member.Constructor c -> Some c | _ -> None)
                    |> Seq.map (fun m -> string m.Value) |> List.ofSeq
                if not (mem.Accessibility.IsPrivate || mem.Accessibility.IsInternal) then
                    let msg = sprintf "Proxy constructor do not match any constructor signatures of target class. Current: %s, candidates: %s" (string cdef.Value) (String.concat ", " candidates)
                    comp.AddWarning(Some (CodeReader.getRange mem.DeclarationLocation), SourceWarning msg)
        | _ -> ()
        if mAnnot.IsJavaScriptExport then
            comp.AddJavaScriptExport (ExportNode (ConstructorNode (def, cdef)))
        clsMembers.Add (NotResolvedMember.Constructor (cdef, (getUnresolved mAnnot kind compiled curriedArgs expr)))

    let annotations = Dictionary ()
        
    let rec getAnnot x : A.MemberAnnotation =
        let mem = sr.ReadMember x
        match annotations.TryFind mem with
        | Some a -> a
        | _ -> 
            let a = 
                sr.AttributeReader.GetMemberAnnot(annot, x.Attributes)
                |> fixMemberAnnot getAnnot cls x
            annotations.Add(mem, a)
            a

    let stubs = HashSet()
    let mutable hasStubMember = false
    let mutable hasNonStubMember = false

    for meth in cls.MembersFunctionsAndValues do
        if meth.IsProperty then () else
        let mAnnot = getAnnot meth
        
        let error m = 
            comp.AddError(Some (CodeReader.getRange meth.DeclarationLocation), SourceError m)

        match mAnnot.Kind with
        | Some A.MemberKind.Stub ->
            hasStubMember <- true
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                let expr, err = Stubs.GetMethodInline annot mAnnot (cls.IsFSharpModule && not meth.IsValCompiledAsMethod) isInstance def mdef
                err |> Option.iter error
                stubs.Add memdef |> ignore
                addMethod (Some (meth, memdef)) mAnnot mdef N.Inline true None expr
            | Member.Constructor cdef ->
                let expr = Stubs.GetConstructorInline annot mAnnot def cdef
                addConstructor (Some (meth, memdef)) mAnnot cdef N.Inline true None expr
            | Member.Implementation _ -> error "Implementation method can't have Stub attribute"
            | Member.Override _ -> error "Override method can't have Stub attribute"
            | Member.StaticConstructor -> error "Static constructor can't have Stub attribute"
        | Some A.MemberKind.JavaScript when meth.IsDispatchSlot -> 
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                if not isInstance then failwith "Abstract method should not be static" 
                addMethod (Some (meth, memdef)) mAnnot mdef N.Abstract true None Undefined
            | _ -> failwith "Member kind not expected for astract method"
        | Some (A.MemberKind.Remote rp) ->
            let memdef = sr.ReadMember meth
            match memdef with
            | Member.Method (isInstance, mdef) ->
                let remotingKind =
                    match mdef.Value.ReturnType with
                    | VoidType -> RemoteSend
                    | ConcreteType { Entity = e } when e = Definitions.Async -> RemoteAsync
                    | ConcreteType { Entity = e } when e = Definitions.Task || e = Definitions.Task1 -> RemoteTask
                    | _ -> RemoteSync
                let handle = 
                    comp.GetRemoteHandle(
                        def.Value.FullName + "." + mdef.Value.MethodName,
                        mdef.Value.Parameters,
                        mdef.Value.ReturnType
                    )
                addMethod (Some (meth, memdef)) mAnnot mdef (N.Remote(remotingKind, handle, rp)) true None Undefined
            | _ -> error "Only methods can be defined Remote"
        | _ -> ()

    let fsharpSpecificNonException =
        cls.IsFSharpUnion || cls.IsFSharpRecord || cls.IsValueType

    let fsharpSpecific = 
        fsharpSpecificNonException || cls.IsFSharpExceptionDeclaration

    let fsharpModule = cls.IsFSharpModule

    let clsTparams =
        lazy 
        cls.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq

    let inlinesOfClass =
        members |> Seq.choose (fun m ->
            match m with 
            | SourceMember (mem, _, _) ->
                let memdef = sr.ReadMember mem
                match memdef with
                | Member.Method (_, meth) -> 
                    let mAnnot = getAnnot mem    
                    match mAnnot.Kind with
                    | Some A.MemberKind.JavaScript -> None
                    | Some _ -> Some meth
                    | _ -> None
                | _ -> None
            | _ -> None   
        )
        |> HashSet

    let baseCls =
        if fsharpSpecificNonException || fsharpModule || cls.IsValueType || annot.IsStub || def.Value.FullName = "System.Object" then
            None
        elif annot.Prototype = Some false then
            cls.BaseType |> Option.bind (fun t -> t.TypeDefinition |> sr.ReadTypeDefinition |> ignoreSystemObject)
        else 
            cls.BaseType |> Option.map (fun t -> t.TypeDefinition |> sr.ReadTypeDefinition)

    for i = 0 to members.Count - 1 do
        let m = members.[i]
        match m with
        | SourceMember (meth, args, expr) ->        
            if meth.IsProperty || (fsharpSpecific && meth.IsCompilerGenerated) then () else

            let mAnnot, isModulePattern = 
                if fsharpModule && meth.IsCompilerGenerated && i < members.Count then
                    // in module compiler-generated members created for pattern matching on let, look for the next member
                    match members.[i + 1] with
                    | SourceMember (nmeth, _, _) ->  
                        getAnnot nmeth, true        
                    | _ -> getAnnot meth, false
                else
                    getAnnot meth, false
            
            let getArgsAndThis() =
                let a, t =
                    args |> List.concat
                    |> function
                    | t :: r when (t.IsMemberThisValue || t.IsConstructorThisValue) && not meth.IsExtensionMember -> r, Some t
                    | a -> a, None

                a
                |> function 
                | [ u ] when CodeReader.isUnit u.FullType -> []
                | a -> a
                , t

            let getVarsAndThis() =
                let a, t = getArgsAndThis()
                a |> List.map (fun p -> CodeReader.namedId p),
                t |> Option.map (fun p -> CodeReader.namedId p)
               
            let error m = comp.AddError(Some (CodeReader.getRange meth.DeclarationLocation), SourceError m)
            let warn m = comp.AddWarning(Some (CodeReader.getRange meth.DeclarationLocation), SourceWarning m)

            match mAnnot.Kind with
            | Some A.MemberKind.Stub
            | Some (A.MemberKind.Remote _) -> ()
            | Some kind ->
                hasNonStubMember <- true
                let memdef = sr.ReadMember meth

                if stubs.Contains memdef then () else
                let kind =
                    // for Proxy projects only, handle F# inline as WS Inline
                    if Option.isSome comp.ProxyTargetName && kind = A.MemberKind.JavaScript then
                        match meth.InlineAnnotation with
                        | FSharpInlineAnnotation.AggressiveInline
                        | FSharpInlineAnnotation.AlwaysInline
                        | FSharpInlineAnnotation.PseudoValue ->
                            A.MemberKind.InlineJavaScript
                        | _ -> kind
                    else kind
                let getBody isInline = 
                    let noCurriedOpt =
                        match memdef with
                        | Member.Method _ 
                        | Member.Constructor _ -> false
                        | _ -> true
                    try
                        let fromRD = 
                            let hasRD =
                                meth.Attributes |> Seq.exists (fun a -> a.AttributeType.FullName = "Microsoft.FSharp.Core.ReflectedDefinitionAttribute")
                            if hasRD then
                                let info =
                                    match memdef with
                                    | Member.Method (_, mdef) 
                                    | Member.Override (_, mdef) 
                                    | Member.Implementation (_, mdef) ->
                                        Reflection.LoadMethod def mdef :> System.Reflection.MethodBase
                                    | Member.Constructor cdef ->
                                        Reflection.LoadConstructor def cdef :> _
                                    | Member.StaticConstructor ->
                                        (Reflection.LoadTypeDefinition def).GetConstructors(System.Reflection.BindingFlags.Static).[0] :> _
                                WebSharper.Compiler.ReflectedDefinitionReader.readReflected comp info 
                            else None
                        match fromRD with
                        | Some rd ->
                            // TODO: curried argument optimization for ReflectedDefinition
                            None, FixThisScope().Fix(rd)
                        | _ ->
                        let a, t = getArgsAndThis()
                        let argsAndVars = 
                            [
                                match t with
                                | Some t ->
                                    yield t, (CodeReader.namedId t, CodeReader.ThisArg)
                                | _ -> ()
                                for p in a ->    
                                    p, 
                                    (CodeReader.namedId p, 
                                        if CodeReader.isByRef p.FullType 
                                        then CodeReader.ByRefArg 
                                        else
                                            if noCurriedOpt then CodeReader.LocalVar
                                            else
                                                match CodeReader.getFuncArg p.FullType with
                                                | NotOptimizedFuncArg -> CodeReader.LocalVar
                                                | _ -> CodeReader.FuncArg
                                    )
                            ]
                        // search for curried function arguments and register them
                        let curriedArgs =
                            if noCurriedOpt then None else
                            let mem =
                                match memdef with
                                | Member.Method (_, mdef) -> Some (ArgCurrying.Member.Method(def, mdef))
                                | Member.Constructor cdef -> Some (ArgCurrying.Member.Constructor(def, cdef))
                                | _ -> None
                            match mem with
                            | None -> None
                            | Some mem ->
                            let ca = 
                                a |> List.map (fun p -> 
                                    CodeReader.getFuncArg p.FullType
                                )
                            if ca |> List.forall ((=) NotOptimizedFuncArg) then None 
                            else 
                                let args =
                                    argsAndVars |> List.map (snd >> fst)
                                    |> if Option.isSome t then List.skip 1 else id    
                                Some (mem, ca, args, Option.isSome t)
                        
                        let tparams = meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq 
                        let env = CodeReader.Environment.New (argsAndVars, tparams, comp, sr)  
                        let res =
                            let b = CodeReader.transformExpression env expr 
                            let b = 
                                if meth.IsConstructor then
                                    try CodeReader.fixCtor def baseCls b
                                    with e ->
                                        let tryGetExprSourcePos expr =
                                            match expr with
                                            | ExprSourcePos (p, _) -> Some p
                                            | _ -> None
                                        comp.AddError(tryGetExprSourcePos b, SourceError e.Message)
                                        errorPlaceholder
                                else b
                            let b = FixThisScope().Fix(b)      
                            if List.isEmpty args && meth.IsModuleValueOrMember then 
                                if isModulePattern then
                                    let scDef, (scContent, scFields) = sc.Value   
                                    let var = Id.New(mut = false)
                                    scContent.Add (VarDeclaration (var, TailCalls.optimize None inlinesOfClass b))
                                    Var var
                                elif isInline then
                                    b
                                else
                                    let scDef, (scContent, scFields) = sc.Value   
                                    let name = Resolve.getRenamed meth.CompiledName scFields
                                    scContent.Add (ExprStatement (ItemSet(Self, Value (String name), TailCalls.optimize None inlinesOfClass b)))
                                    Lambda([], FieldGet(None, NonGeneric scDef, name))
                            else
                                let thisVar, vars =
                                    match argsAndVars with 
                                    | (_, (t, CodeReader.ThisArg)) :: a -> Some t, a |> List.map (snd >> fst)  
                                    | a -> None, a |> List.map (snd >> fst) 
                                let defValues = 
                                    Seq.zip (Seq.concat meth.CurriedParameterGroups) vars |> Seq.choose (fun (p, i) ->
                                        if p.Attributes |> Seq.exists (fun pa -> pa.AttributeType.FullName = "System.Runtime.InteropServices.OptionalAttribute") then
                                            let d =
                                                p.Attributes |> Seq.tryPick (fun pa -> 
                                                    if pa.AttributeType.FullName = "System.Runtime.InteropServices.DefaultParameterValueAttribute" then
                                                        Some (ReadLiteral (snd pa.ConstructorArguments.[0]) |> Value) 
                                                    else None
                                                )
                                            Some (i,
                                                match d with
                                                | Some v -> v
                                                | _ -> DefaultValueOf (sr.ReadType env.TParams p.Type)
                                            )
                                        else None
                                    )
                                    |> List.ofSeq
                                let b =
                                    if List.isEmpty defValues then b else
                                        Sequential [
                                            for i, v in defValues -> VarSet(i, Conditional (Var i ^== Undefined, v, Var i))
                                            yield b
                                        ]
                                if isInline then
                                    let b = 
                                        match thisVar with
                                        | Some t -> ReplaceThisWithVar(t).TransformExpression(b)
                                        | _ -> b
                                    makeExprInline (Option.toList thisVar @ vars) b
                                else 
                                    let returnsUnit =
                                        match memdef with
                                        | Member.Method (_, mdef)  
                                        | Member.Override (_, mdef) 
                                        | Member.Implementation (_, mdef) ->
                                            mdef.Value.ReturnType = VoidType
                                        | _ -> true
                                    if returnsUnit then
                                        Function(vars, ExprStatement b)
                                    else
                                        Lambda(vars, b)
                        let currentMethod =
                            match memdef with
                            | Member.Method (_, m) -> 
                                Some (def, m) 
                            | _ -> None
                        curriedArgs, TailCalls.optimize currentMethod inlinesOfClass res
                    with e ->
                        error (sprintf "Error reading definition: %s at %s" e.Message e.StackTrace)
                        None, errorPlaceholder

                match memdef with
                | Member.Method (_, mdef) 
                | Member.Override (_, mdef) 
                | Member.Implementation (_, mdef) ->
                    let getKind() =
                        match memdef with
                        | Member.Method (isInstance , _) ->
                            if isInstance then N.Instance else N.Static  
                        | Member.Override (t, _) -> N.Override t 
                        | Member.Implementation (t, _) -> N.Implementation t
                        | _ -> failwith "impossible"
                    
                    let addModuleValueProp kind body =
                        if List.isEmpty args && fsharpModule then
                            let iBody = Call(None, NonGeneric def, Generic mdef (List.init mdef.Value.Generics TypeParameter), [])
                            // TODO : check proxy targets for module values
                            addMethod None mAnnot (Method { mdef.Value with MethodName = "get_" + mdef.Value.MethodName }) N.Inline false None iBody    
                            if meth.IsMutable then 
                                let setm = 
                                    let me = mdef.Value
                                    Method {
                                        MethodName = "set_" + me.MethodName   
                                        Parameters = [ me.ReturnType ]
                                        ReturnType = VoidType
                                        Generics = 0
                                    }
                                let setb =
                                    match body with
                                    | Function([], Return (FieldGet(None, {Entity = scDef; Generics = []}, name))) ->
                                        let value = CodeReader.newId()                          
                                        Function ([value], (ExprStatement <| FieldSet(None, NonGeneric scDef, name, Var value)))
                                    | _ -> 
                                        error "unexpected form in module let body"
                                        Undefined
                                        //failwith "unexpected form in module let body"
                                addMethod None { mAnnot with Name = None } setm kind false None setb    
                            true
                        else false

                    let addM = addMethod (Some (meth, memdef)) mAnnot mdef

                    let jsMethod isInline =
                        let kind = if isInline || isModulePattern then N.Inline else getKind()
                        let ca, body = getBody isInline        
                        if isModulePattern || addModuleValueProp kind body then
                            addMethod None mAnnot mdef kind false ca body  
                        else addM kind false ca body

                    let checkNotAbstract() =
                        if meth.IsDispatchSlot then
                            error "Abstract methods cannot be marked with Inline, Macro or Constant attributes."
                        else
                            match memdef with
                            | Member.Override (bTyp, _) -> 
                                if not (bTyp = Definitions.Obj || bTyp = Definitions.ValueType) then
                                    error "Override methods cannot be marked with Inline, Macro or Constant attributes."
                            | Member.Implementation _ ->
                                error "Interface implementation methods cannot be marked with Inline, Macro or Constant attributes."
                            | _ -> ()
                    match kind with
                    | A.MemberKind.NoFallback ->
                        checkNotAbstract()
                        addM N.NoFallback true None Undefined
                    | A.MemberKind.Inline (js, dollarVars) ->
                        checkNotAbstract() 
                        let vars, thisVar = getVarsAndThis()
                        try 
                            let parsed = WebSharper.Compiler.Recognize.createInline comp.MutableExternals thisVar vars mAnnot.Pure dollarVars js
                            List.iter warn parsed.Warnings
                            if addModuleValueProp N.Inline parsed.Expr then
                                addMethod None mAnnot mdef N.Inline true None parsed.Expr
                            else addM N.Inline true None parsed.Expr
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message)
                    | A.MemberKind.Constant c ->
                        checkNotAbstract() 
                        addM N.Inline true None (Value c)                        
                    | A.MemberKind.Direct (js, dollarVars) ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect comp.MutableExternals thisVar vars dollarVars js
                            List.iter warn parsed.Warnings
                            addM (getKind()) true None parsed.Expr
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message)
                    | A.MemberKind.JavaScript ->
                        jsMethod false
                    | A.MemberKind.InlineJavaScript ->
                        checkNotAbstract()
                        jsMethod true
                    | A.MemberKind.OptionalField ->
                        if meth.IsPropertyGetterMethod then
                            let i = JSRuntime.GetOptional (ItemGet(Hole 0, Value (String meth.CompiledName.[4..]), Pure))
                            addM N.Inline true None i
                        elif meth.IsPropertySetterMethod then  
                            let i = JSRuntime.SetOptional (Hole 0) (Value (String meth.CompiledName.[4..])) (Hole 1)
                            addM N.Inline true None i
                        else error "OptionalField attribute not on property"
                    | A.MemberKind.Generated _ ->
                        addM (getKind()) false None Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                    | A.MemberKind.Remote _ 
                    | A.MemberKind.Stub -> failwith "should be handled previously"
                    if mAnnot.IsEntryPoint then
                        match memdef with
                        | Member.Method (false, mdef) when List.isEmpty mdef.Value.Parameters ->
                            let ep = ExprStatement <| Call(None, NonGeneric def, NonGeneric mdef, [])
                            if comp.HasGraph then
                                comp.Graph.AddEdge(EntryPointNode, MethodNode (def, mdef))
                            comp.SetEntryPoint(ep)
                        | _ ->
                            error "The SPAEntryPoint must be a static method with no arguments"
                | Member.Constructor cdef ->
                    let addC = addConstructor (Some (meth, memdef)) mAnnot cdef
                    let jsCtor isInline =   
                        if isInline then 
                            addC N.Inline false <|| getBody true
                        else
                            addC N.Constructor false <|| getBody false
                    match kind with
                    | A.MemberKind.NoFallback ->
                        addC N.NoFallback true None Undefined
                    | A.MemberKind.Inline (js, dollarVars) ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.createInline comp.MutableExternals thisVar vars mAnnot.Pure dollarVars js
                            List.iter warn parsed.Warnings
                            addC N.Inline true None parsed.Expr
                        with e ->
                            error ("Error parsing inline JavaScript: " + e.Message)
                    | A.MemberKind.Direct (js, dollarVars) ->
                        let vars, thisVar = getVarsAndThis()
                        try
                            let parsed = WebSharper.Compiler.Recognize.parseDirect comp.MutableExternals thisVar vars dollarVars js
                            List.iter warn parsed.Warnings
                            addC N.Static true None parsed.Expr
                        with e ->
                            error ("Error parsing direct JavaScript: " + e.Message)
                    | A.MemberKind.JavaScript -> jsCtor false
                    | A.MemberKind.InlineJavaScript -> jsCtor true
                    | A.MemberKind.Generated _ ->
                        addC N.Static false None Undefined
                    | A.MemberKind.AttributeConflict m -> error m
                    | A.MemberKind.Remote _
                    | A.MemberKind.Stub -> failwith "should be handled previously"
                    | A.MemberKind.OptionalField
                    | A.MemberKind.Constant _ -> failwith "attribute not allowed on constructors"
                | Member.StaticConstructor ->
                    clsMembers.Add (NotResolvedMember.StaticConstructor (snd (getBody false)))
            | None -> ()
            let jsArgs =
                meth.CurriedParameterGroups
                |> Seq.concat
                |> Seq.mapi (fun i p -> i, sr.AttributeReader.GetParamAnnot(p.Attributes).ClientAccess)
                |> Seq.choose (fun (i, x) -> if x then Some i else None)
                |> Array.ofSeq
            if not (Array.isEmpty jsArgs) then
                match sr.ReadMember meth with
                | Member.Method (_, mdef) -> comp.AddQuotedArgMethod(thisDef, mdef, jsArgs)
                | _ -> error "JavaScript attribute on parameter is only allowed on methods"
            let tparams = meth.GenericParameters |> Seq.map (fun p -> p.Name) |> List.ofSeq 
            let env = CodeReader.Environment.New ([], tparams, comp, sr)
            CodeReader.scanExpression env meth.LogicalName expr
            |> Seq.iter (fun (pos, mdef, argNames, e) ->
                addMethod None A.MemberAnnotation.BasicJavaScript mdef (N.Quotation(pos, argNames)) false None e 
            )
        | SourceEntity (ent, nmembers) ->
            transformClass sc comp ac sr classAnnots ent nmembers |> Option.iter comp.AddClass   
        | SourceInterface i ->
            transformInterface sr annot i |> Option.iter comp.AddInterface
        | InitAction expr ->
            transformInitAction sc comp sr annot expr    

    if not annot.IsJavaScript && clsMembers.Count = 0 && annot.Macros.IsEmpty then None else

    let ckind = 
        if annot.IsStub || (hasStubMember && not hasNonStubMember)
        then NotResolvedClassKind.Stub
        elif fsharpModule then NotResolvedClassKind.Static
        elif (annot.IsJavaScript && (isAbstractClass cls || cls.IsFSharpExceptionDeclaration)) || (annot.Prototype = Some true)
        then NotResolvedClassKind.WithPrototype
        else NotResolvedClassKind.Class

    let hasWSPrototype =                
        hasWSPrototype ckind baseCls clsMembers

    let mutable hasSingletonCase = false
    let mutable hasConstantCase = false

    let notForcedNotJavaScript = not annot.IsForcedNotJavaScript

    if annot.IsJavaScript || hasWSPrototype || isAugmentedFSharpType cls then
        if cls.IsFSharpUnion then
            let usesNull =
                cls.UnionCases.Count < 4 // see TaggingThresholdFixedConstant in visualfsharp/src/ilx/EraseUnions.fs
                && cls.Attributes |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.UseNullAsTrueValue
                && cls.UnionCases |> Seq.exists (fun c -> c.Fields.Count = 0)

            let mutable nullCase = usesNull 

            let constants = HashSet() 

            let cases =
                cls.UnionCases
                |> Seq.mapi (fun i case ->
                    let constantCase v =
                        hasConstantCase <- true
                        if constants.Add(v) then
                            ConstantFSharpUnionCase v
                        else
                            comp.AddError(Some (CodeReader.getRange case.DeclarationLocation), 
                                SourceError "Union case translated constant value is a duplicate")
                            ConstantFSharpUnionCase (String "$$ERROR$$")
                    let cAnnot = sr.AttributeReader.GetMemberAnnot(annot, case.Attributes)
                    let kind =
                        let argumentless = case.Fields.Count = 0
                        if nullCase && argumentless then
                            nullCase <- false
                            constantCase Null
                        else
                        match cAnnot.Kind with
                        | Some (A.MemberKind.Constant v) -> 
                            constantCase v
                        | _ ->
                            if argumentless && notForcedNotJavaScript then
                                let caseField = Definitions.SingletonUnionCase case.CompiledName
                                let expr = CopyCtor(def, Object [ "$", Value (Int i) ])
                                let a = { A.MemberAnnotation.BasicPureJavaScript with Name = Some case.Name }
                                clsMembers.Add (NotResolvedMember.Method (caseField, (getUnresolved a N.Static false None expr)))
                                hasSingletonCase <- true
                                SingletonFSharpUnionCase
                            else
                                NormalFSharpUnionCase (
                                    case.Fields
                                    |> Seq.map (fun f ->
                                        {
                                            Name = f.Name
                                            UnionFieldType = sr.ReadType clsTparams.Value f.FieldType
                                            DateTimeFormat = 
                                                cAnnot.DateTimeFormat 
                                                |> List.tryPick (fun (target, format) -> if target = Some f.Name then Some format else None)
                                        }
                                    )
                                    |> List.ofSeq
                                )
                    let staticIs =
                        not usesNull || not (
                            case.Attributes
                            |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.Instance
                        )
                    {
                        Name = case.Name
                        JsonName = cAnnot.Name
                        Kind = kind
                        StaticIs = staticIs
                    }
                )
                |> List.ofSeq
            
            let i =
                FSharpUnionInfo {
                    Cases = cases
                    NamedUnionCases = annot.NamedUnionCases
                    HasNull = constants.Contains(Null)
                }

            comp.AddCustomType(def, i, not annot.IsJavaScript)

        if (cls.IsFSharpRecord || cls.IsFSharpExceptionDeclaration) then
            let cdef =
                Hashed {
                    CtorParameters =
                        cls.FSharpFields |> Seq.map (fun f -> sr.ReadType clsTparams.Value f.FieldType) |> List.ofSeq
                }
            let body =
                let vars =
                    cls.FSharpFields |> Seq.map (fun f -> Id.New(f.Name, mut = false)) |> List.ofSeq
                let fields =
                    cls.FSharpFields |> Seq.map (fun f -> 
                        let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    
                        match fAnnot.Name with Some n -> n | _ -> f.Name
                        , 
                        fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                    )
                    |> List.ofSeq
                let obj = 
                    let normalFields =
                        Seq.zip (fields) vars
                        |> Seq.choose (fun ((name, opt), v) -> if opt then None else Some (name, Var v))
                        |> List.ofSeq |> Object
                    if fields |> List.exists snd then
                        let o = CodeReader.newId()
                        Let(o, normalFields, 
                            Sequential [
                                for (name, opt), v in Seq.zip fields vars do
                                    if opt then yield JSRuntime.SetOptional (Var o) (Value (String name)) (Var v)
                                yield Var o
                            ]
                        )
                    else 
                        normalFields
                Lambda (vars, CopyCtor(def, obj))

            let cAnnot =
                if notForcedNotJavaScript then 
                    A.MemberAnnotation.BasicPureJavaScript
                else A.MemberAnnotation.BasicPureInlineJavaScript
            addConstructor None cAnnot cdef N.Static false None body

            // properties

            for f in cls.FSharpFields do
                let recTyp = Generic def (List.init cls.GenericParameters.Count TypeParameter)
                let fTyp = sr.ReadType clsTparams.Value f.FieldType
            
                let getDef =
                    Hashed {
                        MethodName = "get_" + f.Name
                        Parameters = []
                        ReturnType = fTyp
                        Generics = 0
                    }

                let getBody = FieldGet(Some (Hole 0), recTyp, f.Name)
                
                addMethod None A.MemberAnnotation.BasicPureInlineJavaScript getDef N.Inline false None getBody

                if f.IsMutable then
                    let setDef =
                        Hashed {
                            MethodName = "set_" + f.Name
                            Parameters = [ fTyp ]
                            ReturnType = VoidType
                            Generics = 0
                        }

                    let setBody = FieldSet(Some (Hole 0), recTyp, f.Name, Hole 1)
            
                    addMethod None A.MemberAnnotation.BasicInlineJavaScript setDef N.Inline false None setBody

        if cls.IsFSharpRecord then
            let i = 
                cls.FSharpFields |> Seq.map (fun f ->
                    let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    let isOpt = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                    let fTyp = sr.ReadType clsTparams.Value f.FieldType

                    {
                        Name = f.Name
                        JSName = match fAnnot.Name with Some n -> n | _ -> f.Name // TODO : set in resolver instead
                        RecordFieldType = fTyp
                        DateTimeFormat = fAnnot.DateTimeFormat |> List.tryHead |> Option.map snd
                        Optional = isOpt
                        IsMutable = f.IsMutable
                    }
                )
                |> List.ofSeq |> FSharpRecordInfo    

            if comp.HasCustomTypeInfo def then
                printfn "Already has custom type info: %s" def.Value.FullName
            else
                comp.AddCustomType(def, i, not annot.IsJavaScript)

        if cls.IsValueType && not (cls.IsFSharpRecord || cls.IsFSharpUnion || cls.IsEnum) then
            // add default constructor for structs
            let cdef = ConstructorInfo.Default()
            let fields =
                cls.FSharpFields |> Seq.map (fun f -> 
                    if f.IsMutable then
                        comp.AddError(Some (CodeReader.getRange f.DeclarationLocation), SourceError "Mutable structs are not supported for JavaScript translation")
                    let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    
                    match fAnnot.Name with Some n -> n | _ -> f.Name
                    , 
                    DefaultValueOf (sr.ReadType clsTparams.Value f.FieldType)
                )
                |> List.ofSeq
            let body = Lambda([], Sequential (fields |> List.map (fun (n, v) -> ItemSet(This, Value (String n), v))))
            addConstructor None A.MemberAnnotation.BasicPureJavaScript cdef N.Constructor false None body
            comp.AddCustomType(def, StructInfo, not annot.IsJavaScript)

    for f in cls.FSharpFields do
        let fAnnot = sr.AttributeReader.GetMemberAnnot(annot, Seq.append f.FieldAttributes f.PropertyAttributes)
        let nr =
            {
                StrongName = fAnnot.Name
                IsStatic = f.IsStatic
                IsOptional = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                IsReadonly = not f.IsMutable
                FieldType = sr.ReadType clsTparams.Value f.FieldType
            }
        clsMembers.Add (NotResolvedMember.Field (f.Name, nr))

    let strongName =
        annot.Name |> Option.map (fun n ->
            if n.StartsWith "." then n.TrimStart('.') else
            if n.Contains "." then n else 
                let origName = thisDef.Value.FullName
                origName.[.. origName.LastIndexOf '.'] + n
        )   

    Some (
        def,
        {
            StrongName = strongName
            BaseClass = baseCls
            Requires = annot.Requires
            Members = List.ofSeq clsMembers
            Kind = ckind
            IsProxy = Option.isSome annot.ProxyOf
            Macros = annot.Macros
            ForceNoPrototype = (annot.Prototype = Some false) || hasConstantCase
            ForceAddress = hasSingletonCase || def.Value.FullName = "System.Exception" // needed for Error inheritance
        }
    )

let transformAssembly (logger: LoggerBase) (comp : Compilation) assemblyName (config: WsConfig) (checkResults: FSharpCheckProjectResults) =   
    comp.AssemblyName <- assemblyName
    comp.ProxyTargetName <- config.ProxyTargetName
    let sr = CodeReader.SymbolReader(comp)    
    
    let mutable asmAnnot =
        sr.AttributeReader.GetAssemblyAnnot(checkResults.AssemblySignature.Attributes)

    match config.JavaScriptScope with
    | JSDefault -> ()
    | JSAssembly -> asmAnnot <- { asmAnnot with IsJavaScript = true }
    | JSFilesOrTypes a -> asmAnnot <- { asmAnnot with JavaScriptTypesAndFiles = List.ofArray a @ asmAnnot.JavaScriptTypesAndFiles }

    match config.ProjectType with
    | Some Proxy -> asmAnnot <- { asmAnnot with IsJavaScript = true }
    | _ ->  ()

    for jsExport in config.JavaScriptExport do
        comp.AddJavaScriptExport jsExport
        match jsExport with
        | ExportCurrentAssembly -> asmAnnot <- { asmAnnot with IsJavaScript = true }
        | ExportByName n -> asmAnnot <- { asmAnnot with JavaScriptTypesAndFiles = n :: asmAnnot.JavaScriptTypesAndFiles }
        | _ -> ()

    let rootTypeAnnot = asmAnnot.RootTypeAnnot

    comp.AssemblyRequires <- asmAnnot.Requires
    comp.SiteletDefinition <- asmAnnot.SiteletDefinition

    if asmAnnot.IsJavaScriptExport then
        comp.AddJavaScriptExport ExportCurrentAssembly
    for s in asmAnnot.JavaScriptExportTypesFilesAndAssemblies do
        comp.AddJavaScriptExport (ExportByName s)

    let lookupAssembly =
        lazy
        Map [
            yield assemblyName, checkResults.AssemblySignature
            for r in checkResults.ProjectContext.GetReferencedAssemblies() do
                yield r.SimpleName, r.Contents 
        ]

    let lookupTypeDefinition (typ: TypeDefinition) =
        let t = typ.Value
        let path = t.FullName.Split('+')
        let mutable res =
            lookupAssembly.Value.[t.Assembly].Entities |> Seq.tryFind (fun e ->
                match e.TryFullName with
                | Some fn when fn = path.[0] -> true
                | _ -> false
            )
        for i = 1 to path.Length - 1 do
            if res.IsSome then
                res <- res.Value.NestedEntities |> Seq.tryFind (fun e -> e.CompiledName = path.[i])
        res 

    let readAttribute (a: FSharpAttribute) =
        try
            let fixTypeValue (o: obj) =
                match o with
                | :? FSharpType as t -> box (sr.ReadType Map.empty t)
                | _ -> o
            sr.ReadTypeDefinition a.AttributeType,
            a.ConstructorArguments |> Seq.map (snd >> fixTypeValue >> ParameterObject.OfObj) |> Array.ofSeq
        with _ ->
            failwithf "Failed to read custom attribute from F#: %s" a.AttributeType.FullName

    let readAttributes (a: seq<FSharpAttribute>) =
        a |> Seq.map readAttribute |> List.ofSeq

    let lookupTypeAttributes (typ: TypeDefinition) =
        lookupTypeDefinition typ |> Option.map (fun e ->
            e.Attributes |> readAttributes
        )

    let lookupFieldAttributes (typ: TypeDefinition) (field: string) =
        lookupTypeDefinition typ |> Option.bind (fun e -> 
            e.FSharpFields |> Seq.tryFind (fun f -> f.Name = field) |> Option.map (fun f ->
                Seq.append f.FieldAttributes f.PropertyAttributes |> readAttributes
            ) 
        )

    let lookupMethodAttributes (typ: TypeDefinition) (meth: Method) =
        lookupTypeDefinition typ |> Option.bind (fun e -> 
            e.MembersFunctionsAndValues
            |> Seq.tryFind (fun m -> 
                match sr.ReadMember m with
                | Member.Method (_, m)
                | Member.Override (_, m)
                | Member.Implementation (_, m) when m = meth -> true
                | _ -> false
            ) 
            |> Option.map (fun m ->
                m.Attributes |> readAttributes
            ) 
            |> Option.orElseWith (fun () ->
                if e.IsFSharpUnion then 
                    let mn = meth.Value.MethodName
                    let caseName =
                        if mn.StartsWith "New" then Some (mn.Substring(3))
                        elif mn.StartsWith "get_" then Some (mn.Substring(4))
                        else None
                    caseName |> Option.bind (fun cn ->
                        let case = e.UnionCases |> Seq.tryFind (fun c -> c.CompiledName = cn)
                        case |> Option.map (fun c ->
                            c.Attributes |> readAttributes
                        )
                    )
                elif e.IsFSharpRecord then
                    let mn = meth.Value.MethodName
                    let fieldName = 
                        if mn.StartsWith "get_" then Some (mn.Substring(4)) else None
                    fieldName |> Option.bind (fun fn ->
                        let field = e.FSharpFields |> Seq.tryFind (fun f -> f.Name = fn)
                        field |> Option.map (fun f ->
                            f.PropertyAttributes |> readAttributes
                        )
                    )
                else None
            )
        ) 

    let lookupConstructorAttributes (typ: TypeDefinition) (ctor: Constructor) =
        lookupTypeDefinition typ |> Option.bind (fun e -> 
            e.MembersFunctionsAndValues
            |> Seq.tryFind (fun m -> 
                match sr.ReadMember m with
                | Member.Constructor c when c = ctor -> true
                | _ -> false
            ) 
            |> Option.map (fun m ->
                m.Attributes |> readAttributes
            ) 
        )

    let reflectCustomType (typeDef: TypeDefinition): CustomTypeInfo =
        let cls = lookupTypeDefinition(typeDef)
        let branchOnType (entity: FSharpEntity) =
            let clsTparams =
                lazy 
                entity.GenericParameters |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
            if entity.IsDelegate then
                let tparams = 
                    entity.GenericParameters
                    |> Seq.mapi (fun i p -> p.Name, i) |> Map.ofSeq
                let inv = entity.MembersFunctionsAndValues |> Seq.find(fun m -> m.CompiledName = "Invoke")
                DelegateInfo {
                    DelegateArgs =
                        inv.CurriedParameterGroups |> Seq.concat |> Seq.map (fun p -> sr.ReadType tparams p.Type) |> List.ofSeq
                    ReturnType = sr.ReadType tparams inv.ReturnParameter.Type
                }
            else if entity.IsEnum then
                CustomTypeInfo.EnumInfo typeDef
            else if entity.IsFSharpRecord then
                entity.FSharpFields |> Seq.map (fun f ->
                    let fAnnot = sr.AttributeReader.GetMemberAnnot(rootTypeAnnot, Seq.append f.FieldAttributes f.PropertyAttributes)
                    let isOpt = fAnnot.Kind = Some A.MemberKind.OptionalField && CodeReader.isOption f.FieldType
                    let fTyp = sr.ReadType clsTparams.Value f.FieldType
                    {
                        Name = f.Name
                        JSName = match fAnnot.Name with Some n -> n | _ -> f.Name
                        RecordFieldType = fTyp
                        DateTimeFormat = fAnnot.DateTimeFormat |> List.tryHead |> Option.map snd
                        Optional = isOpt
                        IsMutable = f.IsMutable
                    }
                )
                |> List.ofSeq |> FSharpRecordInfo    
            else if entity.IsFSharpUnion then
                let tAnnot = rootTypeAnnot
                let usesNull =
                    entity.UnionCases.Count < 4 // see TaggingThresholdFixedConstant in visualfsharp/src/ilx/EraseUnions.fs
                    && entity.Attributes |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.UseNullAsTrueValue
                    && entity.UnionCases |> Seq.exists (fun c -> c.Fields.Count = 0)

                let mutable nullCase = usesNull 

                let constants = HashSet() 

                let cases =
                    entity.UnionCases
                    |> Seq.map (fun case ->
                        let constantCase v =
                            if constants.Add(v) then
                                ConstantFSharpUnionCase v
                            else
                                ConstantFSharpUnionCase (String "$$ERROR$$")
                        let cAnnot = sr.AttributeReader.GetMemberAnnot(tAnnot, case.Attributes)
                        let kind =
                            let argumentless = case.Fields.Count = 0
                            if nullCase && argumentless then
                                nullCase <- false
                                constantCase Null
                            else
                            match cAnnot.Kind with
                            | Some (A.MemberKind.Constant v) -> 
                                constantCase v
                            | _ ->
                                NormalFSharpUnionCase (
                                    case.Fields
                                    |> Seq.map (fun f ->
                                        {
                                            Name = f.Name
                                            UnionFieldType = sr.ReadType clsTparams.Value f.FieldType
                                            DateTimeFormat = 
                                                cAnnot.DateTimeFormat 
                                                |> List.tryPick (fun (target, format) -> if target = Some f.Name then Some format else None)
                                        }
                                    )
                                    |> List.ofSeq
                                )
                        let staticIs =
                            not usesNull || not (
                                case.Attributes
                                |> CodeReader.hasCompilationRepresentation CompilationRepresentationFlags.Instance
                            )
                        {
                            Name = case.Name
                            JsonName = cAnnot.Name
                            Kind = kind
                            StaticIs = staticIs
                        }
                    )
                    |> List.ofSeq
                
                FSharpUnionInfo {
                    Cases = cases
                    NamedUnionCases = rootTypeAnnot.NamedUnionCases
                    HasNull = constants.Contains(Null)
                }
            else
                CustomTypeInfo.NotCustomType 

        cls
        |> Option.map branchOnType
        |> Option.defaultValue CustomTypeInfo.NotCustomType

    let tryReflectCustomType typeDef =
        try
            reflectCustomType typeDef
        with
        | _ -> CustomTypeInfo.NotCustomType

    comp.CustomTypesReflector <- tryReflectCustomType
    comp.LookupTypeAttributes <- lookupTypeAttributes
    comp.LookupFieldAttributes <- lookupFieldAttributes 
    comp.LookupMethodAttributes <- lookupMethodAttributes
    comp.LookupConstructorAttributes <- lookupConstructorAttributes

    let argCurrying = ArgCurrying.ResolveFuncArgs(comp)

    checkResults.AssemblyContents.ImplementationFiles
    |> List.sortByDescending (fun f -> List.isEmpty f.Declarations)
    |> List.distinctBy (fun f -> f.FileName)
    |> Seq.iter (fun file ->
        if List.isEmpty file.Declarations then () else
        let filePath =
            match file.Declarations.Head with
            | FSIFD.Entity (a, _) -> a.DeclarationLocation.FileName
            | FSIFD.MemberOrFunctionOrValue (_, _, c) -> c.Range.FileName
            | FSIFD.InitAction a -> a.Range.FileName
        let sc =
            lazy
            let name = "StartupCode$" + assemblyName.Replace('.', '_') + "$" + (System.IO.Path.GetFileNameWithoutExtension filePath).Replace('.', '_')
            let def =
                TypeDefinition {
                    Assembly = comp.FindProxiedAssembly(assemblyName)
                    FullName = name
                }
            def, 
            (ResizeArray(), HashSet() : StartupCode)

        let rootTypeAnnot = rootTypeAnnot |> annotForTypeOrFile (System.IO.Path.GetFileName filePath)
        let topLevelTypes = ResizeArray<SourceMemberOrEntity>()
        let types = Dictionary<FSharpEntity, ResizeArray<SourceMemberOrEntity>>()
        let rec getTypesWithMembers (parentMembers: ResizeArray<_>) d =
            match d with
            | FSIFD.Entity (a, b) ->
                if not a.IsFSharpAbbreviation then
                    if a.IsInterface then
                        parentMembers.Add (SourceInterface a)
                    elif isILClass a then 
                        let ms = ResizeArray()
                        parentMembers.Add (SourceEntity (a, ms))
                        try
                            types.Add (a, ms)
                        with _ ->
                            comp.AddError(Some (CodeReader.getRange a.DeclarationLocation), SourceError "Duplicate type definition")
                        b |> List.iter (getTypesWithMembers ms)
                    else
                        b |> List.iter (getTypesWithMembers parentMembers)
            | FSIFD.MemberOrFunctionOrValue (a, b, c) -> 
                types.[CodeReader.getDeclaringEntity a].Add(SourceMember(a, b, c))
            | FSIFD.InitAction a ->
                parentMembers.Add (InitAction a)

        file.Declarations |> Seq.iter (getTypesWithMembers topLevelTypes)

        let classAnnotations = Dictionary()
        
        for t in topLevelTypes do
            match t with
            | SourceEntity (c, m) ->
                collectClassAnnotations classAnnotations sr rootTypeAnnot c m
            | _ -> ()

        // register all proxies for signature redirection
        for (def, annot) in classAnnotations.Values do
            match annot.ProxyOf with
            | Some p -> comp.AddProxy(def, p)
            | _ -> ()

        for t in topLevelTypes do
            match t with
            | SourceMember _ -> failwith "impossible: top level member"
            | InitAction _ -> failwith "impossible: top level init action"
            | SourceEntity (c, m) ->
                transformClass sc comp argCurrying sr classAnnotations c m |> Option.iter comp.AddClass
            | SourceInterface i ->
                transformInterface sr rootTypeAnnot i |> Option.iter comp.AddInterface
            
        let getStartupCodeClass (def: TypeDefinition, sc: StartupCode) =
            
            let statements, fields = sc            
            let cctor = Function ([], Block (List.ofSeq statements))
            let members =
                [
                    for f in fields -> 
                        NotResolvedMember.Field(f, 
                            {
                                StrongName = None
                                IsStatic = true
                                IsOptional = false
                                IsReadonly = true
                                FieldType = VoidType // field types are only needed for adding code dependencies for activator
                            } 
                        )
                    yield NotResolvedMember.StaticConstructor cctor
                ]
            
            def,
            {
                StrongName = None
                BaseClass = None
                Requires = [] //annot.Requires
                Members = members
                Kind = NotResolvedClassKind.Static
                IsProxy = false
                Macros = []
                ForceNoPrototype = false
                ForceAddress = false
            }
            
        if sc.IsValueCreated then
            getStartupCodeClass sc.Value |> comp.AddClass
    )
    
    logger.TimedStage "Parsing with FCS"

    argCurrying.ResolveAll()

    logger.TimedStage "Analyzing function arguments"

    comp.Resolve()

    logger.TimedStage "Resolving names"

    comp
