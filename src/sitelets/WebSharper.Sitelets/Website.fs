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

namespace WebSharper.Sitelets

open System
open System.Reflection
open System.Web

/// Define a Sitelets website. This interface must be
/// implemented by the type passed to WebsiteAttribute.
type IWebsite<'Action when 'Action : equality> =
    abstract Actions : list<'Action>
    abstract Sitelet : Sitelet<'Action>

/// The action type for SinglePageWebsite.
[<RequireQualifiedAccess>]
type SinglePageAction = | Index

module internal Specialization =
    open System

    /// Represents a generic algorithm.
    type IGeneric<'T1,'T2> =

        /// Runs the computation. Note the generic 'T3 argument.
        abstract member Run<'T3 when 'T3 : equality> : 'T1 -> 'T2

    /// Specializes the generic algorithm.
    let Specialize<'T1,'T2> (algorithm: IGeneric<'T1,'T2>) : Type -> 'T1 -> 'T2 =
        let aT = typeof<IGeneric<'T1,'T2>>
        let def = aT.GetMethod("Run").GetGenericMethodDefinition()
        let dT = typeof<Converter<'T1,'T2>>
        fun t ->
            let m = def.MakeGenericMethod [| t |]
            let f = Delegate.CreateDelegate(dT, algorithm, m)
            (f :?> Converter<'T1,'T2>).Invoke

module Utils =
    module S = Specialization

    let GetSitelet : Type -> _ -> _ =
        S.Specialize {
            new S.IGeneric<obj, Sitelet<obj> * list<obj>> with
                member this.Run<'T when 'T : equality>(website) =
                    let website =
                        match website with
                        | :? IWebsite<'T> as website -> website
                        | _ -> failwith "Invalid type: IWebsite not implemented."
                    (Sitelet.Box website.Sitelet, List.map box website.Actions)
        }

/// Mark an assembly that contains a Sitelets website, or a Sitelet static property.
[<AttributeUsage(AttributeTargets.Assembly ||| AttributeTargets.Property)>]
type WebsiteAttribute private (arg: option<System.Type * obj>) =
    inherit Attribute()

    /// Mark an assembly that contains a Sitelets website.
    /// The type passed must implement IWebsite.
    new (ty: System.Type) =
        let innerType =
            ty.GetInterfaces()
            |> Seq.tryPick (fun iT ->
                if iT.IsGenericType
                   && iT.GetGenericTypeDefinition() = typedefof<IWebsite<_>> then
                    Some (iT.GetGenericArguments().[0])
                else
                    None)
        let innerType =
            match innerType with
            | Some t -> t
            | None -> failwith "Type is not implementing IWebsite"
        let website = Activator.CreateInstance(ty)
        new WebsiteAttribute(Some(innerType, website))

    /// Mark a static property or module-bound value representing a Sitelets website.
    /// The value must have type Sitelet<'T>.
    new () =
        new WebsiteAttribute(None)

    member this.Run() =
        match arg with
        | Some (t, website) ->
            Utils.GetSitelet t website
        | None -> failwith "Cannot Run() an argumentless WebsiteAttribute"
