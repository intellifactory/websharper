// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

/// Represents a self-contained website parameterized by the type of actions.
/// A sitelet combines a router, which is used to match incoming requests to
/// actions and actions to URLs, and a controller, which is used to handle
/// the actions.
type Sitelet<'T when 'T : equality> =
    {
        Router : Router<'T>
        Controller : Controller<'T>
    }

    /// Combines two sitelets, with the leftmost taking precedence.
    static member ( <|> ) : Sitelet<'Action> * Sitelet<'Action> -> Sitelet<'Action>

    /// Called through reflection by SiteLoading.TryLoadSite.
    member internal Upcast : Sitelet<obj>

/// Provides combinators over sitelets.
module Sitelet =

    open Microsoft.FSharp.Quotations

    /// Creates an empty sitelet.
    val Empty<'Action when 'Action : equality> : Sitelet<'Action>

    /// Represents filters for protecting sitelets.
    type Filter<'Action> =
        {
            VerifyUser : string -> bool
            LoginRedirect : 'Action -> 'Action
        }

    /// Constructs a protected sitelet given the filter specification.
    val Protect<'T when 'T : equality> :
        filter: Filter<'T> ->
        site: Sitelet<'T> ->
        Sitelet<'T>

    /// Constructs a singleton sitelet that contains exactly one action
    /// and serves a single content value at a given location.
    val Content<'T when 'T : equality> :
        location: string ->
        action: 'T ->
        cnt: (Context<'T> -> Async<Content<'T>>) ->
        Sitelet<'T>

    /// Maps over the sitelet action type. Requires a bijection.
    val Map<'T1,'T2 when 'T1 : equality and 'T2 : equality> :
        ('T1 -> 'T2) -> ('T2 -> 'T1) -> Sitelet<'T1> -> Sitelet<'T2>

    /// Maps over the context passed to the sitelet content.
    val MapContext<'T when 'T : equality> :
        (Context<'T> -> Context<'T>) -> Sitelet<'T> -> Sitelet<'T>

    /// Maps over the sitelet action type with only an injection.
    val Embed<'T1, 'T2 when 'T1 : equality and 'T2 : equality> :
        ('T1 -> 'T2) -> ('T2 -> 'T1 option) -> Sitelet<'T1> -> Sitelet<'T2>

    /// Maps over the sitelet action type, where the destination action type
    /// is a discriminated union with a case containing the source type.
    val EmbedInUnion<'T1, 'T2 when 'T1 : equality and 'T2 : equality> :
        Expr<'T1 -> 'T2> -> Sitelet<'T1> -> Sitelet<'T2>

    /// Shifts all sitelet locations by a given prefix.
    val Shift<'T when 'T : equality> :
        prefix: string -> sitelet: Sitelet<'T> -> Sitelet<'T>

    /// Combines several sitelets, leftmost taking precedence.
    /// Is equivalent to folding with the choice operator.
    val Sum<'T when 'T : equality> :
        sitelets: seq<Sitelet<'T>> -> Sitelet<'T>

    /// Serves the sum of the given sitelets under a given prefix.
    /// This function is convenient for folder-like structures.
    val Folder<'T when 'T : equality> :
        prefix: string -> sitelets: seq<Sitelet<'T>> -> Sitelet<'T>

    /// Boxes the sitelet action type to Object type.
    val Upcast<'T when 'T : equality> :
        sitelet: Sitelet<'T> -> Sitelet<obj>

    /// Reverses the Upcast operation on the sitelet.
    val UnsafeDowncast<'T when 'T : equality> :
        sitelet: Sitelet<obj> -> Sitelet<'T>

    /// Constructs a sitelet with an inferred router and a given controller function.
    val Infer<'T when 'T : equality> : (Context<'T> -> 'T -> Async<Content<'T>>) -> Sitelet<'T>

    /// Constructs a sitelet with an inferred router and a given controller function.
    val InferWithCustomErrors<'T when 'T : equality>
        : (Context<'T> -> ActionEncoding.DecodeResult<'T> -> Async<Content<'T>>)
        -> Sitelet<ActionEncoding.DecodeResult<'T>>

    /// Constructs a partial sitelet with an inferred router and a given controller function.
    val InferPartial<'T1, 'T2 when 'T1 : equality and 'T2 : equality> :
        ('T1 -> 'T2) -> ('T2 -> 'T1 option) -> (Context<'T2> -> 'T1 -> Async<Content<'T2>>) -> Sitelet<'T2>

    /// Constructs a partial sitelet with an inferred router and a given controller function.
    /// The actions covered by this sitelet correspond to the given union case.
    val InferPartialInUnion<'T1, 'T2 when 'T1 : equality and 'T2 : equality> :
        Expr<'T1 -> 'T2> -> (Context<'T2> -> 'T1 -> Async<Content<'T2>>) -> Sitelet<'T2>

open System.Threading.Tasks

type SiteletBuilder =

    new : unit -> SiteletBuilder

    member With<'T> : Func<Context, 'T, Task<CSharpContent>> -> SiteletBuilder
        when 'T : equality

    member With : string * Func<Context, Task<CSharpContent>> -> SiteletBuilder

    member Install : unit -> Sitelet<obj>
