// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module Alignment.OrderedList

open System

/// If we pair the list with its key extracting function, we don't need to 
/// supply comparison functions to ```differenceL```, ```differenceR``` etc.

[<StructuredFormatDisplay("OrderedList = {ListBody}")>]
type OrderedList<'Key,'a when 'Key : comparison> = 
    private { ListBody:'a list
              ProjectKey:'a -> 'Key }
    static member ofList (projection:'a ->'Key) (xs:'a list) : OrderedList<'Key,'a> = 
            { ListBody = List.sortBy projection xs; ProjectKey = projection}


let toList (source:OrderedList<'k,'a>) : 'a list =  source.ListBody


let differenceL (left:OrderedList<'key,'a>) (right:OrderedList<'key,'b>) : OrderedList<'key,'a> = 
    let projectL = left.ProjectKey
    let projectR = right.ProjectKey
    let rec work ac xs ys = 
        match (xs,ys) with
        | xs1, [] -> { ListBody = (List.rev ac) @ xs1; ProjectKey = left.ProjectKey }
        | [], ys1 -> { ListBody = List.rev ac; ProjectKey = left.ProjectKey }
        | (x::xs1, y::ys1) -> 
            match compare (projectL x) (projectR y) with
            | i when i = 0 -> 
                // item in xs and ys, don't accumulate
                work ac xs1 ys1
            | i when i < 0 -> 
                // ys is in front of xs, so accumulate head of xs, still consider all of ys
                work (x::ac) xs1 ys
            | i when i > 0 -> 
                // xs is in front of ys, move to next of ys
                work ac xs ys1
            | i -> failwithf "differenceL - Weird (impossible) pattern failure: %i" i
        
    work [] left.ListBody right.ListBody



let differenceR (left:OrderedList<'key,'a>) (right:OrderedList<'key,'b>) : OrderedList<'key,'b> = 
    let projectL = left.ProjectKey
    let projectR = right.ProjectKey
    let rec work ac xs ys = 
        match (xs,ys) with
        | xs1, [] -> { ListBody = List.rev ac; ProjectKey = projectR }
        | [], ys1 -> { ListBody = (List.rev ac) @ ys1; ProjectKey = projectR }
        | (x::xs1, y::ys1) -> 
            match compare (projectL x) (projectR y) with
            | i when i = 0 -> 
                // item in xs and ys, don't accumulate
                work ac xs1 ys1
            | i when i < 0 -> 
                // ys ahead of xs, move to next of xs
                work ac xs1 ys
            | i when i > 0 -> 
                // xs ahead of ys, so head of ys not in xs (accumulate), still consider all of xs
                work (y::ac) xs ys1
            | i -> failwithf "differenceR - Weird (impossible) pattern failure: %i" i
        
    work [] left.ListBody right.ListBody

/// Intersection 'degenerates' to a list because the projection function for 
/// the pair ('a * 'b) cannot be derived automatically.
let intersection (left:OrderedList<'key,'a>) (right:OrderedList<'key,'b>) : List<'a * 'b> = 
    let projectL = left.ProjectKey
    let projectR = right.ProjectKey
    
    let rec work ac xs ys = 
        match (xs,ys) with
        | _, [] -> List.rev ac
        | [], _ -> List.rev ac
        | (x::xs1, y::ys1) -> 
            match compare (projectL x) (projectR y) with
            | i when i = 0 -> 
                // item in xs and ys, accumulate
                work ((x,y)::ac) xs1 ys1
            | i when i < 0 -> 
                // ys ahead of xs, move to next of xs
                work ac xs1 ys
            | i when i > 0 -> 
                // xs ahead of ys, so head of ys not in xs (accumulate), still consider all of xs
                work ac xs ys1
            | i -> failwithf "union - Weird (impossible) pattern failure: %i" i
        
    work [] left.ListBody right.ListBody


