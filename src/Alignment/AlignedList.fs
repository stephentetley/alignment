// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module Alignment.AlignedList

[<StructuredFormatDisplay("AlignedList = {ListBody}")>]
type AlignedList<'Key,'a when 'Key : comparison> = 
    private { ListBody:'a list
              ProjectKey:'a -> 'Key }
    static member ofList (projection:'a ->'Key) (xs:'a list) : AlignedList<'Key,'a> = 
            { ListBody = xs; ProjectKey = projection}


let toList (source:AlignedList<'k,'a>) : 'a list =  source.ListBody


let differenceL (left:AlignedList<'key,'a>) (right:AlignedList<'key,'b>) : AlignedList<'key,'a> = 
    let projectL = left.ProjectKey
    let projectR = right.ProjectKey
    let dictRight = Map.ofList <| List.map (fun a -> (projectR a, a)) right.ListBody

    let rec work ac xs  = 
        match xs with
        | [] -> { ListBody = List.rev ac; ProjectKey = left.ProjectKey }
        | x::xs1 -> 
            if Map.containsKey (projectL x) dictRight then 
                // item in lefts and rights, don't accumulate
                work ac xs1 
            else
                work (x::ac) xs1
            
    work [] left.ListBody

let differenceR (left:AlignedList<'key,'a>) (right:AlignedList<'key,'b>) : AlignedList<'key,'b> = 
    let projectL = left.ProjectKey
    let projectR = right.ProjectKey
    let dictLeft = Map.ofList <| List.map (fun a -> (projectL a, a)) left.ListBody

    let rec work ac xs = 
        match xs with
        | [] -> { ListBody = List.rev ac; ProjectKey = projectR }
        | (x::xs1) -> 
            if Map.containsKey (projectR x) dictLeft then 
                // item in lefts and rights, don't accumulate
                work ac xs1 
            else
                work (x::ac) xs1
                
    work [] right.ListBody

/// Intersection 'degenerates' to a list because the projection function for 
/// the pair ('a * 'b) cannot be derived automatically.
let intersection (left:AlignedList<'key,'a>) (right:AlignedList<'key,'b>) : List<'a * 'b> = 
    let projectL = left.ProjectKey
    let projectR = right.ProjectKey
    let dictRight = Map.ofList <| List.map (fun a -> (projectR a, a)) right.ListBody

    let rec work ac xs = 
        match xs with
        | [] -> List.rev ac
        | (x::xs1) -> 
            match Map.tryFind (projectL x) dictRight with 
            | Some y -> work ((x,y)::ac) xs1 
            | None -> work ac xs1
                
    work [] left.ListBody

