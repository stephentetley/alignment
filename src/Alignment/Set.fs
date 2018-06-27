// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module Alignment.Set

// Design Note
// Using FSharps's builtin Set obliges fairly onerous work on clients to 
// implement System.IComparable (this is involves wrapping the row type provided
// by ExcelProvider for instance).
// As we are already supplying custom comparisons elsewhere we might be better
// simply wrapping List.


type CompareH<'a,'b> = 'a -> 'b -> int


let differenceL (comparison:CompareH<'a,'b>) (left:Set<'a>) (right:Set<'b>) : Set<'a> = 
    let rec work ac xs ys = 
        match (xs,ys) with
        | xs1, [] -> Set.ofList (ac @ xs1) 
        | [], ys1 -> Set.ofList <| ac
        | (x::xs1, y::ys1) -> 
            match comparison x y with
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
        
    work [] (Seq.toList left) (Seq.toList right)


let differenceR (comparison:CompareH<'a,'b>) (left:Set<'a>) (right:Set<'b>) : Set<'b> = 
    let rec work ac xs ys = 
        match (xs,ys) with
        | xs1, [] -> Set.ofList <| ac 
        | [], ys1 -> Set.ofList (ac @ ys1) 
        | (x::xs1, y::ys1) -> 
            match comparison x y with
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
        
    work [] (Seq.toList left) (Seq.toList right)


