﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an implementation of the tree diff and patch code
// from "Type-safe diff for families of datatypes" by Eelco
// Lempsink, Sean Leather and Andres Löh. 
// The code has been CPS transformed.

namespace SLAlignment


module TreeDiff = 

    open SLAlignment.RoseTree

    type Diff<'a> = 
        | Ins of 'a * arity : int
        | Del of 'a * arity : int
        | Cpy of 'a * arity : int
   
    let private insertCps (x:'a) 
                  (arity:int) 
                  (trees : Tree<'a> list) 
                  (fk : unit -> (Tree<'a> list) option)
                  (sk : Tree<'a> list -> (Tree<'a> list) option) : (Tree<'a> list) option = 
        try 
            let (ys, yss) = List.splitAt arity trees
            sk (Node(x,ys) :: yss)
        with
        | _ -> fk ()

    
    let private deleteCps (x:'a) 
                  (arity:int) 
                  (trees : Tree<'a> list)
                  (fk : unit -> (Tree<'a> list) option)
                  (sk : Tree<'a> list -> (Tree<'a> list) option) : (Tree<'a> list) option = 
        match trees with
        | [] -> fk ()
        | Node(y, ys) :: yss -> 
            if x = y && arity = ys.Length then sk (ys @ yss) else fk ()


    let patch (patchSteps : Diff<'a> list) (source : Tree<'a> list) : (Tree<'a> list) option = 
        let rec work ps xs fk sk = 
            match ps, xs with
            | Ins(x, sz) :: ds, ys -> 
                work ds ys fk (fun ac ->
                insertCps x sz ac fk sk)

            | Del(x, sz) :: ds, ys -> 
                deleteCps x sz ys fk (fun ac ->
                work ds ac fk sk)

            | Cpy(x, sz) :: ds, ys ->
                deleteCps x sz ys fk (fun ac1 ->
                work ds ac1 fk (fun ac2 -> 
                insertCps x sz ac2 fk sk))

            | [], [] -> sk []

            | [], _ -> fk ()
        work patchSteps source (fun _ -> None) (fun xs -> Some xs)


    let inline private cost (diff: Diff<'a> list) : int = diff.Length

    let inline private choose (dx : Diff<'a> list) (dy : Diff<'a> list) : Diff<'a> list = 
        if cost dx <= cost dy then dx else dy

    let forestDiff (list1 : Forest<'a>) (list2 : Forest<'a>) : Diff<'a> list = 
        let rec diff ks ls cont = 
            match ks,ls with
            | [], [] -> cont []
            | [], (Node(y,ys) :: yss) -> 
                diff [] (ys @ yss) (fun ac ->
                cont (Ins(y, ys.Length) :: ac))

            | (Node(x,xs) :: xss), [] -> 
                diff (xs @xss) [] (fun ac ->
                cont (Del(x, xs.Length) :: ac))

            | (Node(x,xs) :: xss), (Node(y,ys) :: yss) ->
                if x = y && xs.Length = ys.Length then best3 x xs xss y ys yss cont else best2 x xs xss y ys yss cont

        and best2 x xs xss y ys yss cont =
            diff (xs @ xss) (Node(y,ys) :: yss) (fun acD ->
            diff (Node(x,xs) :: xss) (ys @ yss) (fun acI -> 
            cont (choose (Del(x, xs.Length) :: acD) (Ins(y, ys.Length) :: acI))))

        and best3 x xs xss y ys yss cont =
            diff (xs @ xss) (ys @ yss) (fun acC ->
            best2 x xs xss y ys yss (fun acB2 ->
            cont (choose (Cpy(x, xs.Length) :: acC) acB2)))

        diff list1 list2 (fun xs -> xs)


    let treeDiff (tree1 : Tree<'a>) (tree2 : Tree<'a>) : Diff<'a> list = 
        forestDiff [tree1] [tree2]
