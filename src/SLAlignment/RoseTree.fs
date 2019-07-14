// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLAlignment


module RoseTree = 

    type Tree<'a> = 
        | Node of label : 'a * kids : Tree<'a> list

        member x.Label 
            with get () : 'a = 
                match x with | Node(x,_) -> x

        member x.Kids 
            with get () : Tree<'a> list = 
                match x with | Node(_, kids) -> kids

    let node (label : 'a) (kids : Tree<'a> list) = Node(label, kids)

    let leaf (label : 'a) = Node(label, [])

    type Forest<'a> = Tree<'a> list

    let leaves (source : Tree<'a>) : 'a [] = 
        let rec work t1 cont =
            match t1 with
            | Node(x,[]) -> cont [x]
            | Node(_,kids) -> 
                workList kids (fun vs ->
                cont vs)
        and workList xs cont = 
            match xs with
            | [] -> cont []
            | t1 :: rest ->
                work t1  (fun vs1 -> 
                workList rest (fun vs2 -> 
                cont (vs1 @ vs2)))
        work source (fun x -> x) |> List.toArray

