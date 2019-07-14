// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an attempt at implementing the XCC diff algorithms

namespace SLAlignment


module XCC = 

    open SLAlignment.RoseTree

    type PathId = string   
    
    type Tree2<'a> = 
        | Node2 of label : 'a * path : PathId * kids : Tree2<'a> list


    let private correctRoot (source : Tree2<'a>) : Tree2<'a> = 
        match source with 
        | Node2(a, _, kids) -> Node2(a, "/", kids)

    let labelTree (source : Tree<'a>) : Tree2<'a> = 
        let rec work (t1 : Tree<'a>) (path : PathId) (cont : Tree2<'a> -> Tree2<'a>) = 
            match t1 with
            | Node(a,[])    -> cont (Node2(a,path,[]))
            | Node(a, kids) -> 
                workList kids path 0 (fun kids2 -> 
                cont (Node2(a, path, kids2)))
        and workList (kids : Tree<'a> list) (path : PathId) (ix : int) (cont : Tree2<'a> list -> Tree2<'a>) = 
            match kids with
            | [] -> cont []
            | k1 :: rest -> 
                let path1 = sprintf "%s/%i" path ix
                work k1 path1 (fun x ->
                workList rest path (ix + 1) (fun xs -> 
                cont (x :: xs)))

        work source "" (fun x -> correctRoot x)
