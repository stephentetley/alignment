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

    type Forest<'a> = Tree<'a> list