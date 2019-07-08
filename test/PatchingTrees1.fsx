#r "netstandard"

// From "Type safe diff for families of datatypes"
// Eelco Lempsink, Sean Leather, Andres Löh

#load "..\src\SLAlignment\TreeDiff.fs"
open SLAlignment.TreeDiff



let demo01 () = 
    patch   [ Cpy(1,0); Cpy(2,0); Ins(3,0); Cpy(4,0) ] 
            [ Node(1,[]); Node(2,[]); Node(4,[]) ]


let demo02 () =
    forestDiff [ Node(1, [Node(2,[]); Node(3,[]); Node(4,[]); Node(5,[])]) ] 
               [ Node(1, [Node(3,[]); Node(4,[]) ; Node(6,[])]) ]


let demo03 () = 
    let tree1 = Node(1,[ Node(2,[]); Node(3,[])] )
    let tree2 = Node(1,[ Node(2,[ Node(4,[]) ]); Node(3,[])] )
    let diffs = treeDiff tree1 tree2
    List.iter (printfn "%O") diffs
    patch diffs [tree1]


