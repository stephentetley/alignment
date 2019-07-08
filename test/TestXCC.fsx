#r "netstandard"

#load "..\src\SLAlignment\RoseTree.fs"
#load "..\src\SLAlignment\XCC.fs"
open SLAlignment.RoseTree
open SLAlignment.XCC


let tree1 = 
    Node("a",   [ Node("b", [ Node("c", []); Node("d",[]) ])
                ; Node("e", [])
                ; Node("f", [ Node("g", []); Node("g",[]) ])
                ] )

let demo01 () = 
    labelTree tree1