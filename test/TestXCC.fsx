#r "netstandard"

#load "..\src\SLAlignment\RoseTree.fs"
#load "..\src\SLAlignment\LCS.fs"
#load "..\src\SLAlignment\XCC.fs"
open SLAlignment.RoseTree
open SLAlignment.LCS
open SLAlignment.XCC



let tree1 = 
    node "a"   
        [ node "b" [ leaf "c"; leaf "d" ]
        ; leaf "e"
        ; node "f" [ leaf "g"; leaf "h" ]
        ]

let tree2 = 
    node "a" 
        [ node "i" [ leaf "c"; leaf "d" ]
        ; node "j" [ leaf "e" ]
        ; node "f" [ leaf "g" ]
        ; node "k" [ leaf "h" ]
        ]

let demo01 () = 
    labelTree tree1

let demo02 () = 
    leaves tree1

let demo03 () = 
    greedyLCS (leaves tree1) (leaves tree2)
