#r "netstandard"

// From "Type safe diff for families of datatypes"
// Eelco Lempsink, Sean Leather, Andres Löh


#load "..\src\SLAlignment\ListDiff.fs"
open SLAlignment.ListDiff

let demo01 () = 
    patch [Cpy(1);Cpy(2);Ins(3);Cpy(4)] [1;2;4]



let demo02 () = diff [1;2;3;4;5] [1;3;4;6]

let demo03 () = 
    let source = [1;2;3;5;6;10] 
    let target = [1;2;3;4;5;6;7]
    let diffs = diff source target
    List.iter (printfn "%O") diffs
    patch diffs source
