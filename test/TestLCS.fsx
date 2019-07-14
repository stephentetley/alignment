#r "netstandard"
open System.Text

#load "..\src\SLAlignment\LCS.fs"
open SLAlignment.LCS

let pathA = "abcabba"
let pathB = "cbabac"


let test0a () = 
    let d = 11
    for k in -d .. 2 .. d do 
        printfn "%i" k



let test01 () = 
    greedyLCS (Seq.toArray pathA) (Seq.toArray pathB)









