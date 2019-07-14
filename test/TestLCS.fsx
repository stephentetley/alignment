#r "netstandard"
open System.Text

#load "..\src\SLAlignment\LCS.fs"
open SLAlignment.LCS

let pathA = "abcabba"
let pathB = "cbabac"

let test01 () = 
    greedyLCS (Seq.toArray pathA) (Seq.toArray pathB)









