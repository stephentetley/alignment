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
    greedySES (Seq.toArray pathA) (Seq.toArray pathB)


let pathAi = [| 1; 2; 3; 1; 2; 2; 1 |]
let pathBi = [| 3; 2; 1; 2; 1; 3 |]

let test02 () = 
    greedySES pathAi pathBi

    
let test03 () = 
    greedySES (Seq.toArray "ABC") (Seq.toArray "C")


let dummy () = [for x in 0 .. 2 do yield x]





