
#r "netstandard"
open System.Text

#load "..\src\SLAlignment\LCS3.fs"
open SLAlignment.LCS3

let defaultLimit01 () : unit = 
    let p1 = "inclined"
    let p2 = "science"
    printfn "%O" <| defaultLimit (Seq.toArray p1) (Seq.toArray p2)
    
let middleSnake01 () = 
    let p1 = "inclined" |> Seq.toArray
    let p2 = "science"  |> Seq.toArray
    middleSnake p1 p2

let middleSnake02 () = 
    let p1 = "in" |> Seq.toArray
    let p2 = "scien"  |> Seq.toArray
    middleSnake p1 p2

let middleSnake03 () = 
    let p1 = "i" |> Seq.toArray
    let p2 = "s"  |> Seq.toArray
    middleSnake p1 p2


let middleSnake05 () = 
    let p1 = "lined" |> Seq.toArray
    let p2 = "ce"  |> Seq.toArray
    middleSnake p1 p2


let demo02 () = 
    let p1 = "inclined"
    let p2 = "science"
    try 
        lcs (Seq.toArray p1) (Seq.toArray p2) |> Some
    with
    | excn -> printfn "exception: %s" excn.Message ; None

