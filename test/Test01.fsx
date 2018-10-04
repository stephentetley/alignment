
open System.Text.RegularExpressions


#load "..\src\Alignment\Levenshtein.fs"
// #load @"Alignment/Set.fs"
open Alignment.Levenshtein
// open Alignment.Set




let s01 : Set<int> = 
    Set.ofList [1;2;3;4;5]

// FSharp orders the set as it is built
let s01a : Set<int> = 
    Set.ofList [4;5;1;3;2]


let s02 : Set<string> = 
    Set.ofList ["0"; "2"; "3"; "4"; "5"; "6"]

let set01 () = 
    /// s02.[0]   // does not compile, no elementary access to sets.
    ()


let test01 () = 
    levenshtein "mouse" "mice"

let str1 : string = 
    @"d----        22/03/2018     10:33            ALBERT_WTW           "

        

//let compareH : CompareH<int,string> = 
//    fun i str -> compare i (int str)


