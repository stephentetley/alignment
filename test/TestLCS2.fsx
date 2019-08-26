﻿
#r "netstandard"
open System.Text

#load "..\src\SLAlignment\LCS2.fs"
open SLAlignment.LCS2


let demo01 () = 
    let p1 = "abcabba"
    let p2 = "cbabac"
    sesLength p1 p2 1000


let demo02 () = 
    let p1 = "inclined"
    let p2 = "science"
    sesLength p1 p2 1000


let demo03 () = 
    let p1 = "abcabba"
    let p2 = "cbabac"
    sesLengthBackward p1 p2 1000


let demo04 () = 
    let p1 = "abcde"
    let p2 = "abcd"
    sesLengthBackward p1 p2 1000
