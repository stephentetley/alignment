
open System.Text.RegularExpressions

#load @"Alignment/AlignedList.fs"
open Alignment.AlignedList


type Compass = NORTH | SOUTH | EAST | WEST

let uncompass (point:Compass) : string = 
    match point with
    | NORTH -> "north"
    | SOUTH -> "south"
    | EAST -> "east"
    | WEST -> "west"

let l1 = ["north";"south";"east";"west"]
let l2 = [WEST;SOUTH;WEST]


let al1 = AlignedList.ofList id l1
let al2 = AlignedList.ofList uncompass l2

let test01 () = differenceL al1 al2
let test02 () = differenceR al1 al2


