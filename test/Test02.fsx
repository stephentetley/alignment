// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#load "..\src\SLAlignment\OrderedList.fs"
open SLAlignment.OrderedList


        

let l1 = [1;2;3;4;6;10]
let l2 = ["2";"3";"4";"5";"7";"8";"9"]

let ol1 = OrderedList.ofList id l1
let ol2 = OrderedList.ofList (int) l2

let test01 () = differenceL ol1 ol2
let test02 () = differenceR ol1 ol2
let test03 () = intersection ol1 ol2



