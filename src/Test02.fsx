
open System.Text.RegularExpressions

#load @"Alignment/OrderedList.fs"
open Alignment.OrderedList


        

let l1 = [1;2;3;4;6;10]
let l2 = ["2";"3";"4";"5";"7";"8";"9"]

let ol1 = OrderedList.ofList id l1
let ol2 = OrderedList.ofList (int) l2

let test01 () = differenceL ol1 ol2
let test02 () = differenceR ol1 ol2


