
#r "netstandard"
open System.Text

#load "..\src\SLAlignment\LCS3.fs"
open SLAlignment.LCS3


let demo01 () = 
    printfn "TODO"



let earlyReturn (test : int64 -> bool) = 
    let rec loop d failk successk = 
        if test d then 
            successk d
        else if d >= 100000000000L then
            failk ()
        else
            loop (d+1L) failk successk
    loop 0L  (fun _ -> None) (fun x -> Some x)


        