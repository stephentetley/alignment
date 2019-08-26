
#r "netstandard"
open System.Text

#load "..\src\SLAlignment\LCS3.fs"
open SLAlignment.LCS3


let earlyReturn (test : int64 -> bool) = 
    let rec loop d failk successk = 
        if test d then 
            successk d
        else if d >= 100000000000L then
            failk ()
        else
            loop (d+1L) failk successk
    loop 0L  (fun _ -> None) (fun x -> Some x)

let demo01 () = 
    let p1 = "abcabba"
    let p2 = "cbabac"
    compute (Seq.toArray p1) (Seq.toArray p2)





        