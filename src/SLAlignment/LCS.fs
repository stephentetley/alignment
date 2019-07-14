// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an attempt at implementing the XCC diff algorithms

namespace SLAlignment


module LCS = 

    exception private ExnBreak

    let greedySES (source : 'T []) (target : 'T [])  : int option when 'T : equality = 
        let m = target.Length
        let n = source.Length
        let mAX = m + n
        let size = mAX * 2 + 1
        let v : int [] = Array.zeroCreate size
        let lookupV (ix: int) = 
            let idx = ix + mAX 
            v.[idx] 
        let updateV (ix: int) (value: int) = 
            let idx = ix + mAX
            v.[idx] <- value
        let mutable sesLength : int = -1
        try
            for d in 0 .. mAX do
                for k in -d .. 2 .. d do
                    let mutable (x : int) = 
                        if k = -d || k <> d && lookupV (k-1) < lookupV (k+1) then lookupV  (k+1) else lookupV (k-1) + 1
                    let mutable (y : int) = x - k
                    /// Note Myers's paper uses 1-indexing
                    while x < n && y < m && source.[x] = target.[y] do
                        x <- x + 1
                        y <- y + 1
                    updateV k x
                    if x >= n && y >= m then
                        sesLength <- d
                        raise ExnBreak
                    else ()
            None
        with
        | ExnBreak -> Some sesLength
        | exn -> printfn "%s" exn.Message; None


    type Operation = 
        | Ins of ixSource : int * ixTarget : int
        | Del of ixSource : int

    type EditScript = Operation list


    let inline private floorDiv (dividend : int) (divisor : int) : int = 
        let a1 = double dividend / double divisor in int (floor a1)

    let myersElderDiff (source : char []) (target : char []) : EditScript = 
        let rec work (e : char []) 
                     (f : char []) 
                     i j cont = 
            let N = e.Length
            let M = f.Length
            let L = N + M
            let Z = 2 * (min N M) + 2
            if N > 0 && M > 0 then 
                let w = N-M
                let g = Array.zeroCreate Z
                let p = Array.zeroCreate Z
                let upperH :int = let x1 = if (L % 2) <> 0 then 1 else 0 in (floorDiv L 2) + x1 + 1
                // for h in 0 .. upperH do 

                cont []
            elif N > 0  then
                cont [for n in 0 .. N do yield Del (i+n)]
            else
                cont [for n in 0 .. M do yield Ins(i, j+n)]


        work source target 0 0 (fun x -> x)
            




