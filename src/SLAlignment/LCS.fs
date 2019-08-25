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


    type Coord = 
        | Coord of int * int

    type Snake = 
        { TopLeft : Coord 
          BottomRight : Coord
          Length : int
        }
    
    let lcsForward  (source : char[]) (target : char[]) (dmax : int) = 
        let m = target.Length
        let n = source.Length
        let size = dmax * 2 + 1
        let v : int [] = Array.zeroCreate size
        let lookupV (ix: int) = 
            let idx = ix + dmax 
            v.[idx] 
        let updateV (ix: int) (value: int) = 
            let idx = ix + dmax
            v.[idx] <- value
        let mutable sesLength : int = -1
        try
            for d in 0 .. dmax do
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



