// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an attempt at implementing the XCC diff algorithms

namespace SLAlignment


module LCS = 

    exception private ExnBreak

    let greedyLCS (source : char []) (target : char[]) : int option = 
        let m = target.Length
        let n = source.Length
        let mAX = m + n
        let size = mAX * 2 + 1      
        let v : int [] = Array.zeroCreate mAX
        let lookupV (ix: int) = v.[ix + mAX] 
        let updateV (ix: int) (value: int) = v.[ix+ mAX] <- value
        let mutable sesLength : int = -1
        try
            for d in 0 .. mAX do
                for k in -d .. 2 .. d do
                    let mutable (x : int) = 
                        if k = -d || k <> d && lookupV (k-1) < lookupV (k+1) then lookupV  (k+1) else lookupV (k-1) + 1
                    let mutable (y : int) = x - k
                    while x < n && y < m && source.[x+1] = target.[y+1] do
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



            




