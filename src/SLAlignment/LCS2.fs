// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an attempt at implementing the XCC diff algorithms

namespace SLAlignment


module LCS2 = 

    type Snake = 
        { TopLeftX : int 
          TopLeftY : int
          BottomRightX : int
          BottomRightY : int
        }
    
    let lcsForwardCps (arrA : 'item []) (arrB : 'item []) (dmax : int) : int option =
        let bigN = arrA.Length
        let bigM = arrB.Length
        let size = dmax * 2 + 1
        let bigV : int [] = Array.zeroCreate size

        let lookupV (ix: int) : int = 
            let idx = ix + dmax 
            bigV.[idx] 
        
        let updateV (ix: int) (value: int) :unit = 
            let idx = ix + dmax
            bigV.[idx] <- value

        let rec outerLoop d fk sk = 
            if d <= dmax then
                innerLoop (-d) d fk sk
            else
                fk ()

        and innerLoop k d fk sk = 
            if k <= d then
                let x1 = 
                    if k = -d || k <> d && lookupV (k-1) < lookupV (k+1) then 
                        // Move down
                        lookupV  (k+1) 
                    else 
                        //  Move right
                        1 + lookupV (k-1)
                // Follow the diagonal
                let x2 = followDiagonal x1 k (fun x -> x)
                updateV k x2
                if x2 >= bigN && (x2-k) >= bigM then
                    sk d
                else
                    innerLoop (k+2) d fk sk
            else
                outerLoop (d+1) fk sk

        and followDiagonal x k sk = 
            if x < bigN && (x-k) < bigM && arrA.[x] = arrB.[x-k] then
                followDiagonal (x+1) k sk
            else
                sk x
        
        outerLoop 0 (fun () -> None) (fun i -> Some i)

    let sesLength (s1 : string) (s2 : string) (dmax : int) : int option = 
        lcsForwardCps (Seq.toArray s1) (Seq.toArray s2) dmax


