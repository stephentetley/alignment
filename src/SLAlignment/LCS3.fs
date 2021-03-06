﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is direct translation to F# of code in Lorenzo Schori's Javascript code, 
// see github.com/znerol/node-delta 


namespace SLAlignment


module LCS3 = 

    open System

    type KPoint = 
        { X : int
          K : int
        }
        
        member v.Y
            with get () : int = v.X - v.K
            

        member v.Translate (other : KPoint) : KPoint = 
            { X = v.X + other.X
              K = v.K + other.K
            }

        member v.MoveLeft (d : int) : KPoint = 
            { X = v.X - d
              K = v.K - d
            }

        member v.MoveRight (d : int) : KPoint = 
            { X = v.X + d
              K = v.K + d
            }

        member v.MoveUp (d : int) : KPoint = 
            { X = v.X
              K = v.K + d
            }

        member v.MoveDown (d : int) : KPoint = 
            { X = v.X
              K = v.K + d
            }

    let diagonalRange (kStart : KPoint) (kEnd : KPoint) : (int * int) option = 
        let yStart = kStart.X - kStart.K
        let yEnd = kEnd.X - kEnd.K
        if yEnd > yStart + 1 then
            // downwards and at least 1 diagonal
            Some (kStart.X+1, kEnd.X)
        else if kEnd.X > kEnd.X + 1  then
            // right and at least 1 diagonal
            Some (kStart.X+2, kEnd.X)
        else
            None
            


    type Limit (left : KPoint, right : KPoint) = 
        member v.Left : KPoint = left
        member v.Right : KPoint = right
        
        member v.Delta : int = right.K - left.K
        
        member v.N : int = right.X - left.X

        member v.M : int = v.N - v.Delta

        member v.DMax : int = v.N + v.M

        override v.ToString() = 
            sprintf "{Left = %O; Right=%O; Delta=%i; N=%i; M=%i; DMax=%i}" v.Left v.Right v.Delta v.N v.M v.DMax

    let defaultLimit (arrA : 'a []) (arrB : 'b []) : Limit = 
        let kleft : KPoint = { X = 0; K = 0}
        let kright : KPoint = { X = arrA.Length; K = arrA.Length - arrB.Length}
        new Limit(kleft, kright)


    type Snake = 
        { Start: KPoint
          End: KPoint
          D: int
        }
    

    // We use (limited) mutation as we don't want to needlessly lose speed compared to the original
    // Javascript code.
    let nextSnakeHeadForward (arrA : 'a []) (arrB : 'a []) 
                             (k : int) (kmin : int) (kmax : int) (limit : Limit) (bigV : int []) : int * KPoint =
        // The original code uses an 'array' with negative and positive indices
        let shift ix = ix + limit.DMax

        // printfn "V access - k-1=%i; k+1=%i; V.length=%i; limit.dmax=%i; " (k-1) (k+1) (bigV.Length) (limit.DMax)
               
        // Determine the preceeding snake head. 
        // Pick the one whose furthest reaching x value is greatest.
        let (x0, k0) : int * int = 
            if k = kmin || (k <> kmax && bigV.[shift (k-1)] < bigV.[shift (k+1)]) then
                // Furthest reaching snake is above (k+1), move down
                // printfn "move down"
                let k0 = k+1 
                (bigV.[shift k0], k0)
            else
                // Furthest reaching snake is left (k-1), move right
                // printfn "move right"
                let k0 = k-1 
                (bigV.[shift k0] + 1, k0)
        
        // Follow the diagonal as long as there are common values in a and b.
        // printfn "Follow the diagonal (forward)"
        let bx = limit.Left.X
        let by = bx - (limit.Left.K + k)
        let n = min limit.N (limit.M + k)
        let mutable x : int = x0
        // printfn "(forward) before while loop x=%i, bx=%i, by=%i, bx+x=%i, by+x=%i" x bx by (bx+x) (by+x)
        while x < n && arrA.[bx + x] = arrB.[by + x] do
            // printfn "while loop... x=%i" x
            x <- x + 1
        
        // printfn "After follow the diagonal (forward), x=%i" x
        // Store x value of snake head after traversing the diagonal in forward
        // direction.
        let snakeHead = { X = x; K = k}.Translate(limit.Left)

        // Memoize furthest reaching x for k
        // printfn "(forward) update bigV.{%i} <- %i" k x
        bigV.[shift k] <- x
        (k0, snakeHead)

    let nextSnakeHeadBackward (arrA : 'a []) (arrB : 'a []) 
                              (k : int) (kmin : int) (kmax : int) (limit : Limit) (bigV : int []) : int * KPoint =
        // The original code uses an 'array' with negative and positive indices
        let shift ix = ix + limit.DMax
        
        // printfn "nextSnakeHeadBackward" 

        // Determine the preceeding snake head. 
        // Pick the one whose furthest reaching x value is greatest.
        // printfn "backward, V[k-1]=%i, V[k+1]=%i" (bigV.[shift (k-1)]) (bigV.[shift (k+1)])
        let (x0, k0) : int * int = 
            if k = kmax || (k <> kmin && bigV.[shift (k-1)] > Int32.MinValue && bigV.[shift (k-1)] < bigV.[shift (k+1)]) then
                // Furthest reaching snake is underneath (k-1), move up.
                // printfn "move up"
                let k0 = k-1 
                (bigV.[shift k0], k0)
            else
                // ...
                // printfn "move right"
                let k0 = k+1 
                (bigV.[shift k0] - 1, k0)
        
        let mutable x : int = x0
        
        // Store x value of snake head before traversing the diagonal in
        // reverse direction.
        let snakeHead = { X = x; K = k}.Translate(limit.Left)
        
        // Follow the diagonal as long as there are common values in a and b.
        let bx = limit.Left.X - 1
        let by = bx - (limit.Left.K + k)
        let n = max k 0
        // printfn "(backward) before while loop x=%i, n=%i, bx=%i, by=%i, bx+x=%i, by+x=%i" x n bx by (bx+x) (by+x)
        while x > n && arrA.[bx + x] = arrB.[by + x] do
            x <- x - 1
        
        // Memoize furthest reaching x for k
        // printfn "(backward) update bigV.{%i} <- %i" k x
        bigV.[shift k] <- x
        (k0, snakeHead)




    let middleSnake (arrA : 'a []) (arrB : 'a [])  : Snake option = 
        let limit = defaultLimit arrA arrB
        // The original code uses an 'array' with negative and positive indices
        let shift ix = ix + limit.DMax

        let delta : int = limit.Delta
        let dmax : int = int << ceil <| (double limit.DMax) / 2.0
        let checkBwSnake : bool = (delta % 2) = 0
        let size = limit.DMax * 2 + 1
        let bigVf : int [] = Array.zeroCreate size
        let bigVb : int [] = Array.init size (fun _ -> Int32.MinValue)

        // printfn "middleSnake limit=%O" limit

        bigVb.[shift (delta-1)] <- limit.N

        let rec outerLoop d fk sk = 
            if d <= dmax then
                forwardLoop (-d) d fk sk
            else
                fk ()
        
        and forwardLoop k d fk sk = 
            // printfn "forward snake (inner loop), d=%i, k=%i" d k
            if k <= d then 
                let (k0, righthead) = nextSnakeHeadForward arrA arrB k (-d) d limit bigVf
                // printfn "k0 answer (forward) = %i" k0
                // check for overlap
                if not checkBwSnake && k >= -d - 1 + delta && k <= d - 1 + delta then
                    // The Javascript implementation relies on Vb being a sparse 
                    // array at this point

                    if bigVb.[shift k] > Int32.MinValue && bigVf.[shift k] >= bigVb.[shift k] then
                        // righthead already contains the right stuff, now set
                        // the lefthead to the values of the last k-line.
                        let lefthead = { X = bigVf.[shift k0]; K = k0}.Translate(limit.Left)
                        // CPS-return the number of edit script operations and left and right heads
                        // printfn "forward - success"
                        sk { D = 2 * d - 1; Start = lefthead; End = righthead }
                    else 
                        forwardLoop (k+2) d fk sk
                else 
                    forwardLoop (k+2) d fk sk
            else 
                backwardLoop (-d+delta) d fk sk


        and backwardLoop k d fk sk = 
            // printfn "backward snake (inner loop), d=%i, k=%i" d k
            if k <= d + delta then
                let (k0, lefthead) = nextSnakeHeadBackward arrA arrB k (-d+delta) (d+delta) limit bigVb
                // printfn "k0 answer (backward) = %i" k0
                // check for overlap
                if checkBwSnake && k >= -d && k <= d then
                   if bigVb.[shift k] > Int32.MinValue && bigVf.[shift k] >= bigVb.[shift k] then
                        // lefthead already contains the right stuff, now set
                        // the righthead to the values of the last k-line.
                        let righthead = { X = bigVb.[shift k0]; K = k0}.Translate(limit.Left);
                        // return the number of edit script operations
                        // printfn "backward - success"
                        sk { D = 2 * d; Start = lefthead; End = righthead }
                   else
                        backwardLoop (k+2) d fk sk
                else
                    backwardLoop (k+2) d fk sk 
            else
                outerLoop (d+1) fk sk

        outerLoop 0 (fun () -> None) (fun x -> Some x)

    let private slice (start: int) (e: int) (arr: 'a[]) : 'a[] = 
        if e < start then [| |] else arr.[start..e]

    let private sliceRight (start: int) (arr: 'a[]) : 'a[] = arr.[start..]


    let lcs (arrA : 'a[]) (arrB : 'a[]) : int * Snake list =         
        let rec work (a1 : 'a[]) (b1 : 'a[]) (origin: KPoint) (snakes : Snake list) (cont : int -> Snake list -> int * Snake list)  = 
            let n = a1.Length
            let m = b1.Length
            if n <= 0 || m <= 0 then
                cont 0 snakes
            else
                match middleSnake a1 b1 with
                | None -> 
                    cont 0 snakes
                | Some ms ->   
                    // printfn "%O" ms
                    if ms.D <= 1 then
                        cont ms.D (ms :: snakes)
                    else
                        printfn "Left  a1.[0..%i] b1.[0..%i]" (ms.Start.X-1) (ms.Start.Y-1)
                        work (slice 0 (ms.Start.X - 1) a1) (slice 0 (ms.Start.Y - 1) b1)  origin   snakes (fun _ snakes1 ->
                        printfn "Right a1.[%i..] b1.[%i..]" (ms.End.X+1) (ms.End.Y+1)
                        let ms1 = { ms with Start = ms.Start.Translate(origin); End = ms.End.Translate(origin) }
                        work (sliceRight (ms.End.X+1) a1) (sliceRight (ms.End.Y+1) b1) ms.End (ms1 :: snakes1) (fun _ snakes2 -> 
                        cont ms.D snakes2))                        
        work arrA arrB {X = 0; K=0} [] (fun x y -> (x,y))



