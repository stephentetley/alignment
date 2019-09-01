// Copyright (c) Stephen Tetley 2019
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
        
        member v.Cartesian
            with get () : int * int = (v.X, v.X - v.K)
            

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
        let kl : KPoint = { X = 0; K = 0}
        let kr : KPoint = { X = arrA.Length; K = arrA.Length - arrB.Length}
        new Limit(kl, kr)


    

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




    let middleSnake (arrA : 'a []) (arrB : 'a []) (limit : Limit) : (int * KPoint * KPoint) option = 
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
                        sk (2 * d - 1, lefthead, righthead)
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
                        sk (2 * d, lefthead, righthead)
                   else
                        backwardLoop (k+2) d fk sk
                else
                    backwardLoop (k+2) d fk sk 
            else
                outerLoop (d+1) fk sk

        outerLoop 0 (fun () -> None) (fun x -> Some x)



    let lcs (arrA : 'a []) (arrB : 'a []) : 'a list = 
        
        let rec work token a1 n b1 m cont = 
            printfn "(%s) A='%A'; b='%A'" token a1 b1
            if n > 0 && m > 0 then
                let limit = defaultLimit a1 b1
                match middleSnake a1 b1 limit with
                | None -> 
                    cont []
                | Some (d, midleft, midright) ->                     
                    let (x,y) = midleft.Cartesian
                    let (u,v) = midright.Cartesian
                    printfn "(%s) d=%i;  (%i,%i) -> (%i,%i)" token d x y u v
                    if d > 1 then
                        work "left" a1.[0 .. x-1] x b1.[0 .. y-1] y (fun xs ->
                        let ys = 
                            match diagonalRange midleft midright with
                            | Some(ix1,ix2) -> 
                                printfn "(%s) a1.[%i..%i]" token ix1 ix2
                                a1.[ix1 .. ix2] |> Array.toList
                            | None -> []
                        printfn "(%s) ys=%O" token ys
                        work "right" a1.[u-1 .. n-1] (n-u) b1.[v-1 .. m-1] (m-v) (fun zs ->
                        cont (xs @ ys @ zs)))
                    else if m > n then
                        cont [] // (a1.[0 .. n] |> Array.toList)
                    else 
                        cont [] // (b1.[0 .. m] |> Array.toList)
            else
                cont []
        work "start" arrA (arrA.Length) arrB (arrB.Length)  (fun x -> x)



    // Initally just return the ses length...
    let compute (arrA : 'a []) (arrB : 'a []) : int = 

        let rec work (limit : Limit) cont = 
            // printfn "limit=%O" limit
            if limit.N <= 0 && limit.M <= 0 then
                cont 0
            else if limit.N > 0 && limit.M = 0 then
                // printfn "limit.N > 0 && limit.M = 0"
                // printfn "returning %i" limit.N
                cont limit.N
            else if limit.N = 0 && limit.M > 0 then
                // printfn "limit.N = 0 && limit.M > 0"
                // printfn "returning %i" limit.M
                cont limit.M
            else 
                // Find the middle snake and store the result in midleft and midright
                match middleSnake arrA arrB limit with
                | None -> 
                    // Failure?
                    // printfn "No middleSnake"
                    cont -1
                | Some (d, midleft, midright) -> 
                    printfn "middleSnake with d=%i" d
                    cont d

        let limit : Limit = defaultLimit arrA arrB

        work limit (fun x -> x)