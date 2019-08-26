// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is direct translation to F# of code in Lorenzo Schori's Javascript code, 
// see github.com/znerol/node-delta 


namespace SLAlignment


module LCS3 = 

    type KPoint = 
        { X : int
          K : int
        }

        member v.Translate(other : KPoint) : KPoint = 
            { X = v.X + other.X
              K = v.K + other.K
            }

        member v.MoveLeft(d : int) : KPoint = 
            { X = v.X - d
              K = v.K - d
            }

        member v.MoveRight(d : int) : KPoint = 
            { X = v.X + d
              K = v.K + d
            }

        member v.MoveUp(d : int) : KPoint = 
            { X = v.X
              K = v.K + d
            }

        member v.MoveDown(d : int) : KPoint = 
            { X = v.X
              K = v.K + d
            }


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
        printfn "nextSnakeHeadForward" 

        // Determine the preceeding snake head. 
        // Pick the one whose furthest reaching x value is greatest.
        let (x0, k0) : int * int = 
            if k = kmin || (k <> kmax && bigV.[shift (k-1)] < bigV.[shift (k+1)]) then
                // Furthest reaching snake is above (k+1), move down
                printfn "move down"
                let k0 = k+1 
                (bigV.[shift k0], k0)
            else
                // Furthest reaching snake is left (k-1), move right
                printfn "move down"
                let k0 = k-1 
                (bigV.[shift k0] - 1, k0)
        
        // Follow the diagonal as long as there are common values in a and b.
        let bx = limit.Left.X
        let by = bx - (limit.Left.K + k)
        let n = min limit.N (limit.M + k)
        let mutable x : int = x0
        while x < n && arrA.[bx + x] = arrB.[by + x] do
            x <- x + 1
        
        // Store x value of snake head after traversing the diagonal in forward
        // direction.
        let snakeHead = { X = x; K = k}.Translate(limit.Left)

        // Memoize furthest reaching x for k
        bigV.[k] <- x
        (k0, snakeHead)

    let nextSnakeHeadBackward (arrA : 'a []) (arrB : 'a []) 
                              (k : int) (kmin : int) (kmax : int) (limit : Limit) (bigV : int []) : int * KPoint =
        // The original code uses an 'array' with negative and positive indices
        let shift ix = ix + limit.DMax
        
        printfn "nextSnakeHeadBackward" 

        // Determine the preceeding snake head. 
        // Pick the one whose furthest reaching x value is greatest.
        let (x0, k0) : int * int = 
            if k = kmin || (k <> kmax && bigV.[shift (k-1)] < bigV.[shift (k+1)]) then
                // Furthest reaching snake is underneath (k-1), move up.
                let k0 = k-1 
                (bigV.[shift k0], k0)
            else
                // ...
                let k0 = k-1 
                (bigV.[shift k0] - 1, k0)
        
        let mutable x : int = x0
        
        // Store x value of snake head before traversing the diagonal in
        // reverse direction.
        let snakeHead = { X = x; K = k}.Translate(limit.Left)
        
        // Follow the diagonal as long as there are common values in a and b.
        let bx = limit.Left.X - 1
        let by = bx - (limit.Left.K + k)
        let n = max k 0
        
        while x > n && arrA.[bx + x] = arrB.[by + x] do
            x <- x - 1
        
        // Memoize furthest reaching x for k
        bigV.[k] <- x
        (k0, snakeHead)




    let middleSnake (arrA : 'a []) (arrB : 'a []) (limit : Limit) : (int * KPoint * KPoint) option = 
        // The original code uses an 'array' with negative and positive indices
        let shift ix = ix + limit.DMax

        let delta : int = limit.Delta
        let dmax : int = int << ceil <| (double limit.DMax) / 2.0
        let checkBwSnake : bool = (delta % 2) = 0
        let size = limit.DMax * 2 + 1
        let bigVf : int [] = Array.zeroCreate size
        let bigVb : int [] = Array.zeroCreate size

        printfn "middleSnake limit=%O" limit

        bigVb.[shift (delta-1)] <- limit.N

        let rec outerLoop d fk sk = 
            if d <= dmax then
                forwardLoop (-d) d fk sk
            else
                fk ()
        
        and forwardLoop k d fk sk = 
            if k <= d then 
                let (k0, righthead) = nextSnakeHeadForward arrA arrB k (-d) d limit bigVf

                // check for overlap
                if not checkBwSnake && k >= -d - 1 + delta && k <= d - 1 + delta then
                    if bigVf.[k] >= bigVb.[k] then
                        // righthead already contains the right stuff, now set
                        // the lefthead to the values of the last k-line.
                        let lefthead = { X = bigVf.[k0]; K = k0}.Translate(limit.Left)
                        // CPS-return the number of edit script operations and left and right heads
                        sk (2 * d - 1, lefthead, righthead)
                    else 
                        forwardLoop (k+2) d fk sk
                else 
                    forwardLoop (k+2) d fk sk
            else 
                backwardLoop (-d+delta) d fk sk


        and backwardLoop k d fk sk = 
            if k <= d + delta then
                let (k0, lefthead) = nextSnakeHeadBackward arrA arrB k (-d+delta) (d+delta) limit bigVb
               
                // check for overlap
                if checkBwSnake && k >= -d && k <= d then
                   if bigVf.[k] >= bigVb.[k] then
                        // lefthead already contains the right stuff, now set
                        // the righthead to the values of the last k-line.
                        let righthead = { X = bigVb.[k0]; K = k0}.Translate(limit.Left);
                        // return the number of edit script operations
                        sk (2 * d, lefthead, righthead)
                   else
                        backwardLoop (k+2) d fk sk
                else
                    backwardLoop (k+2) d fk sk 
            else
                outerLoop (d+1) fk sk

        outerLoop 0 (fun () -> None) (fun x -> Some x)



    // Initally just return the ses length...
    let compute (arrA : 'a []) (arrB : 'a []) : int = 

        let rec work (limit : Limit) cont = 
            if limit.N <= 0 && limit.M <= 0 then
                cont 0
            else if limit.N > 0 && limit.M = 0 then
                printfn "limit.M = 0"
                cont (limit.N - 1)
            else if limit.N = 0 && limit.M > 0 then
                printfn "limit.N = 0"
                cont (limit.M - 1)
            else 
                // Find the middle snake and store the result in midleft and midright
                match middleSnake arrA arrB limit with
                | None -> 
                    // Failure?
                    printfn "No middleSnake"
                    cont -1
                | Some (d, midleft, midright) -> 
                    printfn "middleSnake with d=%i" d
                    if d = 0 then
                        // No single insert / delete operation was identified by the middle
                        // snake algorithm, this means that all the symbols between left and
                        // right are equal -> one straight diagonal on k=0
                        cont 0
                    else if d = 1 then
                        // Middle-snake algorithm identified exactly one operation. (TODO >>>>>) Report
                        // the involved snake(s) to the caller.
                        cont 1
                    else
                        // Recurse if the middle-snake algorithm encountered more than one
                        // operation.
                        if not (limit.Left = midright) then
                            let limit1 = new Limit(limit.Left, midleft)
                            work limit1 cont
                        else if not (midleft = midright) then
                            cont d
                        else if not (midright = limit.Right) then
                            let limit1 = new Limit(midright, limit.Right)
                            work limit1 cont
                        else 
                            // if midleft = midright return d (I think)
                            cont d

        let limit : Limit = defaultLimit arrA arrB

        work limit (fun x -> x)