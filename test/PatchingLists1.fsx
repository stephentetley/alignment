#r "netstandard"

// From "Type safe diff for families of datatypes"
// Eelco Lempsink, Sean Leather, Andres Lo\eh


type Diff<'a> = 
    | Ins of 'a
    | Del of 'a
    | Cpy of 'a
   
let insert (x:'a)  (items : 'a list) : ('a list) option = Some (x :: items)
    

    
let delete (x:'a) (items: 'a list) : ('a list) option = 
    match items with
    | [] -> None
    | y :: ys -> if x = y then Some ys else None


let patch (patchSteps : Diff<'a> list) (source : 'a list) : ('a list) option = 
    let rec work ps xs fk sk = 
        match ps, xs with
        | Ins(x) :: ds, ys -> 
            work ds ys fk (fun ac ->
            sk (x :: ac))

        | Del(x) :: ds, y :: ys -> 
            if x = y then 
                work ds ys fk sk
            else
                fk ()
        | Cpy(x) :: ds, y :: ys ->
            work ds ys fk (fun ac ->
                sk (y :: ac))
        | [], [] -> sk []
        | _, [] -> fk () 
        | [], _ -> fk ()

    work patchSteps source (fun _ -> None) (fun xs -> Some xs)

let demo01 () = 
    patch [Cpy(1);Cpy(2);Ins(3);Cpy(4)] [1;2;4]


let cost (diff: Diff<'a> list) : int = diff.Length

let choose (dx : Diff<'a> list) (dy : Diff<'a> list) : Diff<'a> list = 
    if cost dx <= cost dy then dx else dy

let diff (list1 : 'a list) (list2 : 'a list) : Diff<'a> list = 
    let rec work ks ls cont = 
        match ks,ls with
        | [], [] -> cont []
        | [], y :: ys -> 
            work [] ys (fun ac ->
            cont (Ins(y) :: ac))
        | x :: xs, [] -> 
            work xs [] (fun ac ->
            cont (Del(x) :: ac))
        | x :: xs, y :: ys ->
            if x = y then best3 x y xs ys cont else best2 x y xs ys cont
    and best2 x y xs ys cont =
        work xs (y::ys) (fun acD ->
        work (x::xs) ys (fun acI -> 
        cont (choose (Del(x) :: acD) (Ins(y) :: acI))))

    and best3 x y xs ys cont =
        work xs ys (fun acC ->
        best2 x y xs ys (fun acB2 ->
        cont (choose (Cpy(x) :: acC) acB2)))

    work list1 list2 (fun xs -> xs)

let demo02 () = diff [1;2;3;4;5] [1;3;4;6]

let demo03 () = 
    let source = [1;2;3;5;6;10] 
    let target = [1;2;3;4;5;6;7]
    let diffs = diff source target
    List.iter (printfn "%O") diffs
    patch diffs source
