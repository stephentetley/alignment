// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an implementation of the list diff and patch code
// from "Type-safe diff for families of datatypes" by Eelco
// Lempsink, Sean Leather and Andres Löh. 
// The code has been CPS transformed.

namespace SLAlignment


module ListDiff =  

    /// Note 
    /// Investigate the Myers algorithm as (unlike Levenshtein) it generates an edit 
    /// script rather than the edit distance.
    

    type Diff<'a> = 
        | Ins of 'a
        | Del of 'a
        | Cpy of 'a
   
    let private insertCps (x:'a)  
                    (items : 'a list) 
                    (_ : unit -> ('a list) option) 
                    (sk : 'a list -> ('a list) option) : ('a list) option = 
        sk (x :: items)
    

    
    let private deleteCps (x:'a) 
                    (items : 'a list) 
                    (fk : unit -> ('a list) option) 
                    (sk : 'a list -> ('a list) option) : ('a list) option = 
        match items with
        | [] -> fk ()
        | y :: ys -> if x = y then sk ys else fk ()


    let patch (patchSteps : Diff<'a> list) (source : 'a list) : ('a list) option = 
        let rec work ps xs fk sk = 
            match ps, xs with
            | Ins(x) :: ds, ys -> 
                work ds ys fk (fun ac ->
                insertCps x ac fk sk)

            | Del(x) :: ds, ys -> 
                deleteCps x ys fk (fun ac ->
                work ds ac fk sk)

            | Cpy(x) :: ds, ys ->
                deleteCps x ys fk (fun ac1 ->
                work ds ac1 fk (fun ac2 -> 
                insertCps x ac2 fk sk))

            | [], [] -> sk []

            | [], _ -> fk ()
        work patchSteps source (fun _ -> None) (fun xs -> Some xs)


    let inline private cost (diff: Diff<'a> list) : int = diff.Length

    let inline private choose (dx : Diff<'a> list) (dy : Diff<'a> list) : Diff<'a> list = 
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