#r "netstandard"

// From "Type safe diff for families of datatypes"
// Eelco Lempsink, Sean Leather, Andres Lo\eh

type Tree<'a> = 
    | Node of label : 'a * kids : Tree<'a> list



type Diff<'a> = 
    | Ins of 'a * arity : int
    | Del of 'a * arity : int
    | Cpy of 'a * arity : int
   
let insert (x:'a) (arity:int) (trees : Tree<'a> list) : (Tree<'a> list) option = 
    try 
        let (ys, yss) = List.splitAt arity trees
        Some (Node(x,ys) :: yss)
    with
    | _ -> None

    
let delete (x:'a) (arity:int) (trees : Tree<'a> list) : (Tree<'a> list) option = 
    match trees with
    | [] -> None
    | Node(y, ys) :: yss -> 
        if x = y && arity = ys.Length then Some (ys @ yss) else None


let patch (patchSteps : Diff<'a> list) (source : Tree<'a> list) : (Tree<'a> list) option = 
    let rec work ps xs fk sk = 
        match ps, xs with
        | [], [] -> sk []
        | _, [] -> fk () 
        | [], _ -> fk () 
        | Ins(x,sz) :: ds, ys -> 
            work ds ys fk (fun ac ->
            sk (x :: ac))
        | Del(x,sz) :: ds, _ :: ys -> 
            work ds ys fk sk
        | Cpy(x,sz) :: ds, y :: ys ->
            work ds ys fk (fun ac ->
                sk (y :: ac))
    work patchSteps source (fun _ -> None) (fun xs -> Some xs)

//let demo01 () = 
//    patch [Cpy(1);Cpy(2);Ins(3);Cpy(4)] [1;2;4]


let cost (diff: Diff<'a> list) : int = diff.Length

let choose (dx : Diff<'a> list) (dy : Diff<'a> list) : Diff<'a> list = 
    if cost dx <= cost dy then dx else dy

let diff (list1 : Tree<'a> list) (list2 : Tree<'a> list) : Diff<'a> list = 
    let rec work ks ls cont = 
        match ks,ls with
        | [], [] -> cont []
        | [], (Node(y,ys) :: yss) -> 
            work [] (ys @ yss) (fun ac ->
            cont (Ins(y, ys.Length) :: ac))

        | (Node(x,xs) :: xss), [] -> 
            work (xs @xss) [] (fun ac ->
            cont (Del(x, xs.Length) :: ac))

        | (Node(x,xs) :: xss), (Node(y,ys) :: yss) ->
            if x = y && xs.Length = ys.Length then best3 x xs xss y ys yss cont else best2 x xs xss y ys yss cont

    and best2 x xs xss y ys yss cont =
        work (xs @ xss) (Node(y,ys) :: yss) (fun acD ->
        work (Node(x,xs) :: xss) (ys @ yss) (fun acI -> 
        cont (choose (Del(x, xs.Length) :: acD) (Ins(y, ys.Length) :: acI))))

    and best3 x xs xss y ys yss cont =
        work xs ys (fun acC ->
        best2 x xs xss y ys yss (fun acB2 ->
        cont (choose (Cpy(x, xs.Length) :: acC) acB2)))

    work list1 list2 (fun xs -> xs)

let demo02 () = diff [ Node(1, [Node(2,[]); Node(3,[]); Node(4,[]); Node(5,[])]) ] 
                     [ Node(1, [Node(3,[]); Node(4,[]) ; Node(6,[])]) ]


