#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190616\lib\netstandard2.0"
#r "SLFormat.dll"


open SLFormat.RoseTree

let tree1 : RoseTree<string> = 
    let leaf s = RoseTree(s,[])
    RoseTree("+", [ RoseTree("*", [leaf "a"; leaf "b"]); RoseTree("/", [leaf "c"; leaf "d"]) ])

let demo01 () = 
    drawTree tree1 |> printfn "%s"

type Path<'a> = 
    | Root
    | Path of label : 'a * lefts : RoseTree<'a> list * ups : Path<'a> * rights : RoseTree<'a> list


type Location<'a> =
    | Location of RoseTree<'a> * Path<'a>

let treeRoot (source : RoseTree<'a>) : Location<'a> = 
    Location(source, Root)

let change (newValue : 'a) (loc : Location<'a>) : Location<'a> =
    match loc with | Location(RoseTree(_,kids), path) -> Location(RoseTree(newValue,kids), path)

let goLeft (loc : Location<'a>) : Location<'a> option = 
    match loc with
    | Location(t1, Path(a, l1 :: lefts, ups, rights)) -> Location(l1, Path(a, lefts, ups, t1 :: rights)) |> Some
    | Location(_, Path(_, [], _, _))  -> None
    | Location(_, Root) -> None

let goRight (loc : Location<'a>) : Location<'a> option = 
    match loc with
    | Location(t1, Path(a, lefts, ups, r1 :: rights)) -> Location(r1, Path(a, t1 :: lefts, ups, rights)) |> Some
    | Location(_, Path(_, _, _, []))  -> None
    | Location(_, Root) -> None

let goUp (loc : Location<'a>) : Location<'a> option = 
    match loc with
    | Location(t1, Path(a, lefts, ups, rights)) -> 
        let kids = List.rev lefts @ (t1 :: rights) in Location(RoseTree(a, kids), ups) |> Some
    | Location(_, Root) -> None


    
let goDown (loc : Location<'a>) : Location<'a> option = 
    match loc with
    | Location(RoseTree(a, x :: xs), path) -> Location(x, Path(a, [], path, xs)) |> Some
    | Location(RoseTree(_ , []), _) -> None


let demo02 () = 
    treeRoot tree1


let unwind (loc : Location<'a>) : RoseTree<'a> = 
    let rec work (ctx:Path<'a>) t1 cont = 
        match ctx with
        | Root -> cont t1
        | Path(a,lefts,ups,rights) -> 
            let t2 = RoseTree(a,lefts @ t1 :: rights)
            work ups t2 cont
 
    match loc with | Location(tree1, path) -> work path tree1 (fun x -> x)


let demo03 () = 
    unwind <| treeRoot tree1

let det o = match o with | None -> failwith "None" | Some (x) -> x

let demo04 () = 
    treeRoot tree1 |> (det << goDown) |> (det << goRight) |> change "/" |> unwind

let demo05 () = 
    treeRoot tree1 
        |> (det << goDown) 
        |> (det << goRight)
        |> (det << goDown) 
        |> change "1" 
        |> (det << goRight) 
        |> (change "2")
        |> unwind






