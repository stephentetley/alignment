#r "netstandard"


type Tree23 = 
    | Leaf
    | Node2 of Tree23 * Tree23
    | Node3 of Tree23 * Tree23 * Tree23


type MetaVar = int

type Tree23C<'phi> = 
    | Hole of 'phi
    | LeafC
    | Node2C of Tree23C<'phi> * Tree23C<'phi>
    | Node3C of Tree23C<'phi> * Tree23C<'phi> * Tree23C<'phi>

type Change23<'phi> = (Tree23C<'phi> * Tree23C<'phi>)

let del (context : Tree23C<MetaVar>) (tree : Tree23) : Map<MetaVar, Tree23> option = 
    let rec work ctx t1 acc fk sk  = 
        match (ctx,t1) with
        | LeafC, Leaf -> sk acc
        | Node2C(x,y), Node2(a,b) -> 
            work x a acc fk (fun ac1 -> 
            work y b ac1 fk (fun ac2 -> 
            sk ac2))
        | Node3C(x,y,z), Node3(a,b,c) -> 
            work x a acc fk (fun ac1 -> 
            work y b ac1 fk (fun ac2 -> 
            work z c ac2 fk (fun ac3 -> 
            sk ac3)))
        | Hole(ix), t ->
            match Map.tryFind ix acc with
            | None -> sk (Map.add ix t acc)
            | Some t1 -> if t = t1 then sk acc else fk ()
        | _, _ -> fk ()
    work context tree Map.empty (fun _ -> None) (fun x -> Some x)

let ins (context : Tree23C<MetaVar>) (lookups : Map<MetaVar, Tree23>) : Tree23 option = 
    let rec work ctx fk sk = 
        match ctx with
        | LeafC -> sk Leaf
        | Node2C(x,y) -> 
            work x fk (fun v1 ->
            work y fk (fun v2 ->
            sk (Node2(v1,v2))))
        | Node3C(x,y,z) -> 
            work x fk (fun v1 ->
            work y fk (fun v2 ->
            work z fk (fun v3 ->
            sk (Node3(v1,v2,v3)))))
        | Hole ix -> 
            match Map.tryFind ix lookups with
            | None -> fk ()
            | Some t -> sk t
    work context (fun _ -> None) (fun x -> Some x)

let applyChange (change: Change23<MetaVar>) (original : Tree23) : Tree23 option = 
    match change with | (d,i) -> Option.bind (fun tree -> ins i tree) (del d original) 


let extract (operation : Tree23 -> MetaVar option) (tree : Tree23) : Tree23C<MetaVar> = 
    let rec workP t1 cont = 
        match t1 with
        | Leaf -> cont LeafC
        | Node2(a,b) -> 
            workE a (fun v1 ->
            workE b (fun v2 -> 
            cont (Node2C(v1,v2))))
        | Node3(a,b,c) -> 
            workE a (fun v1 ->
            workE b (fun v2 -> 
            workE c (fun v3 -> 
            cont (Node3C(v1,v2,v3)))))
    and workE t1 cont = 
        match operation t1 with
        | None -> workP tree cont
        | Some ix -> cont (Hole ix)
    workE tree (fun x -> x)

/// Not implemeneted as the paper says its wrong anyway
let ics (tree1 : Tree23) (tree2 : Tree23) : Tree23 -> MetaVar option = 
    fun _ -> None

let faultyChangeTree23 (tree1 : Tree23) (tree2 : Tree23) : Change23<MetaVar> = 
    (extract (ics tree1 tree2) tree1, extract (ics tree1 tree2) tree2)

type Patch23 = Tree23C<MetaVar>

let gcp (context1 : Tree23C<'var>) (context2 : Tree23C<'var>) : Tree23C<Change23<'var>> = 
    let rec work ctx1 ctx2 cont = 
        match ctx1, ctx2 with
        | LeafC, LeafC -> cont LeafC
        | Node2C(a,b), Node2C(x,y) -> 
            work a x (fun v1 -> 
            work b y (fun v2 -> 
            cont (Node2C(v1,v2))))
        | Node3C(a,b,c), Node3C(x,y,z) -> 
            work a x (fun v1 -> 
            work b y (fun v2 -> 
            work c z (fun v3 -> 
            cont (Node3C(v1,v2,v3)))))
        | a, b -> 
            cont (Hole(a,b))
    work context1 context2 (fun x -> x)




