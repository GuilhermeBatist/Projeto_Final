open Graph

type 'a avl  =
|Leaf 
|Node of 'a  * int * 'a avl  * 'a avl 

let height = function
    | Leaf -> 0
    | Node(_,h,_,_) -> h

let node v l r = 
    Node (v, (1 + max (height l) (height r)), l, r)

let balance v l r = 
    let hl = height l in
    let hr = height r in
    if hl > 1 + hr then begin
        match l with
        | Node(lv,_,ll,lr) when height ll >= height lr -> 
            node lv ll (node v lr r)
        | Node(vl, _, ll, Node (lrv, _, lrl,lrr)) ->
            node lrv (node vl ll lrl) (node v lrr r)
        |_ -> 
            assert false
        end else if hr > 1 + hl then begin
            match r with
            | Node (rv, _, rl, rr) when height rr >= height rl ->
                node rv (node v l rl) rr
            | Node (rv, _, Node(rlv, _, rll,rlr), rr) -> 
                node rlv (node v l rll) (node rv rlr rr)
            | _ -> 
                assert false
        end else node v l r 

let rec min_elt = function
    | Leaf -> raise Not_found
    | Node (v, _, Leaf, _) -> v
    | Node (_, _, l, _) -> min_elt l

let rec add x = function
    | Leaf -> Node(x, 1, Leaf, Leaf)
    | Node (v, _, l, r) as t ->
        let c = compare x v in 
        if c = 0 then t 
        else if c < 0 then balance v (add x l) r
        else balance v l (add x r)

let rec remove_min_elt = function
    | Leaf -> Leaf
    | Node(_, _, Leaf, r) -> r
    | Node(v, _, l, r) -> balance v (remove_min_elt l) r 

let merge t1 t2 = match t1, t2 with
    | Leaf, t | t, Leaf -> t
    | _ -> balance (min_elt t2) t1 (remove_min_elt t2)

let rec remove x = function
    | Leaf -> Leaf
    | Node(v, _, l, r) -> 
        let c = compare x v in
        if c = 0 then merge l r 
        else if c < 0 then balance v (remove x l)  r 
        else balance v l (remove x r)


(* let rec desenhar_arvore a level prefix =
    match a with
    | Leaf -> ()
    | Node(v, _,l, r) ->
        Printf.printf "%s%s (%d)\n" (String.make (level * 4) ' ') prefix v;
        desenhar_arvore l (level + 1) "L-- ";
        desenhar_arvore r (level + 1) "R-- "
          
let desenhar avl_tree =
    desenhar_arvore avl_tree 0 "Root: " *)
       
    let rec graph_of_avl_tree g = function
  | Leaf -> g
  | Node(x, _, left, right) ->
    let g' = graph_of_avl_tree g left in
    let g'' = graph_of_avl_tree g' right in
    let node_id = string_of_int x in
    let g'' = add_vertex g'' node_id in
    let g''' = match left with
      | Leaf -> g''
      | Node(y, _, _, _) -> add_edge g'' (string_of_int y) node_id
    in
    let g'''' = match right with
      | Leaf -> g'''
      | Node(y, _, _, _) -> add_edge g''' (string_of_int y) node_id
    in
    g''''

  

let keys = [9; 5; 10; 0; 6; 11; -1; 1; 2] 
let avl_tree = List.fold_left (fun acc e -> add e acc) Leaf keys   
let avl_graph = graph_of_avl_tree (create ()) avl_tree 
let () = Dot.output_graph stdout avl_graph  