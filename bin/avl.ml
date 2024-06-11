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

let rec _remove x = function
    | Leaf -> Leaf
    | Node(v, _, l, r) -> 
        let c = compare x v in
        if c = 0 then merge l r 
        else if c < 0 then balance v (_remove x l)  r 
        else balance v l (_remove x r)
     

    
    module G = Imperative.Digraph.Concrete(struct
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end)
  
  module Dot = Graphviz.Dot(struct
    include G (* use the graph module from above *)
    let edge_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes _ = []
    let vertex_name v = string_of_int v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)
        
    (* Função para abrir a janela e desenhar a árvore *)
    let draw_avl_tree tree =
        let g = G.create () in
        let rec add_edges = function
          | Leaf -> ()
          | Node(v, _, l, r) ->
            G.add_vertex g v;
            (match l with
             | Leaf -> ()
             | Node(lv, _, _, _) -> G.add_edge g v lv; add_edges l);
            (match r with
             | Leaf -> ()
             | Node(rv, _, _, _) -> G.add_edge g v rv; add_edges r)
        in
        add_edges tree;
        let oc = open_out "avl_tree.dot" in
        Dot.output_graph oc g;
        close_out oc;
        ignore (Sys.command "dot -Tpng avl_tree.dot -o avl_tree.png")
      
    

let read_keys_from_file filename =
            let ic = open_in filename in
            let rec loop acc =
                try
                    let line = input_line ic in
                    let key = int_of_string line in
                    loop (key :: acc)
                with End_of_file ->
                    close_in ic;
                    List.rev acc
            in
            loop []

            let keys = read_keys_from_file "output.txt"
   
let () = 
    let avl_tree = List.fold_left (fun tree key -> add key tree) Leaf keys in
    draw_avl_tree avl_tree;