(* Import the Graph module *)
open Graph

(* Define the AVL tree data structure *)
type 'a avl =
  | Leaf
  | Node of 'a * int * 'a avl * 'a avl

(* Function to get the height of a node *)
let height = function
  | Leaf -> 0
  | Node(_, h, _, _) -> h

(* Helper function to create a new node with correct height *)
let node v l r =
  Node (v, (1 + max (height l) (height r)), l, r)(*isto vai para apendice*)

(* Define the graph module for visualization *)
module G = Imperative.Digraph.Concrete(struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)

(* Define the graphviz module for graph visualization *)
module Dot = Graphviz.Dot(struct
  include G (* Use the graph module from above *)
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = []
  let vertex_name v = string_of_int v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* Function to visualize the AVL tree using Graphviz *)
let draw_avl_tree tree n=
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
  let oc = open_out "dot/avl_tree.dot" in
  Dot.output_graph oc g;
  close_out oc;
  ignore (Sys.command ("dot -Tpng dot/avl_tree.dot -o img/avl/avl_test_"^ string_of_int(n) ^".png"))

(* Function to balance the AVL tree during insertion or deletion *)
(**
  @param v: the value of the node
  @param l: the left child of the node
  @param r: the right child of the node
*)
let balance v l r   =
  let hl = height l in
  let hr = height r in
  if hl > 1 + hr then
    match l with
    | Node(lv, _, ll, lr) when height ll >= height lr -> 
        node lv ll (node v lr r) 
    | Node(vl, _, ll, Node (lrv, _, lrl, lrr)) ->
        node lrv (node vl ll lrl) (node v lrr r)
    |_ ->
        assert false
  else if hr > 1 + hl then
    match r with
    | Node (rv, _, rl, rr) when height rr >= height rl ->
        node rv (node v l rl) rr
    | Node (rv, _, Node(rlv, _, rll, rlr), rr) ->
        node rlv (node v l rll) (node rv rlr rr)
    | _ ->
        assert false
  else node v l r

(* Function to find the minimum element in the tree *)
let rec min_elt = function
  | Leaf -> raise Not_found
  | Node (v, _, Leaf, _) -> v
  | Node (_, _, l, _) -> min_elt l

(* Function to add a new element to the AVL tree *)
(**
@param x: the element to be added
*)
let rec add x = function
  | Leaf -> Node(x, 1, Leaf, Leaf)
  | Node (v, _, l, r) as t ->
      let c = compare x v in
      if c = 0 then t
      else if c < 0 then balance v (add x l) r
      else balance v l (add x r)



(* Function to find an element in the AVL tree *)
let rec _find x = function
  | Leaf -> false
  | Node (v, _, l, r) ->
      let c = compare x v in
      c = 0 || _find x (if c < 0 then l else r)

(* Function to remove the minimum element from the tree *)
let rec remove_min_elt = function
  | Leaf -> Leaf
  | Node(_, _, Leaf, r) -> r
  | Node(v, _, l, r) -> balance v (remove_min_elt l) r

(* Function to merge two AVL trees *)
let merge t1 t2 = match t1, t2 with
  | Leaf, t | t, Leaf -> t
  | _ -> balance (min_elt t2) t1 (remove_min_elt t2)

(* Function to remove an element from the AVL tree *)
let rec remove x = function
  | Leaf -> Leaf
  | Node(v, _, l, r) ->
      let c = compare x v in
      if c = 0 then merge l r
      else if c < 0 then balance v (remove x l) r
      else balance v l (remove x r)

(* Function to read keys from a file and return them as a list *)
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


  
(* Main function to create the AVL tree and visualize it *)
let () =
  let keys = read_keys_from_file "tests/output.txt" in
  let n, avl_tree = List.fold_left (fun (i,tree) key ->let t = add key tree in draw_avl_tree t i; (i+1),t ) (0,Leaf) keys in
  (*remove the eigth element of keys from avl*)
  let avl_tree = remove (List.nth keys 8) avl_tree  in
  draw_avl_tree avl_tree (n);

 