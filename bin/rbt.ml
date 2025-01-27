(* Define color type for RED-BLACK Tree nodes *)

type color = 
  | RED 
  | BLACK

(* Define the RED-BLACK Tree data structure *)
type 'a rbtree =
  | Leaf 
  | Node of color * 'a * 'a rbtree * 'a rbtree 

(* Helper function to check if an element exists in the tree *)
let rec _mem x = function
  | Leaf -> false
  | Node (_, y, l, r) ->
    if x < y then _mem x l
    else if x > y then _mem x r
    else true

open Graph
  
(* Define the graph module for visualization *)
module Vertex = struct
  type t = int * color
  let _compare = compare
  let _hash = Hashtbl.hash
  let _equal = (=)
end
  
(* Define the edge label type *)
module E = struct
  type t = color
  let compare = compare
  let default = BLACK
end
  
(* Define the graph module *)
module G = Imperative.Digraph.AbstractLabeled(Vertex)(E)

(* Define the graphviz module for graph visualization *)
module Dot = Graphviz.Dot(struct
  include G
  let iter_vertex = G.iter_vertex
  let iter_edges_e = G.iter_edges_e
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes v =
    let color = match snd (G.V.label v) with
      | RED -> [`Color 0xFF0000; `Penwidth 2.0]  (* RED in RGB *)
      | BLACK -> [`Color 0x000000; `Penwidth 2.0]  (* BLACK in RGB *)
    in
    color
  let vertex_name v = string_of_int(fst (G.V.label v))
  let default_edge_attributes _ = []
  let edge_attributes (_: E.t) = []
  let get_subgraph _ = None
end)
  
(*Function to add nodes and edges to the graph *)
let draw_rb_tree t n = 
  let g = G.create () in
  let rec add_edges = function
    | Leaf -> ()
    | Node (color, x, l, r) ->
        let u = G.V.create (x, color) in
        G.add_vertex g u;
        begin match l with
          | Leaf -> ()
          | Node (_, y, _, _) ->
              let v = G.V.create (y, color) in
              G.add_vertex g v;
              G.add_edge g u v
        end;
        begin match r with
          | Leaf -> ()
          | Node (_, z, _, _) ->
              let v = G.V.create (z, color) in
              G.add_vertex g v;
              G.add_edge g u  v
        end;
        add_edges l;
        add_edges r
  in
  add_edges t;
  let oc = open_out "dot/rbt.dot" in
  Dot.output_graph oc g; 
  close_out oc;
  ignore (Sys.command ("dot -Tpng dot/rbt.dot -o img/rbt/rbt_test_"^ string_of_int(n) ^".png"))


(* Function to balance the tree after insertion *)
let balance (color, value, left, right) tree n = 
  match color, value, left, right with
  | BLACK, z, Node (RED, value, Node (RED, x, a, b), c), d
  | BLACK, z, Node (RED, x, a, Node (RED, value, b, c)), d
  | BLACK, x, a, Node (RED, z, Node (RED, value, b, c), d)
  | BLACK, x, a, Node (RED, value, b, Node (RED, z, c, d)) ->
     let nodo = Node (RED, value, Node (BLACK, x, a, b), Node (BLACK, z, c, d)) in
      draw_rb_tree tree n; 
      nodo
  | a, b, c, d -> let node = Node (a, b, c, d) in draw_rb_tree tree n; node


(* Function to insert a new element into the tree *)
(**
    @param x: the element to be inserted
    @param tree: the tree to insert the element
    @param n: the index of iteration (i.e. change version) of the tree
*)
let insert x tree n =
    let rec ins = function
        | Leaf -> Node (RED, x, Leaf, Leaf)  
        | Node (color, value, left, right) as t ->
            if x < value then balance (color, value, ins left, right) tree n 
            else if x > value then balance (color, value, left, ins right) tree n
            else t in 
  match ins tree with
    | Node (_, value, left, right) ->  Node (BLACK, value, left, right)       
    | Leaf -> failwith "RBT insert failed with ins returning leaf"  (* guaranteed to be nonempty *)

(* Helper function to calculate the height of the tree *)
let rec _height = function
  | Leaf -> 0
  | Node(_, _, l, r) -> 1 + max (_height l) (_height r)

let delete x s n = 
  let rec del = function
    | Leaf -> Leaf
    | Node (color, y, a, b)  ->
      if x < y then balance (color, y, del a, b) x n
      else if x > y then balance (color, y, a, del b) x n
      else match a, b with
        | Leaf, _ -> b
        | _, Leaf -> a
        | _, _ -> let rec min = function
          | Node (_, x, Leaf, _) -> x
          | Node (_, _, l, _) -> min l
          | Leaf -> failwith "RBT delete failed with min returning leaf"
          in
          let m = min b in
          balance (color, m, a, del b) x n
  in
  match del s with
    | Node (_, y, a, b) -> Node (BLACK, y, a, b)
    | Leaf -> Leaf  

   
    
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
  
let delete_with_check x s n =
  if _mem x s then delete x s n
  else s

(* Main function to create the graph and output it as a .dot and .png file *)
let () =  
  let keys = read_keys_from_file "tests/output.txt" in
    let n,rbtree = List.fold_left (fun (i,acc) key -> let t = insert key acc i in draw_rb_tree t i; (i+1),t) (0,Leaf) keys in
      let rbtree =  insert 27 rbtree n in
        draw_rb_tree rbtree n
  

