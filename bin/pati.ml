type 'a pt =
  | Empty
  | Leaf of string
  | Node of string * int * 'a pt * 'a pt

let zero_bit x b =
  let char_index = b / 8 in
  let bit_index = b mod 8 in
  if char_index < String.length x then
    let char = String.get x char_index in
    (Char.code char) land (1 lsl bit_index) == 0
  else
    true

let rec _mem x = function
  | Empty -> false
  | Leaf j -> x = j
  | Node (_, b, l, r) -> _mem x (if zero_bit x b then l else r)

(* This function needs a complete rewrite for strings. Placeholder for now. *)
let righmost_1_bit x = 1

(* This function needs adjustments for string handling. Placeholder for now. *)
let branch p1 t1 p2 t2 =
  let b = righmost_1_bit (p1 ^ p2) in  (* Placeholder operation *)
  let p = "" in  (* Placeholder, need to compute prefix *)
  if zero_bit p1 b 
    then Node (p, b, t1, t2)
    else Node (p, b, t2, t1)

let matches_prefix x p b =
  let prefix_length = b / 8 in
  String.sub x 0 prefix_length = String.sub p 0 prefix_length


let rec add x = function
  | Empty -> Leaf x
  | Leaf j as t -> 
      if j == x then t else branch x (Leaf x) j t 
  | Node (p ,b, l ,r) as t -> 
    if matches_prefix x p b then
      if zero_bit x b then 
        Node (p, b, add x l, r)
      else 
        Node (p, b, l, add x r) 
    else  
      branch x (Leaf x) p t 

let node  = function
  | (_, _, Empty, t) 
  | (_, _, t, Empty) -> t
  | (p, b, l, r) -> Node(p, b, l, r)

let rec _remove x = function
  | Empty -> Empty
  | Leaf j as t -> 
    if x == j then Empty else t
  | Node(p, m, t0, t1) as t ->
    if matches_prefix x p m then
      if zero_bit x m then 
        node (p, m, _remove x t0, t1)
      else node (p, m, t0, _remove x t1)
    else t

let rec _union t1 t2 = match t1, t2 with
  | Empty, t | t ,Empty-> t 
  | Leaf x, t | t, Leaf x -> add x t
  | Node (p1, b1, l1,r1),Node (p2, b2, l2, r2) ->
    if b1 == b2 && matches_prefix p2 p1 b1 then
      Node(p1, b1, _union l1 l2,_union r1 r2)
    else if b1 < b2 && matches_prefix p2 p1 b1 then
      if zero_bit p2 b1 then 
        Node (p1, b1, _union l1 t2,r2)
      else Node (p1, b1, l1,_union r1 t2)
    else if b1 > b2 && matches_prefix p1 p2 b2 then
      if zero_bit p1 b2 then 
        Node (p2, b2, _union t1 l2,r2)
      else Node (p2, b2, l2, _union t1 r2)
    else branch p1 t1 p2 t2 

    let read_words_from_file filename =
      let ic = open_in filename in
      let rec loop acc =
          try
              let line = input_line ic in
              loop (line :: acc)
          with End_of_file ->
              close_in ic;
              List.rev acc
      in
      loop []

open Graph

(* Define the vertex type *)
module Vertex = struct
  type t = string
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let string_of_vertex v = v
end
      
(* Define the edge label type *)
module E = struct
  type t = string
  let compare = compare
  let default = ""
end
      
(* Define the graph module *)
module G = Imperative.Digraph.AbstractLabeled(Vertex)(E)
      
(* Define the graph visualization module *)
      
module Dot = Graph.Graphviz.Dot(struct
  include G
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = []
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)
      
(* Create a graph from a patricia tree *)
let create_graph_from_patricia_tree patricia_tree =
  let graph = G.create () in
  let rec traverse patricia_node parent_graph_node = match patricia_node with
    | Empty -> ()
    | Leaf x ->
      let graph_node = G.V.create (string_of_int x) in
      G.add_vertex graph graph_node;
      (match parent_graph_node with
       | Some p -> G.add_edge graph p graph_node
       | None -> ());
    | Node (p, b, l, r) ->
      let graph_node_label = Printf.sprintf "Node(%d,%d)" p b in
      let graph_node = G.V.create graph_node_label in
      G.add_vertex graph graph_node;
      (match parent_graph_node with
       | Some p -> G.add_edge graph p graph_node
       | None -> ());
      traverse l (Some graph_node);
      traverse r (Some graph_node)
  in
  traverse patricia_tree None;
  graph

let () =
  let keys = read_words_from_file "input.txt" in
  let patricia = List.fold_left (fun acc key -> add key acc) Empty keys in
  let graph = create_graph_from_patricia_tree patricia in
  let oc = open_out "patricia.dot" in
  Dot.output_graph oc graph;
  (* Convert .dot to .png using the dot tool *)
  let _ = Sys.command "dot -Tpng graph.dot -o patrica.png" in
  close_out oc
  let () = Printf.printf "Graph created\n"  
  