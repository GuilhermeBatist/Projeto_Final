type 'a pt =
  | Empty
  | Leaf of string
  | Node of string * int * 'a pt * 'a pt

let zero_bit x b =
  let char_index = b / 8 in
  let bit_index = b mod 8 in
  if char_index < String.length x then
    let char = String.get x char_index in
    (Char.code char) land (1 lsl bit_index) = 0
  else
    true

let rec _mem x = function
  | Empty -> false
  | Leaf j -> x = j
  | Node (_, b, l, r) -> _mem x (if zero_bit x b then l else r)

let righmost_1_bit x =
  let rec loop i x =
    if x land 1 = 1 then i else loop (i+1) (x lsr 1)
  in
  loop 0 x

let branch p1 t1 p2 t2 =
  (* Assuming p1 and p2 are integers for lxor operation. Adjust accordingly if they are not. *)
  let b = righmost_1_bit (p1 lxor p2) in
  let p = p1 land ((1 lsl  b)  -1) in
  if zero_bit (string_of_int p1) b then Node(string_of_int p, b, t1, t2) else Node(string_of_int p, b, t2, t1)

let matches_prefix x p b =
  let prefix_length = b / 8 in
  String.sub x 0 prefix_length = String.sub p 0 prefix_length

let rec add x = function
  | Empty -> Leaf x
  | Leaf j as t -> 
      if j = x then t else branch (int_of_string x) (Leaf x) (int_of_string j) t
  | Node (p, b, l, r) as t -> 
    if matches_prefix x p b then
      if zero_bit x b then 
        Node (p, b, add x l, r)
      else 
        Node (p, b, l, add x r) 
    else  
      branch (int_of_string x) (Leaf x) (int_of_string p) t

let node = function
  | (_, _, Empty, t) 
  | (_, _, t, Empty) -> t
  | (p, b, l, r) -> Node(p, b, l, r)

let rec _remove x = function
  | Empty -> Empty
  | Leaf j as t -> 
    if x = j then Empty else t
  | Node(p, m, t0, t1) as t ->
    if matches_prefix x p m then
      if zero_bit x m then 
        node (p, m, _remove x t0, t1)
      else node (p, m, t0, _remove x t1)
    else t

let rec _union t1 t2 = match t1, t2 with
  | Empty, t | t, Empty -> t 
  | Leaf x, t | t, Leaf x -> add x t
  | Node (p1, b1, l1, r1), Node (p2, b2, l2, r2) ->
    if b1 = b2 && matches_prefix p2 p1 b1 then
      Node(p1, b1, _union l1 l2, _union r1 r2)
    else if b1 < b2 && matches_prefix p2 p1 b1 then
      if zero_bit p2 b1 then 
        Node (p1, b1, _union l1 t2, r1)
      else Node (p1, b1, l1, _union r1 t2)
    else if b1 > b2 && matches_prefix p1 p2 b2 then
      if zero_bit p1 b2 then 
        Node (p2, b2, _union t1 l2, r1)
      else Node (p2, b2, l2, _union t1 r2)
    else branch (int_of_string p1) t1 (int_of_string p2) t2

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

(* open Graph

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
  graph *)

let () =
  let keys = read_words_from_file "Joey@fakeplagio-palavras.txt" in
  let patricia = List.fold_left (fun acc key -> add key acc) Empty keys in 
  let word = "evaluation" in
  let b = _mem word patricia in
  let word2 = "weighted" in
  let b2 = _mem word2 patricia in
  let patricia = _remove word2 patricia in
  let b3 = _mem word2 patricia in
  Printf.printf "Word %s is %s in the patricia tree\n" word (if b then "present" else "not present");
  Printf.printf "Word %s is %s in the patricia tree\n" word2 (if b2 then "present" else "not present");
  Printf.printf "Word %s is %s in the patricia tree\n" word2 (if b3 then "present" else "not present") ;
  




  
  