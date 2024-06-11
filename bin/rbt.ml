type color = 
| RED 
| BLACK
type 'a rbtree =
| Leaf 
| Node of color * 'a * 'a rbtree * 'a rbtree 

let rec _mem x = function
  | Leaf -> false
  | Node (_, y, l, r) ->
    if x < y then _mem x l
    else if x > y then _mem x r
    else true

let balance = function
    | BLACK, z, Node (RED, y, Node (RED, x, a, b), c), d
    | BLACK, z, Node (RED, x, a, Node (RED, y, b, c)), d
    | BLACK, x, a, Node (RED, z, Node (RED, y, b, c), d)
    | BLACK, x, a, Node (RED, y, b, Node (RED, z, c, d)) ->
        Node (RED, y, Node (BLACK, x, a, b), Node (BLACK, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)

let insert x s =
    let rec ins = function
        | Leaf -> Node (RED, x, Leaf, Leaf)
        | Node (color, y, a, b) as s ->
            if x < y then balance (color, y, ins a, b)
            else if x > y then balance (color, y, a, ins b)
            else s in
        match ins s with
          | Node (_, y, a, b) -> Node (BLACK, y, a, b)
          | Leaf -> (* guaranteed to be nonempty *)
            failwith "RBT insert failed with ins returning leaf" 


let rec _height = function
  | Leaf -> 0
  | Node(_, _, l, r) -> 1 + max (_height l) (_height r)
    
  open Graph
  
  (* Define the graph module *)
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


(* Define the graphviz module *)
module Dot = Graphviz.Dot(struct
  include G
  let iter_vertex = G.iter_vertex
  let iter_edges_e = G.iter_edges_e
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  (* let vertex_attributes v =
    let color = match snd (G.V.label v) with
      | RED -> [`Color "red"]
      | BLACK -> [`Color "black"]
    in
    color *)
    let vertex_attributes v =
      let color = match snd (G.V.label v) with
        | RED -> [`Color 0xFF0000; `Penwidth 2.0]  (* Red in RGB *)
        | BLACK -> [`Color 0x000000; `Penwidth 2.0]  (* Black in RGB *)
      in
      color
  let vertex_name v = string_of_int(fst (G.V.label v))
  let default_edge_attributes _ = []
  let edge_attributes (_: E.t) = []
  let get_subgraph _ = None
end)

  


  
(* Function to add nodes and edges to the graph *)
let rec add_to_graph g = function
  | Leaf -> ()
  | Node (c, v, l, r) ->
    let vertex = G.V.create (v, c) in
    G.add_vertex g vertex;
    (match l with Node (_, lv, _, _) ->
      let edge = G.E.create vertex c (G.V.create (lv, c)) in
      G.add_edge_e g edge | Leaf -> ());
    (match r with Node (_, rv, _, _) -> 
      let edge = G.E.create vertex c (G.V.create (rv, c)) in
      G.add_edge_e g edge | Leaf -> ());
    add_to_graph g l;
    add_to_graph g r

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
  
(* Create the graph and add the nodes and edges *)
let () =
  let keys = read_keys_from_file "output.txt" in
  let rbtree: int rbtree = List.fold_left (fun acc key -> insert key acc) Leaf keys in
  let g = G.create () in
  add_to_graph g rbtree;
  let oc = open_out "graph.dot" in
  Dot.output_graph oc g;
  (*convert .dot to .png*)
  let _ = Sys.command "dot -Tpng graph.dot -o graph.png" in
  close_out oc

  