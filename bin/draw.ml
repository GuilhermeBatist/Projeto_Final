open Graph

module type VertexType = sig
  type t
  val to_string: t -> string
  val compare: t -> t -> int
  val hash: t -> int
  val equal: t -> t -> bool
end

module MyVertexType : VertexType= struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let to_string = string_of_int
end

module Make(V: VertexType) = struct
  module G = Imperative.Digraph.Concrete(V)

  module Dot = Graph.Graphviz.Dot(struct
    include G
    let edge_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes _ = []
    let vertex_name v =string_of_int (G.V.label v)
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end) 
  
  let create_graph keys add_to_graph =
    let g = G.create () in
    add_to_graph g keys;
    let oc = open_out "graph.dot" in
    Dot.output_graph oc g;
    (*convert .dot to .png*)
    let _ = Sys.command "dot -Tpng graph.dot -o graph.png" in
    close_out oc;
    Printf.printf "Graph created\n"
end

module MyGraph = Make(MyVertexType)
