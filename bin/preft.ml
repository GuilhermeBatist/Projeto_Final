module type Letter = sig
  type t 
  val compare: t -> t -> int 
end

module type PersistentSet = sig 
  type elt 
  type t
  val empty : t
  val add : elt -> t -> t 
  val mem :  elt -> t -> bool
  val remove : elt -> t -> t
  val inter : t -> t -> t 
  val compare : t -> t -> int 
end

module Make(L : Letter) : PersistentSet with type elt = L.t list =
struct
  
  module M = Map.Make(L)

  type elt = L.t list
  type t = {word : bool; branches : t M.t;}
  
  let empty = {word = false; branches = M.empty}

  let is_empty t = not t.word && M.is_empty t.branches

  let rec mem x t = 
    match x with
    | [] -> t.word
    |i::l -> try mem l (M.find i t.branches) with Not_found -> false

  let rec add x t =
    match x with
    | [] -> if t.word then t else {t with word = true}
    | i :: l -> 
      let b = try M.find i t.branches with Not_found -> empty in 
      {t with branches = M.add i (add l b ) t.branches}

  let rec remove x t =
    match x with
    | [] -> if not t.word then t else {t with word = false}  (* Corrected the condition here *)
    | i :: l -> try
      let s  = remove l (M.find i t.branches) in 
      let new_branches = 
        if is_empty s then M.remove i t.branches 
        else M.add  i s t.branches
      in
      { t with branches = new_branches}
    with Not_found -> t 

  let rec inter t1 t2 =
    { word = t1.word && t2.word;
    branches = inter_branches t1.branches t2.branches;}
  and inter_branches m1 m2 =
    M.fold (
      fun  i ti m -> try
        let t = inter ti (M.find i m2) in 
        if is_empty t then m else M.add i t m 
      with Not_found -> m)
    m1 M.empty

  let rec compare t1 t2 =
    let c = Stdlib.compare t1.word t2.word in 
    if c <> 0 then c else M.compare compare t1.branches t2.branches
    

  type 'a b = { value : 'a option ; branches : 'a b M.t}  

  let rec _find x t =
    match x, t.value  with
    | [], None -> raise Not_found
    | [], Some v -> v
    | i::l , _ -> _find l (M.find i t.branches)

end 

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

let read_keys_from_file filename =  
  let words = read_words_from_file filename in
  List.map (fun x -> String.to_seq x |> List.of_seq) words

module L = struct
  type t = char
  (* Explicitly defining compare, even though it's the same as Stdlib.compare, for clarity and consistency with the Letter module's requirements. *)
  let compare = Stdlib.compare  
end

module Trie = Make(L) 

let create_prefix_tree keys tree = 
  Trie.add keys tree  

open Graph

module Vertex = struct
  type t = char list
  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = (=)
  
end

module G = Imperative.Digraph.ConcreteBidirectional(Vertex)

module E = struct
  type t = unit
  let compare = Stdlib.compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module Dot = Graph.Graphviz.Dot(struct
  include G
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_name v = String.concat "" (List.map (String.make 1) v)
  let vertex_attributes _ = []
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let add_to_graph g keys = 
  let rec add_to_graph' g keys = 
    match keys with
    | [] -> ()
    | x::xs -> 
      let v = G.V.create x in
      G.add_vertex g v;
      match xs with
      | [] -> ()
      | y::ys -> 
        let v' = G.V.create y in
        G.add_vertex g v';
        G.add_edge g v v';
        add_to_graph' g (y::ys)
  in
  add_to_graph' g keys


let () = 
  let keys = read_keys_from_file "SW_A_NEW_HOPE_palavras.txt" in  (* Removed $/ which seemed like a typo or placeholder *)
  let trie = List.fold_left (fun acc s -> create_prefix_tree  s acc) Trie.empty keys  in
  let g = G.create () in
  add_to_graph g keys;
  let oc = open_out "prefT.dot" in
  Dot.output_graph oc g;
  close_out oc;
  ignore (Sys.command ("dot -Tpng prefT.dot -o prefix_tree.png"))
  (* create tree or print tree or save to file *)
  
  (* Graph visualization code is commented out and should be implemented or removed based on requirements. *)
  (* let g = G.create () in
  add_to_graph g keys ;
  let oc = open_out "preft.dot" in
  Dot.output_graph oc g;
  (*convert .dot to .png*)
  let _ = Sys.command "dot -Tpng preft.dot -o preft.png" in
  close_out oc 
  let () = Printf.printf "Graph created\n" *)