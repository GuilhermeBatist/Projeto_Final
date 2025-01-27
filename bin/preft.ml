(* Module type defining the interface for a letter *)
module type Letter = sig
  type t 
  val compare: t -> t -> int 
end

(* Module type defining the interface for a persistent set *)
module type PersistentSet = sig 
  type elt 
  type t
  val empty : t
  val add : elt -> t -> t 
  val mem :  elt -> t -> bool
  val remove : elt -> t -> t
  val inter : t -> t -> t 
  val compare : t -> t -> int 
  val iter : (elt -> unit) -> t -> unit
end

(* Module implementing a persistent set using a prefix tree (trie) *)
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
    | [] -> if not t.word then t else {t with word = false}  
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
    

  let rec iter f t =
    if t.word then f [] ;
    M.iter (fun k v -> iter (fun l -> f (k::l)) v) t.branches
    
  type 'a b = { value : 'a option ; branches : 'a b M.t}  

  let rec find x t =
    match x, t.value  with
    | [], None -> raise Not_found
    | [], Some v -> v
    | i::l , _ -> find l (M.find i t.branches)

end 
(* Function to read words from a file and return them as a list *)
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

(* Function to read keys from a file, converting each word into a list of characters *)
let read_keys_from_file filename =  
  let words = read_words_from_file filename in
  List.map (fun x -> String.to_seq x |> List.of_seq) words

(* Module L implementing the Letter interface for characters *)
module L = struct
  type t = char
  let compare = Stdlib.compare  
end

(* Creating a trie with the type of elements being lists of characters *)
module Trie = Make(L) 

(* Function to add keys to the prefix tree *)
let create_prefix_tree keys tree = 
  Trie.add keys tree  

(*--------------------------------------------BEGIN DRAW THE TRIE--------------------------------------------------------------------*)

open Graph

module Vertex = struct
  type t = string
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Edge = struct
  type t = string
  let compare = compare
  let default = ""
end

module G = Imperative.Digraph.ConcreteLabeled(Vertex)(Edge)
module CharMap = Map.Make(Char)

module Dot = Graphviz.Dot(struct
  include G
    let edge_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes _ = []
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* Function to draw a trie to a png image *)
(**
@param trie: the trie to be drawn
@param n: the number of the test
@param prefix: the prefix of the word
@param t: the trie
@return: a png image of the trie
*)
let draw_trie trie n =
  let g = G.create () in
  let rec add_edges prefix t =
    let node_label = String.concat "" (List.map (String.make 1) prefix) in
    let v = G.V.create node_label in
    G.add_vertex g v;
    CharMap.iter (fun k subtree ->
      let new_prefix = prefix @ [k] in
      let child_label = String.concat "" (List.map (String.make 1) new_prefix) in
      let child = G.V.create child_label in
      G.add_vertex g child;
      G.add_edge g v child;
      add_edges new_prefix subtree
    )t.branches
  in
  add_edges [] trie;
  let oc = open_out "dot/trie.dot" in
  Dot.output_graph oc g;
  close_out oc;
  ignore (Sys.command ("dot -Tpng dot/trie.dot  -o img/pat/pat_test_" ^ string_of_int(n) ^ ".png"))



(*---------------------------------------------END DRAW THE TRIE---------------------------------------------------------------------*)
  

 (* Main function to create a graph from the trie and output it to a file *)
 
let () = 
  let keys = read_keys_from_file "tests/1_words_test.txt" in  (* Removed $/ which seemed like a typo or placeholder *)
  let trie = List.fold_left (fun acc s -> create_prefix_tree  s acc) Trie.empty keys  in
  let word = "evaluation" in 
    let explode s = 
      List.init (String.length s) (String.get s) in 
      let nboom = explode word in
  let found = Trie.mem nboom trie in
  let w2 = "landscapes" in
  let boom = explode w2 in
  let trie = Trie.remove boom trie in
  let rem = Trie.mem boom trie in
  Printf.printf "Word %s was found: %b.\n Word %s was removed:%b\n" word found w2 (not rem);


  