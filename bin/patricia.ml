type t = 
| Empty
| Leaf of string
| Node of string * int * t * t


(* Function to verify if tree is empty*)
(**
@param tree: the Patricia tree
@return: true if the tree is empty, false otherwise
*)
let is_empty tree =
  match tree with
  | Empty -> true
  | _ -> false

(* Function to find the rightmost 1 bit in a string *)
(**
@param x: the string
@param b: the position of the rightmost 1 bit
@return: the position of the rightmost 1 bit
*)
let zero_bit x b =
  let bit = if String.length x > b then Char.code x.[b] land 1 else 0 in
  bit == 0

(* Function to find the rightmost 1 bit in a string *)
(**
@param x: the string
@return: the position of the rightmost 1 bit
*)
let rec mem x = function
  | Empty -> false
  | Leaf x2 -> x = x2
  | Node(_, b, left, right) ->
    if zero_bit x b then mem x left else mem x right

(* Function to find the rightmost 1 bit in a string *)
(**
@param x: the string
@return: the position of the rightmost 1 bit
*)
let rightmost_1_bit x =
  1

(* Function to create a new branch *)
(**
@param x: the string
@param t1: the left child of the node
@param t2: the right child of the node
@return: the new branch
*)
let branch x1 t1 x2 t2 =
  let b = rightmost_1_bit (x1 ^ x2) in (* Simplification: using string concatenation as a placeholder. *)
  let x = String.sub x1 0 (b - 1) in (* This is a simplification. *)
    if zero_bit x1 b then 
      Node(x, b, t1, t2) 
    else 
      Node(x, b, t2, t1)

(* Function to check if a string has a prefix *)
(**
@param x: the string
@param p: the prefix
@param b: the position of the rightmost 1 bit
@return: true if the string has the prefix, false otherwise
*)
let match_prefix x p b =
  String.sub x 0 (b - 1)= p

(* Function to add a new element to the Patricia tree *)
(**
@param x: the string
@param t: the Patricia tree
@return: the Patricia tree with the new element added
*)
let rec add x = function 
| Empty -> Leaf x
| Leaf j as t ->
  if x == j then t else branch x (Leaf x) j t
| Node(p, b, left, right) as t ->
    if match_prefix x p b then
      if zero_bit x b then
        Node(p, b, add x left, right)
      else
        Node(p, b, left, add x right)
    else
      branch x (Leaf x) p t

(* Function to create a new node *)
(**
@param p: the prefix
@param b: the position of the rightmost 1 bit
@param left: the left child of the node
@param right: the right child of the node
@return: the new node
*)
let node = function
  |(_, _, Empty, t)  
  | (_, _, t , Empty) -> t
  | (p, b, left, right) ->
    Node(p, b, left, right)

(* Function to remove an element from the Patricia tree *)
(**
@param x: the string
@param t: the Patricia tree
@return: the Patricia tree with the element removed
*)
let rec remove x = function
  | Empty -> Empty
  | Leaf j as t-> if x == j then Empty else t
  | Node(p, m, left, right) as t ->
    if match_prefix x p m then
      if zero_bit x m then
        node (p, m, remove x left, right)
      else
        node (p, m, left, remove x right)
    else
      t
      
(* Function to find the union of two Patricia trees *)
(**
@param t1: the first Patricia tree
@param t2: the second Patricia tree
@return: the union of the two Patricia trees
*)
let rec union t1 t2 = match t1, t2 with
  | Empty, t | t, Empty -> t
  | Leaf x, t | t, Leaf x -> add x t
  | Node(p1, b1, left1, right1), Node(p2,b2, left2, right2 ) -> 
    if b1 == b2 && match_prefix p2 p1 b1 then
      Node (p1, b1, union left1 left2, union right1 right2)
    else if b1 < b2 && match_prefix p2 p1 b1 then
      if zero_bit p2 b1 then
        Node(p1, b1, union left1 t2, right1)
      else
        Node(p1, b1, left1, union right1 t2)
      else if b1 > b2 && match_prefix p1 p2 b2 then
        if zero_bit p1 b2 then
          Node(p2, b2, union t1 left2, right2)
        else
          Node(p2, b2, left2, union right2 t1)
      else
        branch p1 t1 p2 t2

(* Function to search in the tree *)
(**
@param x: the string
@param t: the Patricia tree
@return: true if the string is found, false otherwise
*)
let rec search x = function
  | Empty -> false
  | Leaf j -> x = j
  | Node(p, b, left, right) ->
    if match_prefix x p b then
      if zero_bit x b then search x left else search x right
    else
      false

(* Function to read words from files *)
(**
@param filenames: the name of the files
@return: the list of words
*)
let read_words_from_files filenames =
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
      in
      List.flatten (List.map read_words_from_file filenames)


 (*----------------------------BEGIN DRAW THE PATRICIA TREE-----------------------------------------------------*)
(*        
module G = Graph.Imperative.Digraph.ConcreteBidirectional(struct
  type t = string
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)

module Dot = Graph.Graphviz.Dot(struct
  include G
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_name v = v
  let vertex_attributes _ = []
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let draw_patricia_tree tree n =
   let g = G.create () in
  let rec add_edges = function
    | Empty -> ()
    | Leaf x -> G.add_vertex g x
    | Node(p, _, left, right) ->
      G.add_vertex g p;
      (match left with
       |Empty -> ()
       | Leaf x -> G.add_edge g p x
       | Node(l, _, _, _) -> G.add_edge g p l; add_edges left);
      (match right with
        |Empty -> ()
       | Leaf x -> G.add_edge g p x
       | Node(r, _, _, _) -> G.add_edge g p r; add_edges right)
  in
  add_edges tree;
  let oc = open_out "dot/pat_tree.dot" in
  Dot.output_graph oc g;
  close_out oc;
  ignore (Sys.command ("dot -Tpng dot/pat_tree.dot -o img/pat/patricia_test_"^ string_of_int(n) ^".png")) *)

(*--------------------------------------------BEGIN DRAW THE TREE--------------------------------------------------------------------*)
(*---------------------------------------------BEGIN TESTS---------------------------------------------------------------------------*) 

(* Function to append results to a file *)
(**
@param filename: the name of the file
@param results: the results to be appended to the file
*)
let append_results_to_file filename results =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 filename in
  output_string oc results;
  close_out oc

(* Function to measure the time taken to add words to the Patricia tree *)
(**
@param words: the words to be added to the Patricia tree
@return: the Patricia tree and the time taken to add the words
*)
let measure_add_time words =
  let start_time = Unix.gettimeofday () in
  let tree = List.fold_left (fun acc word -> add word acc) Empty words in
  let end_time = Unix.gettimeofday () in
  let time_taken = end_time -. start_time in
  (tree, time_taken)

(* Function to measure the time taken to search for words in the Patricia tree *)
(**
@param tree: the Patricia tree to search in
@param words: the words to search for
@return: the time taken to search for the words
*)
let measure_search_time tree words =
  let start_time = Unix.gettimeofday () in
  List.iter (fun word -> ignore (search word tree)) words;
  let end_time = Unix.gettimeofday () in
  end_time -. start_time

(* Function to measure the time taken to remove words from the Patricia tree *)
(**
@param tree: the Patricia tree to remove words from
@param words: the words to remove
@return: the Patricia tree and the time taken to remove the words
*)
let measure_remove_time tree words =
  let start_time = Unix.gettimeofday () in
  let tree = List.fold_left (fun acc word -> remove word acc) tree words in
  let end_time = Unix.gettimeofday () in
  let time_taken = end_time -. start_time in
  (tree, time_taken)

(*-------------------------------------------END TESTS-------------------------------------------------------------------------------*)


(* Main function to create a graph from the Patricia tree and output it to a file *)
let () = 
  let keys = read_words_from_files ["tests/Gen_strings_100000.txt"] in
  (* Measure add time *)
  let (tree, add_time) = measure_add_time keys in
  let add_time_str = Printf.sprintf "Add words: %f seconds\n" add_time in

  (* Measure search time *)
  let search_time = measure_search_time tree keys in
  let search_time_str = Printf.sprintf "Search words: %f seconds\n" search_time in

  (* Measure remove time *)
  let (_, remove_time) = measure_remove_time tree keys in
  let remove_time_str = Printf.sprintf "Remove words: %f seconds\n" remove_time in

  (* Combine results *)
  let results = add_time_str ^ search_time_str ^ remove_time_str ^ "\n"in

  (* Append results to a file *)
  append_results_to_file "results/results_pat_100000.txt" results





  (* let print_tree t =
  let rec aux t = match t with
    | Empty -> Printf.printf "Empty\n"
    | Leaf x -> Printf.printf "Leaf %s\n" x
    | Node(p, b, left, right) -> 
      Printf.printf "Node %s %d\n" p b;
      aux left;
      aux right
  in
  aux t  *)