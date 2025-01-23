type t = 
| Empty
| Leaf of string
| Node of string * int * t * t

let zero_bit x b =
  let bit = if String.length x > b then Char.code x.[b] land 1 else 0 in
  bit == 0

let rec mem x = function
  | Empty -> false
  | Leaf x2 -> x = x2
  | Node(_, b, left, right) ->
    if zero_bit x b then mem x left else mem x right


let rightmost_1_bit x =
  1

  let branch x1 t1 x2 t2 =
    let b = rightmost_1_bit (x1 ^ x2) in (* Simplification: using string concatenation as a placeholder. *)
    let x = String.sub x1 0 (b - 1) in (* This is a simplification. *)
    if zero_bit x1 b then 
      Node(x, b, t1, t2) 
    else 
      Node(x, b, t2, t1)

let match_prefix x p b =
  String.sub x 0 (b - 1)= p

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

let node = function
  |(_, _, Empty, t)  
  | (_, _, t , Empty) -> t
  | (p, b, left, right) ->
    Node(p, b, left, right)

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

let print_tree t =
  let rec aux t = match t with
    | Empty -> Printf.printf "Empty\n"
    | Leaf x -> Printf.printf "Leaf %s\n" x
    | Node(p, b, left, right) -> 
      Printf.printf "Node %s %d\n" p b;
      aux left;
      aux right
  in
  aux t 


let () =
  let keys = read_words_from_file "Joey@fakeplagio-palavras.txt" in
  let patricia = List.fold_left (fun acc key -> add key acc) Empty keys in 
  List.iter (fun key -> Printf.printf "Key %s\n" key) keys;
  Printf.printf "Patricia tree created\n" ;
  print_tree patricia;
  let word = "volume" in 
  let b =  mem word patricia in
  Printf.printf "The word %s is in the Patricia tree: %b\n" word b

