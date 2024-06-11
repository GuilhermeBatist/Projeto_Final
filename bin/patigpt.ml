type 'a pt =
  | Empty
  | Leaf of string
  | Node of string * int * 'a pt * 'a pt

(* Function to determine the character position for branching *)
let get_branch_position s1 s2 =
  let min_len = min (String.length s1) (String.length s2) in
  let rec loop i =
    if i >= min_len || s1.[i] <> s2.[i] then i
    else loop (i + 1)
  in
  loop 0

let branch s1 t1 s2 t2 =
  let pos = get_branch_position s1 s2 in
  if s1.[pos] < s2.[pos] then
    Node (String.sub s1 0 pos, pos, t1, t2)
  else
    Node (String.sub s1 0 pos, pos, t2, t1)

let rec mem s = function
  | Empty -> false
  | Leaf s2 -> s = s2
  | Node (prefix, pos, left, right) ->
    let prefix_len = String.length prefix in
    if prefix_len <= pos then
      if s.[pos] = prefix.[pos] then
        mem s (if s.[pos] = prefix.[pos] then left else right)
      else
        false
    else
      false

let rec add s = function
  | Empty -> Leaf s
  | Leaf s2 as t ->
    if s = s2 then t else branch s (Leaf s) s2 t
  | Node (prefix, pos, left, right) as t ->
    let prefix_len = String.length prefix in
    if prefix_len <= pos && s.[pos] = prefix.[pos] then
      if s.[pos] = prefix.[pos] then
        Node (prefix, pos, add s left, right)
      else
        Node (prefix, pos, left, add s right)
    else
      branch s (Leaf s) prefix t

let rec remove s = function
  | Empty -> Empty
  | Leaf s2 ->
    if s = s2 then Empty else Leaf s2
  | Node (prefix, pos, left, right) ->
    if s.[pos] = prefix.[pos] then
      Node (prefix, pos, remove s left, remove s right)
    else if s.[pos] = prefix.[pos] then
      Node (prefix, pos, left, remove s right)
    else
      Node (prefix, pos, left, right)

let rec union t1 t2 =
  match t1, t2 with
  | Empty, t | t, Empty -> t
  | Leaf s, t | t, Leaf s -> add s t
  | Node (prefix1, pos1, left1, right1), Node (prefix2, pos2, left2, right2) ->
    if prefix1 = prefix2 then
      Node (prefix1, pos1, union left1 left2, union right1 right2)
    else if pos1 < pos2 then
      Node (prefix1, pos1, union left1 t2, right1)
    else if pos1 > pos2 then
      Node (prefix2, pos2, union t1 left2, right2)
    else
      branch prefix1 t1 prefix2 t2
