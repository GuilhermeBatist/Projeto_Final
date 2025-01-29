(*
  This script generates a file with n random strings of variable length.
*)
(**
@param n: the number of strings to generate
@return: a list of n random strings of variable length
*)
let generate_random_string length =
  let gen () = char_of_int (97 + Random.int 26) in
  String.init length (fun _ -> gen ())

let generate_random_strings n =
  let rec aux acc n =
    if n <= 0 then acc
    else
      let length = 1 + Random.int 10 in
      let str = generate_random_string length in
      aux (str :: acc) (n - 1)
  in
  aux [] n

let write_strings_to_file filename strings =
  let oc = open_out filename in
  List.iter (fun s -> output_string oc (s ^ "\n")) strings;
  close_out oc

let () =
  Printf.printf "Please insert the number of strings you want to generate:\n";
  flush stdout;
  Scanf.scanf"%d" (fun n -> write_strings_to_file ("tests/Gen_strings_" ^ string_of_int n ^ ".txt") @@ generate_random_strings n)