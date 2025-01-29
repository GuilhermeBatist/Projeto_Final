(*
  This script generates a file with n random integers between 0 and upper_bound.
  The user is prompted to insert the number of integers to generate.
*)
(**
@param n: the number of integers to generate
@return: a list of n random integers between 0 and upper_bound
*)
let gen_tests n =
  let () = Random.self_init () in
  let upper_bound = 10000000 in
  let ht = Hashtbl.create 1000000 in  
  let rec aux i acc =
    if i = n then acc
    else 
      let r = Random.full_int upper_bound in
      match Hashtbl.find_opt ht r with
      | None -> Hashtbl.add ht r true; aux (i+1) (r::acc)
      | Some _ -> aux i acc
    in aux 0 []

let print_to_file f lst = 
  let oc = open_out f in
  List.iter (fun e -> Printf.fprintf oc "%d\n" e) lst; 
  close_out oc

let () =
  Printf.printf "Please insert the number of ints you want to generate:\n";
  flush stdout;
  Scanf.scanf " %d" (fun n -> print_to_file ("tests/Gen_nums_" ^ string_of_int n ^ ".txt")  @@ gen_tests n)