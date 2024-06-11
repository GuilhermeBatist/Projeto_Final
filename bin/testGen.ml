let gen_tests n =
  let () = Random.self_init () in
  let upper_bound = 250 in
  let ht = Hashtbl.create 100 in  
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
  Scanf.scanf " %d" (fun n -> print_to_file "output.txt" @@ gen_tests n)