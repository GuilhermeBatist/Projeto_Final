(* filepath: /home/krrik/Projeto_Final/results/calculate_averages.ml *)
open Printf

(* Function to read lines from a file *)
(**
@param filename: string
@return: list of strings
*)
let read_lines filename =
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

(* Function to extract times from lines *)
(**
@param lines: list of strings
@return: tuple of lists of floats
*)
let extract_times lines =
  let search_times = ref [] in
  let remove_times = ref [] in
  let add_times = ref [] in
  List.iter (fun line ->
    if String.contains line ':' then
      let parts = String.split_on_char ':' line in
      if List.length parts > 1 then
        let time_str = String.trim (List.nth parts 1) in
        try
          let time = float_of_string (String.sub time_str 0 (String.index time_str ' ')) in
          if String.contains line 'S' then
            search_times := time :: !search_times
          else if String.contains line 'R' then
            remove_times := time :: !remove_times
          else if String.contains line 'A' then
            add_times := time :: !add_times
        with Failure _ -> ()
  ) lines;
  (!search_times, !remove_times, !add_times)

(* Function to calculate average of a list of floats *)
(**
@param lst: list of floats
@return: average of the list
*)
let average lst =
  let sum = List.fold_left (+.) 0.0 lst in
  sum /. float_of_int (List.length lst)

(* Main function *)
let () =
  let filename = "results/results_pre_normal_docs.txt" in
  let lines = read_lines filename in
  let (search_times, remove_times, add_times) = extract_times lines in
  let avg_search = average search_times in
  let avg_remove = average remove_times in
  let avg_add = average add_times in
  let oc = open_out_gen [Open_append; Open_creat] 0o666 filename in
  fprintf oc "Average Add words: %.6f seconds\n" avg_add;
  fprintf oc "Average Search words: %.6f seconds\n" avg_search;
  fprintf oc "Average Remove words: %.6f seconds\n" avg_remove;
  close_out oc;
  print_endline "Averages calculated and appended to the file."