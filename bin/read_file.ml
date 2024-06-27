module StringSet = Set.Make(String)

let read_words_from_file filename =
  let ic = open_in filename in
  let re = Str.regexp "[^a-zA-Z0-9]+" in
  let rec read_lines acc =
    try
      let line = input_line ic in
      let words = Str.split re line in
      let updated_acc = List.fold_left (fun acc word -> StringSet.add word acc) acc words in
      read_lines updated_acc
    with End_of_file ->
      close_in ic;
      acc
  in
  let word_set = read_lines StringSet.empty in
  StringSet.elements word_set

let write_words_to_file words filename =
  let oc = open_out filename in
  let rec write_words = function
    | [] -> ()
    | word :: rest ->
        output_string oc (word ^ "\n");
        write_words rest
  in
  write_words words;
  close_out oc

let () =
  let word_set = read_words_from_file "/home/guilhermeb/Projeto_Final/bin/SW_A_NEW_HOPE.txt" in
  write_words_to_file word_set "/home/guilhermeb/Projeto_Final/SW_A_NEW_HOPE_palavras.txt"