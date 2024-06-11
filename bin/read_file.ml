module StringSet = Set.Make(String)

let read_words_from_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  let lines = read_lines [] in
  let content = String.concat "\n" lines in
  let re = Str.regexp "[^a-zA-Z0-9]+" in
  let words = Str.split re content  in
  words 


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
  let word_set = read_words_from_file "/home/guilhermeb/Projeto_Final/bin/Os_Lusiadas.txt" in
  write_words_to_file word_set "/home/guilhermeb/Projeto_Final/bin/Os_Lusiadas_palavras.txt" 
