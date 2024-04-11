type color = 
| RED 
| BLACK
type 'a rbtree =
| Leaf 
| Node of color * 'a * 'a rbtree * 'a rbtree 

let rec _mem x = function
  | Leaf -> false
  | Node (_, y, l, r) ->
    if x < y then _mem x l
    else if x > y then _mem x r
    else true

let balance = function
    | BLACK, z, Node (RED, y, Node (RED, x, a, b), c), d
    | BLACK, z, Node (RED, x, a, Node (RED, y, b, c)), d
    | BLACK, x, a, Node (RED, z, Node (RED, y, b, c), d)
    | BLACK, x, a, Node (RED, y, b, Node (RED, z, c, d)) ->
        Node (RED, y, Node (BLACK, x, a, b), Node (BLACK, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)

let insert x s =
    let rec ins = function
        | Leaf -> Node (RED, x, Leaf, Leaf)
        | Node (color, y, a, b) as s ->
            if x < y then balance (color, y, ins a, b)
            else if x > y then balance (color, y, a, ins b)
            else s in
        match ins s with
          | Node (_, y, a, b) -> Node (BLACK, y, a, b)
          | Leaf -> (* guaranteed to be nonempty *)
            failwith "RBT insert failed with ins returning leaf" 


let rec height = function
  | Leaf -> 0
  | Node(_, _, l, r) -> 1 + max (height l) (height r)


      
let draw_rbtree tree =
  let rec draw x y = function
    | Leaf -> ()
    | Node(color, key, left, right) ->
      let radius = 20 in
      let gap = 40 in
      let node_x = x + (height tree) * gap in
      let node_y = y * gap in
                
      (* Desenhar o nó *)
      Graphics.set_color (match color with RED -> Graphics.red | BLACK -> Graphics.black);
      Graphics.fill_circle node_x node_y radius;
      Graphics.set_color Graphics.white;
      Graphics.draw_circle node_x node_y radius;
      Graphics.moveto (node_x - 5) (node_y - 5);
      Graphics.draw_string (string_of_int key);
                
      (* Desenhar a linha para o filho esquerdo *)
      (match left with
        | Leaf -> ()
        | Node(_, _, _, _) ->
          let left_x = node_x - gap in
          let left_y = node_y - gap in
            Graphics.set_color Graphics.black;
            Graphics.moveto node_x node_y;
            Graphics.lineto left_x left_y);
                
  (* Desenhar a linha para o filho direito *)
            (match right with
            | Leaf -> ()
            | Node(_, _, _, _) ->
              let right_x = node_x + gap in
              let right_y = node_y - gap in
                Graphics.set_color Graphics.black;
                Graphics.moveto node_x node_y;
                Graphics.lineto right_x right_y);
                
              (* Desenhar os filhos *)
              draw (x + 1) (y - 1) left;
              draw (x + 1) (y - 1) right
            in
            let width = height tree in
            let total_width = int_of_float ((2. ** float_of_int width -. 1.) *. 40.) in
            let total_height = height tree in
            Graphics.open_graph (" " ^ string_of_int total_width ^ "x" ^ string_of_int (total_height * 40));
            Graphics.clear_graph ();
            draw 0 (width) tree;
            ignore (Graphics.wait_next_event [Graphics.Button_down])
          
 let keys = [9; 5; 10; 0; 6; 11; -1; 1; 2]
 let rbtree = List.fold_left (fun acc key -> insert key acc) Leaf keys
          
 let () =
  draw_rbtree rbtree;
  ignore (Graphics.wait_next_event [Graphics.Button_down])

