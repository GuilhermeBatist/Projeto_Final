type 'avl a =
|Leaf 
|Node of 'avl * int * 'avl a * 'avl a
(*------------------------------------------------------------------------------------------*)
let altura = function 
| Leaf -> 0
| Node(_, h, _, _) -> h
(*------------------------------------------------------------------------------------------*)
let fator_equilibrio = function
| Leaf -> 0
| Node(_, _, esq, dir) -> altura esq - altura dir
(*------------------------------------------------------------------------------------------*)
let atualizar_altura =  function
| Leaf -> Leaf
| Node(v, _, esq, dir) ->
  Node(v, 1 + max (altura esq) (altura dir),esq,  dir)
(*------------------------------------------------------------------------------------------*)
let rotacao_direita = function
| Node(x, _, l, Node(y, _, rl, rr)) ->
  Node(y, 1 + max (altura (Node(x, 1 + max (altura l) (altura rl),l,  rl)))
    (altura rr),Node(x, 1 + max (altura l) (altura rl),l,  rl),  rr)
| _ -> Leaf
(*------------------------------------------------------------------------------------------*)
let rotacao_esquerda = function
| Node(y,_ ,Node(x, _, ll, lr),r) ->
  Node(x,1 + max (altura ll) (altura(Node(y, 1 + max (altura lr) (altura r),lr, r))), 
  ll , Node(y, 1 + max (altura lr) (altura r), lr, r)) 
| _ -> Leaf
(*let rotacao_esquerda = function
| Node(y, _, Node(x, _, ll, lr), r) ->
  let altura_lr = 1 + max (altura lr) (altura r) in
  Node(x, 1 + max (altura ll) altura_lr, ll, Node(y, altura_lr, lr, r))
| _ -> Leaf*)
(*------------------------------------------------------------------------------------------*)
let rec inserir a v =
  match a with
  |Leaf -> Node(v, 1,Leaf ,Leaf )
  | Node( x, _,left,  r) ->
    if v < x then
      let new_left = inserir left v in
      let balanced_tree = atualizar_altura (Node(x, 1 + max (altura new_left) (altura r),new_left,  r)) in
      if fator_equilibrio balanced_tree > 1 then
        rotacao_direita balanced_tree
      else
        balanced_tree
    else if v > x then
      let new_r = inserir r v in
      let balanced_tree = atualizar_altura (Node(x, 1 + max (altura left) (altura new_r),left,  new_r)) in
      if fator_equilibrio balanced_tree < -1 then
        rotacao_esquerda balanced_tree
      else
        balanced_tree
    else
      a
(*------------------------------------------------------------------------------------------*)
let rec desenhar_arvore a level prefix =
  match a with
  | Leaf -> ()
  | Node(v, _altura,l, r) ->
    Printf.printf "%s%s (%d)\n" (String.make (level * 4) ' ') prefix v;
    desenhar_arvore l (level + 1) "L-- ";
    desenhar_arvore r (level + 1) "R-- "

let desenhar avl_tree =
  desenhar_arvore avl_tree 0 "Root: "

  let () =
  let keys = [9; 5; 10; 0; 6; 11; -1; 1; 2] in
  let avl_tree = List.fold_left inserir Leaf  keys in
  desenhar avl_tree