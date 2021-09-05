open Core

type color = Red | Black

type 'a node = Node of { mutable value: 'a;
                         mutable color: color;
                         mutable left: 'a node;
                         mutable right: 'a node;
                         mutable parent: 'a node }
             | Leaf of 'a node ref
             | NilNode

type 'a t = 'a node ref

type direction = Left | Right | Non

let empty () : 'a t = ref NilNode

let is_leaf = function
  | Leaf _ -> true
  | _ -> false

let parent = function
  | Leaf refparent -> !refparent
  | Node fields -> fields.parent
  | NilNode -> NilNode

let set_parent node parent =
  match node with
  | Leaf refparent -> refparent := parent
  | Node fields -> fields.parent <- parent
  | NilNode -> ()

let is_root node = node <> NilNode && parent node = NilNode

let branch node =
  match parent node with
  | Node fields when phys_equal node fields.left -> Left
  | Node _ -> Right
  | _ -> Non

let is_left node =
  branch node = Left

let is_right node =
  branch node = Right

let left = function
  | Node fields -> fields.left
  | _ -> NilNode

let right = function
  | Node fields -> fields.right
  | _ -> NilNode

let set_left node child =
  match node with
  | Node fields ->
    fields.left <- child;
    set_parent child node
  | _ -> invalid_arg "Can not set child of Leaf/Nil"

let set_right node child =
  match node with
  | Node fields ->
    fields.right <- child;
    set_parent child node
  | _ -> invalid_arg "Can not set child of Leaf/Nil"

let grandparent node = node |> parent |> parent

let sibling node =
  let p = parent node in
  if is_left node then right p
  else left p

let uncle node =
  let p = parent node in
  let g = parent p in
  if is_left p then right g
  else left g

let color = function
  | Leaf _ -> Black
  | Node node -> node.color
  | _ -> invalid_arg "Nil has no color"

let set_color node color = match node with
  | Node node -> node.color <- color;
  | _ -> ()

let swap_color node1 node2 =
  let c1 = color node1 in
  let c2 = color node2 in
  set_color node2 c1;
  set_color node1 c2;
  ()

let make_node ?(parent=NilNode) ?(left=NilNode) ?(right=NilNode) ?(color=Red) value =
  let node = Node { value; color; left; right; parent; } in
  if left = NilNode then Leaf (ref node) |> set_left node;
  if right = NilNode then Leaf (ref node) |> set_right node;
  node

let left_rotate node =
  let right = right node in
  match right with 
  | Node rn ->
    set_right node rn.left;
    set_left right node;
    let p = parent node in
    set_parent right p;
    if is_left node then set_left p right
    else set_right p right;
    set_parent node right;
    right
  | _ -> invalid_arg "Can't rotate on this node"

let right_rotate node =
  let left = left node in
  match left with
  | Node ln ->
    set_left node ln.right;
    set_right left node;
    let p = parent node in
    set_parent left p;
    if is_left node then set_left p left
    else set_right p left;
    set_parent node left;
    left
  | _ -> invalid_arg "Can't rotate on this node"

let succ_node node =
  let rec leftmost node =
    let left = left node in
    match left with
    | Node _ -> leftmost left
    | _ -> node
  in
  let right = right node in
  match right with
  | Node rnode -> leftmost right
  | _ -> NilNode

let prev_node node =
  let rec rightmost node =
    let right = right node in
    match right with
    | Node rnode -> rightmost right
    | _ -> node
  in
  let left = left node in
  match left with
  | Node lnode -> rightmost left 
  | _ -> NilNode

let nearest_child node =
  match succ_node node with
  | NilNode -> prev_node node
  | succ -> succ

let find t elem =
  let rec find' node elem =
    match node with
    | Node fields when fields.value = elem -> node
    | Node fields when fields.value > elem -> find' fields.left elem
    | Node fields -> find' fields.right elem
    | _ -> NilNode
  in
  find' !t elem

let finish_adjust node g =
  swap_color (parent node) g;
  match branch node with
  | Left -> right_rotate g |> ignore
  | Right -> left_rotate g |> ignore
  | _ -> failwith "Can not adjust Leaf/NilNode"

let adjust_black_uncle node p g =
  match branch node, branch p with
  | Right, Left ->
    left_rotate p |> ignore;
    finish_adjust p g
  | Left, Right ->
    right_rotate p |> ignore;
    finish_adjust p g
  | _ ->
    finish_adjust node g

let rec adjust_insert = function
  | NilNode -> ()
  | Node _ as node when is_root node ->
    set_color node Black
  | node ->
    let p = parent node in
    if color p = Red then
      let uncle = uncle node in
      let g = parent p in
      match color uncle with
       | Red ->
         set_color uncle Black;
         set_color p Black;
         set_color g Red;
         adjust_insert g
       | Black ->
         adjust_black_uncle node p g

let insert t elem =
  let rec insert' node = match node with
    | Node fields ->
      if elem = fields.value then NilNode
      else if elem < fields.value then insert' fields.left
      else insert' fields.right
    | _ ->
      let parent = parent node in
      let newnode = make_node ~parent elem in
      (match branch node with
      | Left -> set_left parent newnode
      | Right -> set_right parent newnode
      | _ -> ());
      newnode
  in
  let node = insert' !t in (
    if is_root node then t := node;
    adjust_insert node)

let copy_value node1 node2 =
  match node1, node2 with
  | Node fields1, Node fields2 ->
    fields2.value <- fields1.value
  | _ -> ()

let replace_node node1 node2 =
  let p = parent node1 in
  set_parent node2 p;
  match branch node1 with
  | Left -> set_left p node2
  | Right -> set_right p node2
  | _ -> ()

let finish_remove p s =
  swap_color p s;
  match branch s with
  | Left ->
    set_color (right s) Black;
    left_rotate p |> ignore
  | _ ->
    set_color (left s) Black;
    right_rotate p |> ignore

let rec remove_case1 node =
  if not (is_root node) then
    let p = parent node in
    let s = sibling node in
    if color s = Red then (
      swap_color s p;
      match branch node with
      | Left -> left_rotate p |> ignore
      | _ -> right_rotate p |> ignore
    );
    adjust_black_sibling node s p
and adjust_black_sibling node s p =
  match color (left s), color (right s) with
  | Black, Black -> (
      match color p with
      | Black ->
        set_color s Red;
        remove_case1 p
      | Red ->
        swap_color p s
    )
  | Black, Red when is_left s ->
    swap_color s (right s);
    left_rotate s |> ignore;
    finish_remove p s
  | Red, Black when is_right s ->
    swap_color s (left s);
    right_rotate s |> ignore;
    finish_remove p s
  | _ -> finish_remove p s

let remove_single_child node =
  let right = right node in
  let child = if is_leaf right then left node else right in
  replace_node node child;
  if color node = Black then (
    match color child with
      | Red -> set_color child Black
      | Black -> remove_case1 child
  );
  node

let remove t elem =
  match find t elem with
  | Node fields as node -> (
      match fields.left, fields.right with
      | Node _, Node _ ->
        let child = nearest_child node in
        copy_value node child;
        remove_single_child child
      | _ -> remove_single_child node
    )
  | _ -> NilNode
