open Core

type color = Red | Black

type 'a node =
  | Node of {
      mutable value: 'a;
      mutable color: color;
      mutable left: 'a node;
      mutable right: 'a node;
      mutable parent: 'a node;
    }
  | Leaf

type 'a t = 'a node ref

type direction = Left | Right | Stop

(* type branch = L | R *)

let flip = function
  | Left -> Right
  | Right -> Left
  | Stop -> Stop

let color_repr = function
  | Red -> "red"
  | Black -> "black"

let empty () = ref Leaf

let is_empty = function
  | {contents=Leaf} -> true
  | _ -> false

let is_leaf = function
  | Leaf -> true
  | _ -> false

let left = function
  | Node n -> n.left
  | _ -> invalid_arg "leaf has no child"

let right = function
  | Node n -> n.right
  | _ -> invalid_arg "leaf has no child"

let value = function
  | Node n -> n.value
  | Leaf -> -1

let child_node node = function
  | Left -> left node
  | Right -> right node
  | Stop -> invalid_arg "invalid direction"

let set_child node child branch = match node with
  | Node nd -> (match branch with
    | Left -> nd.left <- child
    | Right -> nd.right <- child
    | Stop -> invalid_arg "invalid direction");
    (match child with
    | Node nd' -> nd'.parent <- node
    | Leaf -> ());
  | Leaf -> invalid_arg "leaf has no child"

let parent = function
  | Node n -> n.parent
  | Leaf -> invalid_arg "leaf has no parent"

let set_parent ?(branch=Stop) node p = match node with
    | Node n ->
      n.parent <- p;
      (match p with
       | Node nd -> (match branch with
           | Left -> nd.left <- node
           | Right -> nd.right <- node
           | Stop -> ());
       | Leaf -> ());
    | Leaf -> invalid_arg "leaf has no parent"

let is_root node =
  phys_equal (parent node) Leaf

let is_left node p = match node with
  | Node _ -> phys_equal node (left p)
  | Leaf -> invalid_arg "should not call is_left on leaf"

let get_branch node parent =
  try
    if is_left node parent then Left
    else Right
  with invalid_arg -> Stop

let sibling node =
  let p = parent node in
  get_branch node p |> flip |> child_node p

let uncle node =
  sibling (parent node)

let color = function
  | Node node -> node.color
  | Leaf -> Black

let set_color node color = match node with
  | Node node -> node.color <- color;
  | Leaf -> invalid_arg "Can not set leaf color"

let swap_color node1 node2 =
  let c1 = color node1
  and c2 = color node2 in
  set_color node2 c1;
  set_color node1 c2;
  ()

let is_red node =
  phys_equal (color node) Red

let is_black node =
  phys_equal (color node) Black

let swap_value node1 node2 = match node1, node2 with
  | Node n1, Node n2 ->
    let v = n1.value in
    n1.value <- n2.value;
    n2.value <- v
  | _ -> invalid_arg "2 nodes must both be Node!"

let rotate node child =
  let branch = get_branch child node in
  let opposite = flip branch in
  let p = parent node in
  let grandson = child_node child opposite in
  set_child node grandson branch;
  set_child child node opposite;
  (* if is_leaf grandson |> not then
   *   set_parent grandson node;
   * set_parent node child; *)
  set_parent child p ~branch:(get_branch node p);
  (* (if not (is_leaf p) then
   *   get_branch node p |> set_child p child); *)
  child

let tranverse {contents=root} f =
  let rec tranverse' node parent direction =
    match node with
    | Node n ->
      (match f(n.value) with
      | Left -> tranverse' n.left node Left
      | Right -> tranverse' n.right node Right
      | Stop -> node, Stop)
    | Leaf -> parent, direction in
  tranverse' root root Stop

let find tree value =
  tranverse tree (fun v -> if Poly.(=) v value then Stop
                   else if v>value then Left
                   else Right)

let size tree =
  let rec size' node = match node with
    | Node n -> size' n.left + size' n.right + 1
    | Leaf -> 0 in
  size' !tree

let make_node ?(parent=Leaf) ?(color=Red) value =
  Node { value; color; left=Leaf; right=Leaf; parent=parent}

let rec adjust_insert' node =
  let p = parent node in
  if Poly.(=) (color p) Black then p
  else if is_root p then (
    set_color p Black;
    p)
  else
    let g = parent p
    and uncle = sibling p in
    if Poly.(=) (color uncle) Black then
      let p' = if not (phys_equal (get_branch p g) (get_branch node p)) then
          rotate p node
        else p in
      let p'' = rotate g p' in
      (* set_color p'' Black; *)
      (* set_color g Red; *)
      swap_color p'' g;
      p''
    else (
      set_color p Black;
      set_color uncle Black;
      set_color g Red;
      if is_root g then g
      else adjust_insert' g)

let rec adjust_insert node p =
  if is_black p then p
  else if is_root p then (
    if is_red p then
      set_color p Black;
    p)
  else
    let g = parent p
    and u = sibling p in
    (* printf "%d, %d, %d\n" (value p) (value g) (value u); *)
    match color p, color g, color u with
    | Red, Black, Red ->
      swap_color p g;
      set_color u Black;
      if is_root g then g
      else adjust_insert g (parent g)
    | Red, Black, Black ->
      let p' = match get_branch p g, get_branch node p with
        | b1, b2 when (phys_equal b1 b2) -> p
        | _ -> rotate p node in
      let p'' = rotate g p' in
      swap_color p'' g;
      p''
    | c1, c2, c3 ->
      Printf.sprintf "%s,%s,%s" (color_repr c1) (color_repr c2) (color_repr c3) |> invalid_arg

let insert tree elem = match find tree elem with
  | Leaf, Stop ->
    (* empty tree *)
    let root = make_node ~color:Black elem in
    set_parent root Leaf ~branch:Stop;
    tree := root;
    ()
  | Node n, Stop -> ()
  | (Node n) as node, d ->
    let newnode = make_node ~parent:node ~color:Red elem in
    (match d with
    | Left -> n.left <- newnode
    | _ -> n.right <- newnode);
    let node' = adjust_insert newnode node in
    if is_root node' && not (phys_equal !tree node') then
      tree := node';
  | _ -> invalid_arg "Leaf Left|Right can not happen here"

let nearest_child node =
  let rec nearest child direction =
    match child_node child direction with
    | Leaf -> child
    | _ as child' -> nearest child' direction in
  match child_node node Left with
  | (Node _) as node' -> nearest node' Right
  | Leaf -> match child_node node Right with
    | (Node _) as node' -> nearest node' Left
    | Leaf -> Leaf

let rec remove_black_end p b top =
  (* if is_leaf p then p *)
  (* else *)
  let s = child_node p (flip b) in
  let d = child_node s (flip b)
  and c = child_node s b in
  match color p, color s, color c, color d with
  | Black, Black, Black, Black ->
    set_color s Red;
    if is_root p then p
    else
      let pp = parent p in
      let top' = if is_root pp then pp else top in
      remove_black_end pp (get_branch p pp) top'
  | Black, Red, Black, Black ->
    rotate p s |> ignore;
    swap_color p s;
    let top' = if is_root s then s else top in
    remove_black_end p b top'
  | Red, Black, Black, Black ->
    swap_color p s;
    if is_root p then p else top
  | _, Black, Red, Black ->
    rotate s c |> ignore;
    swap_color s c;
    remove_black_end p b top
  | _, Black, _, Red ->
    rotate p s |> ignore;
    swap_color p s;
    set_color d Black;
    if is_root s then s else top
  | _ -> invalid_arg ""

let rec remove_node node =
  match left node, right node with
  | Node _, Node _ ->
    let nearest = nearest_child node in
    swap_value nearest node;
    remove_node nearest
  | ((Node _) as child), Leaf | Leaf, ((Node _) as child)  ->
    let p = parent node in
    set_parent child p ~branch:(get_branch node p);
    set_color child Black;
    (* if not (is_leaf p) then
     *   get_branch node p |> set_child p child; *)
    p
  | Leaf, Leaf -> let p = parent node in
    if is_leaf p then p
    else match color node with
      | Red ->
          get_branch node p |> set_child p Leaf;
        p
      | Black -> (
          let b = get_branch node p in
          set_child p Leaf b;
          remove_black_end p b p;
        )

let remove tree elem = match find tree elem with
  | (Node n) as nd, Stop ->
    let node = remove_node nd in
    (* (if (is_root node) && (left node |> is_leaf) && (right node |> is_leaf) then
     *   tree := Leaf); *)
    if (is_leaf node || is_root node) && phys_equal !tree node |> not then
      tree := node
  | _ -> ()
