open Core
open Rbtree

(* let rec print_node = function
 *   | Node node -> *)

let unique_list l =
  let rec unique l acc = match l with
    | [] -> acc
    | x::xs -> unique (List.filter ~f:(fun e -> e <> x) xs) (x::acc)
  in unique l [] |> List.rev

let shuffle l =
  let l' = List.map ~f:(fun e -> (Random.bits (), e)) l in
  let l'' = List.sort ~compare: (fun (x, _) (y, _) -> y - x) l' in
  List.map ~f:snd l''

let from_list list =
  let tree = empty() in
  List.iter list ~f:(fun e -> insert tree e);
  tree

let to_list tree =
  let rec to_list' = function
    | Node node ->
      List.append (node.value :: to_list' node.left) (to_list' node.right)
    | _ -> []
in
to_list' !tree

let to_list_sorted tree =
  let rec to_list' = function
    | Node node ->
      List.append (to_list' node.left) (node.value :: to_list' node.right)
    | _ -> []
  in
to_list' !tree

let print_list l =
  List.iter ~f:(printf "%d;") l;
  printf("\n")

let check_insert = let open! QCheck in
  Test.make ~count:100 ~name:"insert" (list small_nat)
    (fun list ->
       (* let sorted = unique_list list in *)
       (* let list' = shuffle sorted in
        * printf "shuffled: ";
        * print_list list'; *)
       let tree = from_list list in
       Poly.(=) (to_list_sorted tree) (List.dedup_and_sort ~compare list)
    )

let check_size = let open! QCheck in
  Test.make ~count:100 ~name:"size" (list small_nat)
    (fun list ->
       (* let list' = unique_list list in *)
       let tree = from_list list in
       Poly.(=) (size tree) (List.dedup_and_sort ~compare list |> List.length)
    )

let check_remove = let open! QCheck in
  Test.make ~count:100 ~name:"remove" (list small_int)
    (fun list ->
       (* let list' = unique_list list in *)
       (* List.iter list' ~f:(Printf.printf "%d, "); *)
       (* Printf.printf "\n("; *)
       let tree = from_list list in
       List.iter (shuffle list) ~f:(fun e -> remove tree e);
       (* Printf.printf ")\n"; *)
       size tree = 0
    )

let () =
  let suite = [ QCheck_alcotest.to_alcotest check_insert;
                QCheck_alcotest.to_alcotest check_size;
                QCheck_alcotest.to_alcotest check_remove;
              ] in
  Alcotest.run "RBTree test" ["suite", suite]
