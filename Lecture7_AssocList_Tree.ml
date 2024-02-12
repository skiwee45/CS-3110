open List

(* association list is a map, implemented as a list of pairs *)
let name_to_age = [ ("John", 5); ("Barry", 16); ("Tom", 11) ]

(* note that [;] is list separator while [,] is tuple separator *)

let insert k v map = (k, v) :: map
(* [`x] means arbitrary type *)

(* standard library functions *)
let john_age = List.assoc_opt "John" name_to_age
let dict_without_barry = List.remove_assoc "Barry" name_to_age

(* trees *)
type 'a tree =
  | Leaf
  | Node of 'a node

and 'a node = {
  (* the and allows node definition to be after tree and for tree to still use
     it *)
  value : 'a;
  left : 'a tree;
  right : 'a tree;
}

let preorder_lin t =
  let rec pre_acc acc = function
    (* [pre_acc] takes a accumulated list and a tree *)
    | Leaf -> acc
    | Node { value; left; right } -> value :: pre_acc (pre_acc acc right) left
  in
  pre_acc [] t
