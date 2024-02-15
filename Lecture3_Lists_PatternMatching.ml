(* list are immutable and singly linked *)
let list1 = [ 1; 2; 3; 4 ]

(* [::] is called cons, it prepends to the list *)
let list2 = 0 :: list1

(* called "nil" *)
let emptyList = []

(* is type int list list *)
let specialList = [ [ 5 ]; [ 6 ] ]

(* [@] is append lists. It prepends list1 to list2 in linear time *)
let largeList = list1 @ list2

let rec rev lst acc =
  match lst with
  | [] -> acc
  | h :: t -> rev t (h :: acc)

let rec append_helper lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> append_helper t (h :: lst2)

let rec append lst1 lst2 =
  let rev_lst1 = rev lst1 [] in
  append_helper rev_lst1 lst2

(* [list] by itself is not a type, it is a type constructor, [int list] is a
   type *)
type specialList = int list

(* tuple, only pairs have methods fst and snd *)
let pair = (5, 6)
let first = fst pair
let second = snd pair

(* pattern matching *)
(* function isEmpty *)
let isEmpty list =
  match list with
  | [] -> true
  | _ -> false
(* [] matches if list is [] *)
(* _ matches anything, good for catching edge cases *)
