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
