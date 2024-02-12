(* option is like maybe, it can be null or have a value *)
let first_in_list = function
  | [] -> None
  | head :: t -> Some head
(* this returns an option of type [a'] (whatever type the list is) *)

(* extract from option *)
let extract = function
  | None -> 0
  | Some x -> x
