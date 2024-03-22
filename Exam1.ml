(* take and drop *)
let rec take = function
  | n, [] -> []
  | n, h :: t -> if n <= 0 then [] else h :: take (n - 1, t)

let rec drop = function
  | n, [] -> []
  | n, h :: t -> if n <= 0 then h :: t else drop (n - 1, t)

(* tail recursive version *)
let rec rev lst acc =
  match lst with
  | [] -> acc
  | h :: t -> rev t (h :: acc)

let take_tail n lst =
  let rec helper n lst acc =
    match (n, lst) with
    | x, [] -> acc
    | x, h :: t -> if x <= 0 then acc else helper (x - 1) t (h :: acc)
  in
  rev (helper n lst []) []

(* drop is already tail recursive *)
