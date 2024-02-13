(* first problem, implementing list with length value *)
type 'a node = {
  value : 'a;
  next : 'a node option;
}

type 'a lenlist =
  | Nil
  | Cons of {
      head : 'a node;
      length : int;
    }

let length = function
  | Nil -> 0
  | Cons { length } -> length

let empty_list = Nil

let cons h = function
  | Nil -> Cons { head = { value = h; next = None }; length = 1 }
  | Cons { head; length } ->
      Cons { head = { value = h; next = Some head }; length = length + 1 }

(* problem two, folding *)
let product lst = List.fold_left ( * ) 1 lst
let append lst1 lst2 = List.fold_right (fun x l -> x :: l) lst2 lst1
let rev lst : 'a list = List.fold_left (fun l x -> x :: l) [] lst
let map func lst = List.fold_right (fun x l -> func x :: l) [] lst
let rev_map func lst = List.fold_left (fun l x -> func x :: l) [] lst

(* unfolding *)
let rec unfold f seed =
  match f seed with
  | None -> []
  | Some (x, seed') -> x :: unfold f seed'

let range lo hi =
  unfold (fun curr -> if curr > hi then None else Some (curr, curr + 1)) lo

let fact n = product (range 1 n)

let fib n =
  let step (n1, n2, i) =
    if i > n then None else Some (n1, (n2, n1 + n2, i + 1))
  in
  unfold step (0, 1, 0)
