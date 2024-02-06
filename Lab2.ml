let rec last d = function
  | [ h ] -> h
  | h :: t -> last d t
  | [] -> d

let rec contains x p =
  match p with
  | u, _ when u = x -> true
  | _, v when v = x -> true
  | _ -> false

(* use when *)

let rec even n =
  match n mod 2 with
  | 0 -> true
  | _ -> false

let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | x -> fib (x - 1) + fib (x - 2)

let fib_step (n1, n2) = (n2, n1 + n2)

let rec fib_loop n (x1, x2) =
  match (n, (x1, x2)) with
  | 0, _ -> x2
  | n, (x1, x2) -> fib_loop (n - 1) (fib_step (x1, x2))

let fibi = function
  | 0 -> 0
  | n -> fib_loop (n - 1) (0, 1)
