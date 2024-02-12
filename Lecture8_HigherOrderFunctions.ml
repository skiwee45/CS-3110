(* the whole idea is to treat functions like variables, functions can take
   functions as input and output *)
let nest func1 func2 input = func1 (func2 input)
let double x = x * 2
let increment x = x + 1

(* remember that not inputting all arguments results in a method returning
   another method with some inputs filled in "partial application" *)
let increment_then_double = nest double increment

(* this is actually shorthand for *)
let increment_then_double x = nest double increment x
let y = increment_then_double 4 (* 10 *)

(* map function: returns list with each variable ran through function *)
let clamp min max x : int = if x > max then max else if x < min then min else x

let rec map func list =
  match list with
  | [] -> []
  | h :: t -> func h :: map func t

let clamp_list min max = map (clamp min max)

(* using library function *)
let clamp_list min max = List.map (clamp min max)

(* filter function: returns filtered list *)
let divisible_by divisor x = x mod divisor = 0

let rec filter func list =
  match list with
  | [] -> []
  | h :: t -> if func h then h :: filter func t else filter func t

let mod_filter divisor list = filter (divisible_by divisor) list

(* using library function *)
let mod_filter divisor list = List.filter (divisible_by divisor) list

(* fold function: combines list into one value *)
let add = ( + )

(* this function goes all the way down first, then sums on the way up, therefore
   it sums the right-most element first *)
let rec fold_right func list default_value =
  match list with
  | [] -> default_value
  | h :: t -> func h (fold_right func t default_value)

let sum list = fold_right add list 0

(* this function sums as it goes, therefore it is tail recursive and sums left
   to right, using the accumulator to keep track on the way down *)
let rec fold_left func list acc =
  match list with
  | [] -> acc
  | h :: t -> fold_left func t (func acc h)

let sum list = fold_left add list 0

(* beware that [fold_right] and [fold_left] don't behave the same for
   subtraction or other non-commutative functions *)
