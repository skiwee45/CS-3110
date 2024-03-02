(* Should be iterative, but currently don't know how to do that *)
let rec range i j = if i > j then [] else i :: range (i + 1) j

(* tail call optimization to reduce stack *)
(* tail call means the recursive call is the last task to do in the method *)

(* the optimization is automatically detected, no keywords *)
let rec range_helper i j arr =
  if i > j then [] else range_helper i (j - 1) (j :: arr)

let range_tail_rec i j = range_helper i j []

(* variants *)
type color =
  | Red
  | Green
  | Blue

let pick_red color = if color = Red then true else false
let color1 = Red

(* constant variant definition, sort of like enum *)
(* no additional information after the constructor *)
let string_of_color = function
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"

(* synonym definition. float * float means tuple pair of float *)
type point = float * float

(* non-constant variant definition *)
(* each constructor carries data *)
type shape =
  | Point of point
  | Circle of {
      center : point;
      radius : float;
    }
(* inside a variant, records can be defined without extra work *)

(* construct a Point *)
let p = Point (45., 23.)

(* match can bind the input to variables *)
let center = function
  | Point (x, y) -> (x, y)
  | Circle { center = c } -> c

(* exercise 1 *)
type temperature =
  | Celcius of float
  | Fahrenheit of float

let kelvin = function
  | Celcius v -> v +. 273.15
  | Fahrenheit v -> (v +. 459.67) *. 5. /. 9.

let pick_celcius temp = if temp = Celcius 1. then true else false
