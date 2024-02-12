(* records *)
type job =
  | Manager
  | Engineer
  | Janitor

type worker = {
  name : string;
  age : int;
  job : job;
}

let x = { name = "John"; age = 52; job = Manager };;

match x with
| { name; age; job } -> print_string name
(* this is syntactic sugar for {name = name; age = age; job = job} *)

(* copying a record *)
let y = { x with age = 42; job = Engineer }
(* creates a copy of x with age and job changed *)

(* tuples *)
let tuple1 = (1, 3, 5);;

(* note that [;;] means [in] the rest of the code *)

match tuple1 with
| x, y, z -> print_int x

(* synonym tuple definition *)
type strangeTupleType = int * string * float
(* tuple of an int, a string, and then a float *)
