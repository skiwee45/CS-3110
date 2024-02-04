(* let definition *)
let x = 4;;
(* this is not an expression by itself, it just put x = 4 in memory, doesn't evaluate to anything *)

(* let expression *)
let y = 5 in y + 1;;
(* binds y to 5 in the expression "y + 1" and then evaluates that body expression *)

(* let definition in reality *)
let x = 5;;
(* is "let x = 5 in" where the implicit scope is the rest of the class *)

(* functions *)
(* functions are by themselves a value of type <fun> *)
(* anonymous function *)
(fun x -> x + 1) 5;; (* = 6 *)

(* multiple argument functions and function definitions *)
let add x y = x + y;;
(* shorthand for *)
let add = fun x y -> x + y;;

(* in reality, multiple argument functions don't exist *)
let add x y = x + y;;
(* shorthand for *)
let add = fun x -> fun y -> x + y;;
(* essentially there are only 1 argument functions, they just implicitly chain *)
(* (fun y -> x + y) evaluates to a function of type int that takes one argument of type int *)