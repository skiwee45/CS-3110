(** Modularity features inside languages Namespaces: Structures in OCaml,
    Packages / Classes in Java Interfaces: Signatures in OCaml, Interfaces in
    Java Encapsulation: Signatures and Abstract types in OCaml, Public / Private
    in Java Code Reuse: Functors and includes in Ocaml, Subtyping and
    Inheritance in Java *)

(* sort of like a static class in Java, but not an instance class *)
(* modules are nestable *)
module MyList = struct
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

  let cons h = function
    | Nil -> Cons { head = { value = h; next = None }; length = 1 }
    | Cons { head; length } ->
        Cons { head = { value = h; next = Some head }; length = length + 1 }
end

(* this solves shadowing, gives hierarchical namespace *)
let empty_list = MyList.Nil

(* stack *)
(* could also use the OCaml list library, linked list is perfect for stack *)
module MyStack = struct
  type 'a t =
    | Empty
    | Entry of 'a * 'a t

  let empty = Empty
  let push x s = Entry (x, s)

  exception EmptyStack

  let peek = function
    | Empty -> raise EmptyStack
    | Entry (x, _) -> x

  (* only removes top element, doesn't return it *)
  let pop = function
    | Empty -> raise EmptyStack
    | Entry (_, s) -> s

  let is_empty = function
    | Empty -> true
    | _ -> false
end

module ListStack = struct
  type 'a t = 'a list

  let empty = []

  exception EmptyStack

  let push x s = x :: s

  let pop = function
    | [] -> raise EmptyStack
    | _ :: s -> s

  let peek = function
    | [] -> raise EmptyStack
    | x :: _ -> x

  let is_empty = function
    | [] -> true
    | _ -> false
end

(* Functional data structures are persistent (operations do not mutate
   structures, they return a new copy) *)

(* signatures *)
(* essentially an interface *)
module type Fact = sig
  val fact : int -> int
  (** [fact n] is [n] factorial *)
end

(* this module implements Fact *)
module MyMath : Fact = struct
  let rec fact n = if n = 0 then 1 else n * fact (n - 1)
  let other_function b = b
end

(* this doesn't work, because signatures encapsulate methods that aren't defined
   in the signature *)
(** [let b = MyMath.other_function 5] *)

module type Stack = sig
  type 'a t
  (* abstract data type, can be list or your own stack *)
  (* basically a generic type, could also define a concrete type here *)

  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t
  val peek : 'a t -> 'a
  val is_empty : 'a t -> bool
end

(* these are sealed because the internal implementation is no longer
   accessible *)
(* that is why empty is necessary, this is encapsulation *)
module ListStackSealed : Stack = ListStack

module MyStackSealed : Stack = MyStack
(** using t for abstract types is convention, write [MyStackSealed.t] *)

(* write documentation functions tin signature only, not implementation *)
