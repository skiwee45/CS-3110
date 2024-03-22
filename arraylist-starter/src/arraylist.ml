type 'a t = {
  mutable data : 'a option array;
  mutable size : int;
}
(* RI: Let [capacity] be [Array.length data]. It is required that:

   - [0 <= size <= capacity]. - The first [size] elements of [data] are [Some
   _], and the remaining [capacity - size] elements are [None].

   AF: If [data] is [[|Some e0; Some e1; ...; Some en; None; ...; None|]], it
   represents the list [[e0; e1; ... en]]. *)

let make () = { data = Array.make 10 None; size = 0 }
let size al = al.size

let add al elt =
  if al.size = Array.length al.data then
    al.data <- Array.append al.data (Array.make (Array.length al.data) None);
  al.data.(al.size) <- Some elt;
  al.size <- al.size + 1

exception
  OutOfBounds of {
    attempted : int;
    size : int;
  }

let get al idx =
  if idx < 0 || idx >= al.size then
    raise (OutOfBounds { attempted = idx; size = al.size })
  else
    match al.data.(idx) with
    | Some elt -> elt
    | None -> assert false

let set al idx elt =
  if idx < 0 || idx >= al.size then
    raise (OutOfBounds { attempted = idx; size = al.size })
  else al.data.(idx) <- Some elt
