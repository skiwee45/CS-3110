(* WARNING: This file contains deliberately injected faults. Some functions are
   implemented incorrectly. *)

type t = float * float

let make lo hi = (lo, hi)
let contains x (lo, hi) = lo <= x && x <= hi
let lo (lo, _) = lo
let hi (_, hi) = hi
let ( + ) (lo1, hi1) (lo2, hi2) = (lo1 +. lo2, hi1 +. hi2)
let ( - ) (lo1, hi1) (lo2, hi2) = (lo1 -. hi2, hi1 -. lo2)

(** [list_min lst] is the minimum element of [lst]. Requires: [lst] is not
    empty. *)
let list_min lst = List.(fold_left min (hd lst) (tl lst))

(** [list_max lst] is the maximum element of [lst]. Requires: [lst] is not
    empty. *)
let list_max lst = List.(fold_left max (hd lst) (tl lst))

let ( * ) (lo1, hi1) (lo2, hi2) =
  if lo1 >= 0. && lo2 >= 0. then (lo1 *. lo2, hi1 *. hi2)
  else
    let extremes = [ lo1 *. lo2; lo1 *. hi2; hi1 *. lo2; hi1 *. hi2 ] in
    (list_min extremes, list_max extremes)

(** [inv intv] is the multiplicative inverse of interval [intv]. *)
let inv (lo, hi) =
  if not (contains 0. (lo, hi)) then (1. /. hi, 1. /. lo)
  else if lo = 0. then (1. /. hi, infinity)
  else if hi = 0. then (neg_infinity, 1. /. lo)
  else (neg_infinity, infinity)

let ( / ) intv1 intv2 = intv1 * inv intv2
