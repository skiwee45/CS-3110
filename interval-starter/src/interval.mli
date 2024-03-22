(** An interval represents a range of possible floating-point numbers. *)

type t
(** The type of intervals. *)

val make : float -> float -> t
(** [make lo hi] is an interval representing a floating-point number x such that
    [lo <= x <= hi]. If [lo > hi] then the interval will be empty, meaning that
    it does not contain any floating-point numbers. *)

val contains : float -> t -> bool
(** [contains x intv  is whether [x] is in the interval [intv]. *)

val lo : t -> float
(** [lo intv] is the lower bound of interval [intv]. *)

val hi : t -> float
(** [hi intv] is the upper bound of interval [intv]. *)

val ( + ) : t -> t -> t
(** [intv1 + intv2] is the addition of intervals [intv1] and [intv2]. *)

val ( - ) : t -> t -> t
(** [intv1 - intv2] is the subtraction of intervals [intv1] and [intv2]. *)

val ( * ) : t -> t -> t
(** [intv1 * intv2] is the multiplication of intervals [intv1] and [intv2]. *)

val ( / ) : t -> t -> t
(** [intv1 / intv2] is the division of intervals [intv1] and [intv2]. *)
