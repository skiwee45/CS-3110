(** @author Raymond Lin (rpl67) *)

(** [convert_unit_helper] is [value1] converted from [unit1] to [unit2]. If the
    conversion is not possible, the result is -1. [unit1] and [unit2] must be
    unique and compatible (of the same type) *)
let convert_unit_helper value unit1 unit2 =
  match (value, unit1, unit2) with
  | _, "seconds", "minutes" -> value /. 60.
  | _, "minutes", "seconds" -> value *. 60.
  | _, "feet", "meters" -> value /. 3.281
  | _, "meters", "feet" -> value *. 3.281
  | _, u1, u2 -> if u1 = u2 then value else -1.

(** [convert_unit] is a sentence equating [value1] in [unit1] to the equivalent
    value in [unit2]. If the conversion is not possible, it is an error string *)
let convert_unit value1 unit1 unit2 =
  let value2 = convert_unit_helper value1 unit1 unit2 in
  if value2 = -1. then
    unit1 ^ " and " ^ unit2 ^ " are not units of the same kind"
  else
    "" ^ string_of_float value1 ^ " " ^ unit1 ^ " = " ^ string_of_float value2
    ^ " " ^ unit2

let test_convert_unit =
  let () =
    assert (convert_unit 1. "feet" "meters" = "1. feet = 0.304785126486 meters")
  in
  let () =
    assert (convert_unit 1. "meters" "feet" = "1. meters = 3.281 feet")
  in
  let () =
    assert (
      convert_unit 15.55 "minutes" "seconds" = "15.55 minutes = 933. seconds")
  in
  let () =
    assert (convert_unit 90. "seconds" "minutes" = "90. seconds = 1.5 minutes")
  in
  let () =
    assert (convert_unit 90. "seconds" "seconds" = "90. seconds = 90. seconds")
  in
  ()

(** Adapted from
    https://www.geeksforgeeks.org/euclidean-algorithms-basic-and-extended/,
    accessed 2/4/24. [greatest_common_denominator] is the greatest common
    denominator of [int1] and [int2] *)
let rec greatest_common_denominator int1 int2 =
  if int1 = 0 then int2 else greatest_common_denominator (int2 mod int1) int1

(** [fraction_from_float] is the fraction approximation of [value] where [value]
    is truncated to the hundredths place *)
let fraction_from_float value =
  let numerator = truncate (value *. 100.) in
  let gcd = greatest_common_denominator numerator 100 in
  (numerator / gcd, 100 / gcd)

let test_fraction_from_float =
  let () = assert (fraction_from_float 1.1 = (11, 10)) in
  let () = assert (fraction_from_float (-5.) = (-5, 1)) in
  let () = assert (fraction_from_float 0.333333 = (33, 100)) in
  ()
