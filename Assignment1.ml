(** @author Raymond Lin (rpl67) *)

(** [convert_unit] is [value1] converted from [unit1] to [unit2]. If the
    conversion is not possible, the result is -1. [unit1] and [unit2] must be
    unique and compatible (of the same type) *)
let convert_unit value unit1 unit2 =
  if unit1 = "seconds" && unit2 = "minutes" then value /. 60.
  else if unit1 = "minutes" && unit2 = "seconds" then value *. 60.
  else if unit1 = "feet" && unit2 = "meters" then value /. 3.281
  else if unit1 = "meters" && unit2 = "feet" then value *. 3.281
  else -1.

(** [get_conversion_string] is a sentence equating [value1] in [unit1] to the
    equivalent value in [unit2]. If the conversion is not possible, it is an
    error string *)
let get_conversion_string value1 unit1 unit2 =
  let value2 = convert_unit value1 unit1 unit2 in
  if value2 = -1. then
    unit1 ^ " and " ^ unit2 ^ " are not units of the same kind"
  else
    "" ^ string_of_float value1 ^ " " ^ unit1 ^ " = " ^ string_of_float value2
    ^ " " ^ unit2
