open OUnit2
open Interval

(** [string_of_float_pair] converts a pair of floats to a string representation.
    Example: [string_of_float_pair (1., 1.)] is ["(1., 1.)"]. *)
let string_of_float_pair (f1, f2) =
  "(" ^ string_of_float f1 ^ ", " ^ string_of_float f2 ^ ")"

(** [assert_pair_equal] is a helper function for constructing tests on
    intervals. The [expected] argument is a pair of floats representing the
    expected lower and upper bounds of an interval. The function asserts that
    [Interval.lo] and [Interval.hi], when applied to the [intv] argument,
    produce the expected bounds. *)
let assert_pair_equal expected intv =
  assert_equal expected (lo intv, hi intv) ~printer:string_of_float_pair

let tests =
  [
    ("contains" >:: fun _ -> assert_equal true (make 0. 1. |> contains 0.));
    ("make" >:: fun _ -> assert_pair_equal (0., 1.) (make 0. 1.));
    ("add" >:: fun _ -> assert_pair_equal (1., 2.) (make 0. 1. + make 1. 1.));
    ("sub" >:: fun _ -> assert_pair_equal (-4., -2.) (make 1. 2. - make 4. 5.));
    ( "mult_positive" >:: fun _ ->
      assert_pair_equal (30., 200.) (make 5. 10. * make 6. 20.) );
    ( "mult_negative" >:: fun _ ->
      assert_pair_equal (-100., 200.) (make (-5.) 10. * make 6. 20.) );
    ( "div_normal" >:: fun _ ->
      assert_pair_equal (0.25, 2.) (make 1. 4. / make 2. 4.) );
    ( "div_low0" >:: fun _ ->
      assert_pair_equal (0.25, infinity) (make 1. 4. / make 0. 4.) );
    ( "div_high0" >:: fun _ ->
      assert_pair_equal (neg_infinity, -0.5) (make 1. 4. / make (-2.) 0.) );
    ( "div_mid0" >:: fun _ ->
      assert_pair_equal (neg_infinity, infinity) (make 1. 4. / make (-2.) 2.) );
  ]

let test_suite = "interval test suite" >::: tests
let _ = run_test_tt_main test_suite
