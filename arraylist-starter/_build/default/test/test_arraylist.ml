open OUnit2
open Arraylist

(***************************************************************************)
(* Helper functions *)
(***************************************************************************)

(** [get_oob_test] is a test that [get] correctly raises [OutOfBounds] when
    applied to an arraylist of size [size] and an index [attempted]. *)
let get_oob_test attempted size _ =
  assert_equal
    (Some (OutOfBounds { attempted; size }))
    (try
       let al = make () in
       for _ = 1 to size do
         add al 0
       done;
       ignore (get al attempted);
       None
     with
    | e -> Some e)

(** [grow_test] is a test that adding [n] elements to an empty array creates the
    right array. Specifically, passing in large enough [n] will cause the array
    to grow. The test checks that all the elements are in the right place after
    all the growth, and that the size is correct. *)
let grow_test n _ =
  assert_equal n
    (let al = make () in
     for i = 0 to n - 1 do
       add al i
     done;
     for i = 0 to n - 1 do
       assert (get al i = i)
     done;
     size al)

(***************************************************************************)
(* The test suite *)
(***************************************************************************)

let tests =
  [
    ("make; size 0" >:: fun _ -> assert_equal 0 (size (make ())));
    ( "add; size" >:: fun _ ->
      assert_equal 1
        (let al = make () in
         add al 42;
         size al) );
    ( "add; get" >:: fun _ ->
      assert_equal 42
        (let al = make () in
         add al 42;
         get al 0) );
    "get too small" >:: get_oob_test ~-1 1;
    "get too big" >:: get_oob_test 2 1;
    ( "add; set; get" >:: fun _ ->
      assert_equal 42
        (let al = make () in
         add al 0;
         set al 0 42;
         get al 0) );
    "grow once; get all; size" >:: grow_test 15;
    "grow many; get all; size" >:: grow_test 1000;
  ]

let test_suite = "arraylist test suite" >::: tests
let _ = run_test_tt_main test_suite
