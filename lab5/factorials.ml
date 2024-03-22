module type Factorial = sig
  val fact : int -> int
end

module FactorialRecursive = struct
  let rec fact n = if n = 0 then 1 else n * fact (n - 1)
end

module FactorialIterative = struct
  let rec fact_aux n acc = if n = 0 then acc else fact_aux (n - 1) (n * acc)
  let fact n = fact_aux n 1
end

let recursive_tests =
  let open FactorialRecursive in
  [ fact 1 = 1; fact 3 = 6; fact 10 = 3628800 ]

let iterative_tests =
  let open FactorialIterative in
  [ fact 1 = 1; fact 3 = 6; fact 10 = 3628800 ]

module FactorialTester (F : Factorial) = struct
  let tests =
    let open F in
    [ fact 1 = 1; fact 3 = 6; fact 10 = 3628800; fact 5 = 120 ]
end

let run_tests1 =
  assert (
    List.for_all Fun.id (List.flatten [ recursive_tests; iterative_tests ]))

module FactorialRecursiveTester = FactorialTester (FactorialRecursive)
module FactorialIterativeTester = FactorialTester (FactorialIterative)

let run_tests2 =
  assert (
    List.for_all Fun.id
      (List.flatten
         [ FactorialIterativeTester.tests; FactorialRecursiveTester.tests ]))
