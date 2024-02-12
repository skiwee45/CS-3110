let get_dictionary filename =
  BatList.of_enum (BatFile.lines_of ("data/" ^ filename ^ ".txt"))

let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_endline h;
      print_list t
