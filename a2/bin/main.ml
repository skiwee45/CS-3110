open A2.Wordle

(** ask user for input and return that input *)
let prompt_and_input () =
  let () = print_string "> " in
  read_line ()

(** prints word from char -> color mapping that has each letter of the word in
    the corresponding color value *)
let rec print_colored_word = function
  | [] -> print_string ""
  | (char, color) :: t ->
      ANSITerminal.print_string
        [ ANSITerminal.white; color ]
        (String.make 1 char);
      print_colored_word t

(** prints the [input] with colors corresponding to wordle rules and the
    [correct] word. [input] and [correct] must both be 5 letters long *)
let output_feedback input correct =
  let input_map = create_input_map input ANSITerminal.on_black 0 in
  let correct_list = charlist_from_string correct 0 in
  let green_map = map_green_letters input_map correct_list in
  let freqmap = update_freqmap green_map (freqmap_from_string correct 0 []) in
  let colored_map = map_yellow_letters green_map freqmap in
  print_colored_word colored_map

(** prints a list of [(input, correct)] corresponding to wordle rules, [input]
    and [correct] must both be 5 letters long *)
let rec multi_output = function
  | [] -> ()
  | (input, correct) :: t ->
      output_feedback input correct;
      print_endline "";
      multi_output t

(* main *)
let dictionary = get_dictionary "dictionary"
let () = initialize_random ()
let correct_word = get_random_word dictionary
let () = print_endline ("Correct word is: " ^ correct_word)

(** runs the game, repeats until iterator is 0, if the input is invalid, repeats
    without decerasing iterator, prints feedback based on [output_list] which
    should contain all previous guesses *)
let rec game_loop output_list = function
  | 0 -> print_endline "You Lost :("
  | turns ->
      let input = prompt_and_input () in
      if not (check_input input dictionary) then
        let () = print_endline "Invalid Input, Try Again" in
        game_loop output_list turns
      else
        let new_output_list = output_list @ [ (input, correct_word) ] in
        let _f = multi_output new_output_list in
        print_endline "";
        if input = correct_word then print_endline "You Won!"
        else game_loop new_output_list (turns - 1)

let () = game_loop [] 6
