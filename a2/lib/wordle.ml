open ANSITerminal
open List
open Batteries

(** reads from file [filename.txt] in the data folder, [data/filename.txt] must
    exist *)
let get_dictionary filename =
  BatList.of_enum (BatFile.lines_of ("data/" ^ filename ^ ".txt"))

(** initializes random number generator *)
let initialize_random () = Random.self_init ()

(** gets the [i]th string from a list of strings, [i] must be within range of
    the list *)
let rec get_word_by_index i = function
  | [] -> failwith "index out of range"
  | h :: t -> if i = 0 then h else get_word_by_index (i - 1) t

let%test "get first word out of ['dog'; 'cat']" =
  get_word_by_index 0 [ "dog"; "cat" ] = "dog"

(** gets random string from [dictionary], which is a list of strings.
    [dictionary] must not be empty *)
let get_random_word dictionary =
  get_word_by_index (Random.int (length dictionary)) dictionary

(** checks if [input] exists within a list of strings *)
let rec check_input input = function
  | [] -> false
  | h :: t -> if input = h then true else check_input input t

let%test "input is not in list" = not (check_input "parrot" [ "dog"; "cat" ])
let%test "input is in list" = check_input "dog" [ "dog"; "cat" ]

(** check correct position letters in [input_map] according to [correct] and
    return a dictionary with those letters assigned the value [on_green] *)
let rec map_green_letters input_map correct =
  match (input_map, correct) with
  | (letter, orig_color) :: t, cletter :: ct ->
      if letter = cletter then (letter, on_green) :: map_green_letters t ct
      else (letter, orig_color) :: map_green_letters t ct
  | _ -> []

(** check for letters in [input_map] that already have [on_green] values and
    return a frequency map that takes those already-marked letters into account *)
let rec update_freqmap input_map freqmap =
  match (input_map, freqmap) with
  | (letter, color) :: t, _ ->
      if color = on_green then
        let newMap =
          BatList.modify_def 1 letter (fun prev -> prev - 1) freqmap
        in
        update_freqmap t newMap
      else update_freqmap t freqmap
  | _ -> freqmap

(** true if [char] appears in [freqmap] with a frequency of more than 1 *)
let rec yellow_helper char freqmap =
  match freqmap with
  | [] -> false
  | (c, _) :: t ->
      if char = c && assoc char freqmap > 0 then true else yellow_helper char t

(** check correct out-of-position letters in [input_map] according to [freqmap]
    and return a dictionary with those letters assigned the value [on_yellow] *)
let rec map_yellow_letters input_map freqmap =
  match (input_map, freqmap) with
  | (letter, orig_color) :: t, _ ->
      if yellow_helper letter freqmap && orig_color <> on_green then
        let new_freqmap =
          BatList.modify letter (fun freq -> freq - 1) freqmap
        in
        (letter, on_yellow) :: map_yellow_letters t new_freqmap
      else (letter, orig_color) :: map_yellow_letters t freqmap
  | _ -> []

(** map each char in [word] to [default_value], [i] must be less than 5 *)
let rec create_input_map word default_value i =
  match (word, default_value, i) with
  | _, _, 5 -> []
  | _ -> (word.[i], default_value) :: create_input_map word default_value (i + 1)

let%test "mapping of characters in 'fools' to on_black" =
  create_input_map "fools" on_black 0
  = [
      ('f', on_black);
      ('o', on_black);
      ('o', on_black);
      ('l', on_black);
      ('s', on_black);
    ]

(** create char list from [str], [i] must be less than 5 *)
let rec charlist_from_string str i =
  match (str, i) with
  | _, 5 -> []
  | _ -> str.[i] :: charlist_from_string str (i + 1)

let%test "char list of 'fools'" =
  charlist_from_string "fools" 0 = [ 'f'; 'o'; 'o'; 'l'; 's' ]

(** create letter frequency map from [str], [i] must be less than 5, [map] is
    the existing frequencies of letters *)
let rec freqmap_from_string str i map =
  match i with
  | 5 -> map
  | _ ->
      let newMap = BatList.modify_def 0 str.[i] (fun prev -> prev + 1) map in
      freqmap_from_string str (i + 1) newMap

let%test "frequency map of 'fools'" =
  freqmap_from_string "fools" 0 [] = [ ('f', 1); ('o', 2); ('l', 1); ('s', 1) ]
