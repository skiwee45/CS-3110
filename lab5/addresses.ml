type address = {
  street_address : string;
  city : string;
  state : string;
}

module Address = struct
  type t = address

  let compare_string_case_insensitive s1 s2 =
    String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)

  let compare { street_address = street1; city = city1; state = state1 }
      { street_address = street2; city = city2; state = state2 } =
    match compare_string_case_insensitive state1 state2 with
    | 0 -> (
        match compare_string_case_insensitive city1 city2 with
        | 0 -> compare_string_case_insensitive street1 street2
        | c -> c)
    | c -> c
end

let gates =
  { street_address = "1 Microsoft Way"; city = "Redmond"; state = "WA" }

let jobs =
  { street_address = "1 Infinite Loop"; city = "Cupertino"; state = "CA" }

let cornell =
  { street_address = "410 Thurston Ave"; city = "Ithaca"; state = "NY" }

let empire_state =
  { street_address = "350 5th Ave"; city = "New York"; state = "NY" }

let central_park =
  { street_address = "14 E 60th St"; city = "New York"; state = "NY" }

module AddressSet = Set.Make (Address)

let addresses =
  AddressSet.of_list [ gates; jobs; cornell; empire_state; central_park ]

let ny_addresses =
  AddressSet.filter (fun a -> String.capitalize_ascii a.state = "NY") addresses
