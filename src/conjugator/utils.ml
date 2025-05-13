exception Todo of string
let todo msg = raise (Todo msg)

let preformative_pos = 1
let coordinator_pos = 2
let first_prefix_pos = 0
let ventive_pos = 3
let middle_prefix_pos = 4
let initial_person_prefix_pos = 5
let indirect_object_prefix_pos = 6
let comitative_pos = 7
let adverbial_pos = 8
let locative_pos = 9
let final_person_prefix_pos = 10
let stem_pos = 11
let ed_marker_pos = 12
let final_person_suffix_pos = 13
let subordinator_pos = 14

type markerName = 
  | FirstPrefix
  | Preformative
  | Coordinator
  | Ventive
  | MiddlePrefix
  | InitialPersonPrefix
  | IndirectObjectPrefix
  | Comitative
  | Adverbial
  | Locative
  | FinalPersonPrefix
  | Stem
  | EdMarker
  | FinalPersonSuffix
  | Subordinator

let marker_by_pos (pos: int): markerName option =
    match pos with
        | 0 -> Some(FirstPrefix)
        | 1 -> Some(Preformative)
        | 2 -> Some(Coordinator)
        | 3 -> Some(Ventive)
        | 4 -> Some(MiddlePrefix)
        | 5 -> Some(InitialPersonPrefix)
        | 6 -> Some(IndirectObjectPrefix)
        | 7 -> Some(Comitative)
        | 8 -> Some(Adverbial)
        | 9 -> Some(Locative)
        | 10 -> Some(FinalPersonPrefix)
        | 11 -> Some(Stem)
        | 12 -> Some(EdMarker)
        | 13 -> Some(FinalPersonSuffix)
        | 14 -> Some(Subordinator)
        | _ -> None

let marker_to_pos (marker: markerName): int =
    match marker with 
    | FirstPrefix -> first_prefix_pos
    | Preformative -> preformative_pos
    | Coordinator -> coordinator_pos
    | Ventive -> ventive_pos
    | MiddlePrefix -> middle_prefix_pos
    | InitialPersonPrefix -> initial_person_prefix_pos
    | IndirectObjectPrefix -> indirect_object_prefix_pos
    | Comitative -> comitative_pos
    | Adverbial -> adverbial_pos
    | Locative -> locative_pos
    | FinalPersonPrefix -> final_person_prefix_pos
    | Stem -> stem_pos
    | EdMarker -> ed_marker_pos
    | FinalPersonSuffix -> final_person_suffix_pos
    | Subordinator -> subordinator_pos


let rec find_previous_morpheme (pos: int) (arr: string array): (string * markerName) option = 
    if (pos < 1)
    then None
    else
      try 
        let morph = arr.(pos - 1) in
        if String.length morph > 0
        then
          (match marker_by_pos(pos - 1) with
          | Some marker -> Some((morph, marker))
          | None -> None)
        else find_previous_morpheme (pos - 1) arr
      with
        (* | Invalid_argument _ -> find_previous_morpheme (pos - 1) arr *)
        | Invalid_argument _ -> None


let rec find_next_morpheme (pos: int) (arr: string array): (string * markerName) option = 
    if (pos > 14)
    then None
    else
      try 
        let morph = arr.(pos + 1) in
        if String.length morph > 0
        then
          (match marker_by_pos(pos + 1) with
          | Some marker -> Some((morph, marker))
          | None -> None)
        else None
      with
        | Invalid_argument _ -> find_next_morpheme (pos + 1) arr

let starts_with_vowel (str: string): bool =
    let firstChar = String.get str 0 |> String.make 1 in
    match firstChar with
        | "a" | "á" | "e" | "è" | "i" | "o" | "u" -> true
        | _ -> false

let ends_with_vowel (str: string): bool = 
    let vowels = ["a"; "á"; "e"; "è"; "i"; "o"; "u"] in
    try
      match String.length str with
        | 0 -> false
        | 1 -> List.mem str vowels
        | _ ->
          let last_char = String.get str (String.length str - 1) |> String.make 1
          in List.mem last_char vowels
    with
      | Invalid_argument _ -> false

let starts_with_consonant (str: string): bool =
    let firstChar = String.get str 0 |> String.make 1 in
    match firstChar with
        | "b" | "d" | "h" | "ḫ" | "g" | "k" | "l" | "m" | "n" | "p" | "r" | "s" | "š" 
        | "t" | "w" | "z" -> true
        | _ -> false

let remove_first_char (str: string): string = 
    if String.length str > 0
    then String.sub str 1 (String.length str - 1)
    else
      str

let consonant_vowel_sequence (str: string): string =
  str
  |> String.to_seq
  |> List.of_seq
  |> List.map (fun ch ->
      match String.make 1 ch with
      | "a" | "á" | "e" | "è" | "i" | "o" | "u" -> "V"
      | _ -> "C"
  )
  |> String.concat ""

let get_morpheme_at_pos (pos: int) (arr: string array) : string option =
    try
        let morph = arr.(pos) in
        if String.length morph > 0
        then Some(morph)
        else None
    with
        | Invalid_argument _ -> None