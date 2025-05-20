include Infixes
(*
    Slot 1 Modal prefix (ḫa); negative particle; prefix of anteriority; stem (in imperative forms)
    Slot 2 Finite-marker prefix; modal prefixes (all the other)
    Slot 3 Coordinator prefix
    Slot 4 Ventive (cislocative) prefix
    Slot 5 Middle prefix or the 3.SG.NH pronominal prefix /b/ (specifying the person;
    gender and number of the first in the sequence of adverbial prefixes)
    Slot 6 Initial Pronominal Prefix (= IPP) (specifying the person; gender and number
    of the first in the sequence of adverbial prefixes)
    Slot 7 Adverbial I: dative prefix
    Slot 8 Adverbial II: comitative prefix
    Slot 9 Adverbial III: ablative or terminative prefix
    Slot 10 Adverbial IV: locative1; locative2; or locative3 prefix
    Slot 11 Final Pronominal Prefix (= FPP) (referring to A or P; depending on the tense;
    or locative3)
    Slot 12 stem
    Slot 13 present-future marker (in intransitive verbs)
    Slot 14 pronominal suffix (referring A; S; or P depending on the tense)
    Slot 15 subordinator
*)

type t = Constructs.conjugated_verb

type verb_output = string array

type ipfv_stem =
    | Reduplicate
    | Ed_marker
    | Other of string

let create (stem: string): t =
  {
    stem = stem;
    is_perfective = false;
    is_transitive = false;
    oblique_object = None;
    first_prefix = None;
    preformative = None;
    coordinator = false;
    ventive = false;
    middle_prefix = false;
    initial_person_prefix = None;
    indirect_object_prefix = None;
    comitative = false;
    adverbial = None;
    locative = None;
    final_person_prefix = None;
    ed_marker = false;
    final_person_suffix = None;
    subordinator = false;
    subject = None;
    object_ = None;
  }

let set_stem (verb: t) (stem: string): t = { verb with stem = stem }

let is_perfective verb: t = { verb with is_perfective = true }

let is_imperfective (verb: t) ipfv_stem: t =
    match ipfv_stem with
    | Some Reduplicate -> { verb with stem = verb.stem ^ "-" ^ verb.stem; is_perfective = false; ed_marker = false}
    | Some Ed_marker -> { verb with is_perfective = false; ed_marker = true}
    | Some Other stem -> { verb with is_perfective = false; stem = stem}
    | None -> { verb with is_perfective = false}

let is_transitive verb: t = { verb with is_transitive = true }

let is_intransitive verb: t = { verb with is_transitive = false }

let set_modal verb: t = { verb with first_prefix = Some FirstPrefix.Modal }

let reset_modal verb: t = { verb with first_prefix = None }

let set_modalGa verb: t = { verb with first_prefix = Some FirstPrefix.Modal_ga }

let reset_modal_ga = reset_modal

let set_negative verb: t = { verb with first_prefix = Some FirstPrefix.Negative}

let reset_negative = reset_modal

let set_negative_nan verb: t = { verb with first_prefix = Some FirstPrefix.Negative_nan }

let reset_negative_nan = reset_modal

let set_preformative verb preformative: t = { verb with preformative = Some preformative}

let reset_preformative verb: t = { verb with preformative = None}


let set_coordinator verb: t = { verb with coordinator = true}

let reset_coordinator verb: t = { verb with coordinator = false}

let set_ventive verb: t = { verb with ventive = true}

let reset_ventive verb: t = { verb with ventive = false}

let set_terminative verb (person: PersonParam.t option): t =
    let open InitialPersonPrefix in
    let ipp: InitialPersonPrefix.t option = match person with
        | Some First_sing -> Some First_sing
        | Some Second_sing -> Some Second_sing
        | Some Third_sing_human -> Some Third_sing_human
        | Some Third_sing_non_human -> Some Third_sing_non_human
        | Some First_plur -> Some First_plur
        | Some Second_plur -> Some Second_plur
        | Some Third_plur_human -> Some Third_plur_human
        | Some Third_plur_non_human -> Some Third_plur_non_human
        | None -> None
    in { verb with adverbial = Some Terminative; initial_person_prefix = ipp }
    
let reset_terminative verb: t = { verb with adverbial = None; initial_person_prefix = None}

let set_ablative verb (person: PersonParam.t option): t =
    let open InitialPersonPrefix in
    let ipp: InitialPersonPrefix.t option = 
        match person with
        | Some First_sing -> Some First_sing
        | Some Second_sing -> Some Second_sing
        | Some Third_sing_human -> Some Third_sing_human
        | Some Third_sing_non_human -> Some Third_sing_non_human
        | Some First_plur -> Some First_plur
        | Some Second_plur -> Some Second_plur
        | Some Third_plur_human -> Some Third_plur_human
        | Some Third_plur_non_human -> Some Third_plur_non_human
        | None -> None
  in { verb with adverbial = Some Ablative; initial_person_prefix = ipp }

let reset_ablative verb: t = { verb with adverbial = None; initial_person_prefix = None }

let set_middle_prefix verb: t = { verb with middle_prefix = true }

let reset_middle_prefix verb: t = { verb with middle_prefix = false }

let set_initial_person_prefix verb (ipp: PersonParam.t): t =
    let open InitialPersonPrefix in
    let prefix: InitialPersonPrefix.t option = match ipp with
        | First_sing -> Some First_sing
        | Second_sing -> Some Second_sing
        | Third_sing_human -> Some Third_sing_human
        | Third_sing_non_human -> Some Third_sing_non_human
        | First_plur -> Some First_plur
        | Second_plur -> Some Second_plur
        | Third_plur_human -> Some Third_plur_human
        | Third_plur_non_human -> Some Third_plur_non_human
    in { verb with initial_person_prefix = prefix }

let set_final_person_prefix verb (pp: PersonParam.t): (t, string) result =
    try
        let prefix = FinalPersonPrefix.from_person pp
        in Ok { verb with final_person_prefix = Some prefix }
    with
    | Failure exn -> Error exn

let set_indirect_object verb (ipp: PersonParam.t): t =
    let open IndirectObjectPrefix in
    let prefix: IndirectObjectPrefix.t option = match ipp with
    | First_sing -> Some First_sing
    | Second_sing -> Some Second_sing
    | Third_sing_human -> Some Third_sing_human
    | Third_sing_non_human -> Some Third_sing_non_human
    | First_plur -> Some First_plur
    | Second_plur -> Some Second_plur
    | Third_plur_human -> Some Third_plur_human
    | Third_plur_non_human -> Some Third_plur_non_human
    in { verb with indirect_object_prefix = prefix }

let set_comitative verb (person: PersonParam.t option): t =
    let open InitialPersonPrefix in
    let ipp: InitialPersonPrefix.t option = match person with
        | Some First_sing -> Some First_sing
        | Some Second_sing -> Some Second_sing
        | Some Third_sing_human -> Some Third_sing_human
        | Some Third_sing_non_human -> Some Third_sing_non_human
        | Some First_plur -> Some First_plur
        | Some Second_plur -> Some Second_plur
        | Some Third_plur_human -> Some Third_plur_human
        | Some Third_plur_non_human -> Some Third_plur_non_human
        | None -> None
    in { verb with comitative = true; initial_person_prefix = ipp }

let reset_comitative verb: t = { verb with comitative = false; initial_person_prefix = None}

let set_locative_in verb person: t =
    match person with
    | Some(_) -> { 
        verb with locative = Some In_with_initial_person
        (* initialPersonPrefix = Some(ps->Utils.fromPersonToIPP) *)
    }
    | None -> { verb with locative = Some In_without_initial_person}

let set_locative_on verb person: t =
    match person with
    | Some(_) -> { 
        verb with locative = Some On_with_initial_person
        (* initialPersonPrefix = Some(ps->Utils.fromPersonToIPP) *)
    }
    | None -> { verb with locative = Some On_without_initial_person}

let reset_locative verb: t = { verb with locative = None}

let set_subject (verb: t) (person: PersonParam.t): (t, string) result =
    let open FinalPersonSuffix in
    if not verb.is_transitive || (verb.is_transitive && not verb.is_perfective)
    then let suffix: FinalPersonSuffix.t = match person with
            | First_sing -> First_sing
            | Second_sing -> Second_sing
            | Third_sing_human -> Third_sing_human 
            | Third_sing_non_human -> Third_sing_non_human
            | First_plur -> First_plur
            | Second_plur -> Second_plur
            | Third_plur_human -> Third_plur_human
            | Third_plur_non_human -> Third_plur_non_human
        in Ok { 
                verb with 
                    final_person_suffix = Some suffix; 
                    subject = Subject_suffix (suffix |> FinalPersonSuffix.to_person) 
            }
    else
        try
            let prefix = FinalPersonPrefix.from_person person
            in Ok { 
                    verb with 
                        final_person_prefix = Some prefix; 
                        subject = Subject_prefix (prefix |> FinalPersonPrefix.to_person) 
                }
        with
        | Failure exn -> Error exn

let set_object (verb: t) (person: PersonParam.t): t =
    let open FinalPersonSuffix in
    if verb.is_transitive && verb.is_perfective
    then let suffix: FinalPersonSuffix.t = match person with
            | First_sing -> First_sing
            | Second_sing -> Second_sing
            | Third_sing_human -> Third_sing_human
            | Third_sing_non_human -> Third_sing_non_human
            | First_plur -> First_plur
            | Second_plur -> Second_plur
            | Third_plur_human -> Third_plur_human
            | Third_plur_non_human -> Third_plur_non_human
        in { 
                verb with 
                    final_person_suffix = Some suffix; 
                    object_ = Object_suffix (suffix |> FinalPersonSuffix.to_person) 
            }
    else if verb.is_transitive && not verb.is_perfective
    then 
        let prefix = FinalPersonPrefix.from_person person
        in { 
                verb with 
                    final_person_prefix = Some prefix; 
                    object_ = Object_prefix (prefix |> FinalPersonPrefix.to_person) 
            }
    else
        (* cannot set the object of an intransitive verb
        TODO = may be worth throwing an error here
        but it will mess with the verb building with pipes *)
        verb

(* reset_ting the subject and the object is useful
when switching between transitive/intransitive and perfective/imperfective
on the website *)
let reset_subject_object verb: t = { verb with final_person_prefix = None; final_person_suffix = None}

let set_oblique_object (verb: t) person =
    (* 18.2.1 Since a finite verbal form can contain at most one final person-prefix, the oblique
    object cannot be expressed with a final person-prefix if a verbal form already contains such a
    prefix for expressing the transitive subject or the direct object. If that is the case, that is to say,
    if a finite verbal form contains a final person-prefix expressing the transitive subject or the
    direct object, an oblique object is expressed with an initial person-prefix. *)
    match verb.final_person_prefix with
    | Some(_) -> 
        { verb with oblique_object = Initial_person_prefix (person|>InitialPersonPrefix.from_person) }
    | None -> 
        { verb with oblique_object =  Final_person_prefix (person|>FinalPersonPrefix.from_person) }

let set_ed_marker verb: t = { verb with ed_marker = true}

let reset_ed_marker verb: t = { verb with ed_marker = false}

let print verb (meaning: string option) = 
        try Verb_output.print verb meaning
        with
            | Invalid_argument exn -> Error ("Invalid argument: " ^ exn)
            | _ -> Error "Unknown error"