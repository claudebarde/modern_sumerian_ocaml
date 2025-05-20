open Infixes
open Utils

type t = {
    (* Slot 1 Modal prefix (ḫa), negative particle, prefix of anteriority, stem (in imperative forms) *)
    slot1: (FirstPrefix.t * string) option;
    (* Slot 2 Finite-marker prefix, modal prefixes (all the other) *)
    slot2: string option;
    (* Slot 3 Coordinator prefix *)
    slot3: string option;
    (* Slot 4 Ventive (cislocative) prefix *)
    slot4: string option;
    (* Slot 5 Middle prefix or the 3.SG.NH pronominal prefix /b/ (specifying the person, *)
    (* gender and number of the first in the sequence of adverbial prefixes) *)
    slot5: string option;
    (* Slot 6 Initial Pronominal Prefix (= IPP) (specifying the person, gender and number *)
    (* of the first in the sequence of adverbial prefixes) *)
    slot6: (InitialPersonPrefix.t * string) option;
    (* Slot 7 Adverbial I: dative prefix *)
    slot7: string option;
    (* Slot 8 Adverbial II: comitative prefix *)
    slot8: string option;
    (* Slot 9 Adverbial III: ablative or terminative prefix *)
    slot9: (adverbial_prefix * string) option;
    (* Slot 10 Adverbial IV: locative1, locative2, or locative3 prefix *)
    slot10: string option;
    (* Slot 11 Final Pronominal Prefix (= FPP) (referring to A or P, depending on the tense, *)
    (* or locative3) *)
    slot11: (FinalPersonPrefix.t * string) option;
    (* Slot 12 stem *)
    slot12: string option;
    (* Slot 13 present-future marker (in intransitive verbs) *)
    slot13: string option;
    (* Slot 14 pronominal suffix (referring A, S, or P depending on the tense) *)
    slot14: (FinalPersonSuffix.t * string) option;
    (* Slot 15 subordinator *)
    slot15: string option;
}

let create () = {
    slot1 = None;
    slot2 = None;
    slot3 = None;
    slot4 = None;
    slot5 = None;
    slot6 = None;
    slot7 = None;
    slot8 = None;
    slot9 = None;
    slot10 = None;
    slot11 = None;
    slot12 = None;
    slot13 = None;
    slot14 = None;
    slot15 = None;
}

let rec analyse (verbArr: string array) (verbRec: Constructs.conjugated_verb) (verb: t) (start: int): t =
    if start >= 15
    then
        verb
    else
        match (start, get_morpheme_at_pos start verbArr) with
        | (0, Some prefix) ->
            if String.length prefix > 0 
            then
                (match verbRec.first_prefix with
                | None -> failwith "First prefix is expected to be set in verb"
                | Some fp ->
                    let verb = { verb with slot1 = Some (fp, prefix) } in
                    analyse verbArr verbRec verb (start + 1)
                )
             else 
                analyse verbArr verbRec verb (start + 1)
        | (1, Some prefix) ->
            if String.length prefix > 0 
            then
                let verb = { verb with slot2 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)
        | (2, Some prefix) ->
            if String.length prefix > 0 
            then 
                let verb = { verb with slot3 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)            
        | (3, Some prefix) ->
            if String.length prefix > 0  
            then
                let verb = { verb with slot4 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)
        | (4, Some prefix) ->
            if String.length prefix > 0  
            then
                let verb = { verb with slot5 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)
        | (5, Some prefix) ->
            (match verbRec.initial_person_prefix with
            | Some ipp -> 
                if String.length prefix > 0 
                then
                    let verb = { verb with slot6 = Some (ipp, prefix) } in
                    analyse verbArr verbRec verb (start + 1)
                 else 
                    analyse verbArr verbRec verb (start + 1)
            | _ -> analyse verbArr verbRec verb (start + 1))            
        | (6, Some prefix) ->
            if String.length prefix > 0  
            then
                let verb = { verb with slot7 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)            
        | (7, Some prefix) ->
            if String.length prefix > 0  
            then
                let verb = { verb with slot8 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)            
        | (8, Some prefix) ->
            if String.length prefix > 0  
            then
                (match verbRec.adverbial with
                | None -> failwith "Adverbial is expected to be set in verb"
                | Some adverbial ->
                    let verb = { verb with slot9 = Some((adverbial, prefix)) } in
                    analyse verbArr verbRec verb (start + 1)
                )
             else 
                analyse verbArr verbRec verb (start + 1)            
        | (9, Some prefix) ->
            if String.length prefix > 0  
            then
                let verb = { verb with slot10 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)            
        | (10, Some prefix) -> 
            (match verbRec.final_person_prefix with
            | Some fpp -> 
                if String.length prefix > 0 
                then
                    let verb = { verb with slot11 = Some (fpp, prefix) } in
                    analyse verbArr verbRec verb (start + 1)
                 else 
                    analyse verbArr verbRec verb (start + 1)  
            | _ -> analyse verbArr verbRec verb (start + 1))
        | (11, Some prefix) -> 
            if String.length prefix > 0  
            then
                let verb = { verb with slot12 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)
        | (12, Some prefix) -> 
            if String.length prefix > 0  
            then
                let verb = { verb with slot13 = Some prefix } in
                analyse verbArr verbRec verb (start + 1)
             else 
                analyse verbArr verbRec verb (start + 1)
        | (13, Some prefix) -> 
            (match verbRec.final_person_suffix with 
            | Some fpp -> 
                if String.length prefix > 0  
                then
                    let verb = { verb with slot14 = Some (fpp, prefix) } in
                    analyse verbArr verbRec verb (start + 1)
                 else 
                    analyse verbArr verbRec verb (start + 1)
            | _ -> analyse verbArr verbRec verb (start + 1))
        | (14, Some prefix) -> 
            if String.length prefix > 0  
            then
                 { verb with slot15 = Some prefix }
             else 
                verb
        | (_, None) -> analyse verbArr verbRec verb (start + 1)
        | _ -> verb

let output (verb: t): (string * string) array = 
    let slot1 = match verb.slot1 with 
        | None -> ("", "")
        | Some((prefix_type, prefix)) -> 
            (match prefix_type with
            | FirstPrefix.Modal -> ("modal", prefix)
            | FirstPrefix.Negative -> ("negative", prefix)
            | FirstPrefix.Negative_nan -> ("negativeNan", prefix)
            | FirstPrefix.Modal_ga -> ("modalGa", prefix))
    in
    let slot2 = match verb.slot2 with 
        | None -> ("", "")
        | Some prefix -> ("preformative", prefix)
    in
    let slot3 = match verb.slot3 with 
        | None -> ("", "")
        | Some prefix -> ("coordinator", prefix)
    in
    let slot4 = match verb.slot4 with 
        | None -> ("", "")
        | Some prefix -> ("ventive", prefix)
    in
    let slot5 = match verb.slot5 with 
        | None -> ("", "")
        | Some prefix -> ("middlePrefix", prefix)
    in
    let slot6 = match verb.slot6 with 
        | None -> ("", "")
        | Some((_, prefix)) -> ("initialPersonPrefix", prefix)
    in
    let slot7 = match verb.slot7 with 
        | None -> ("", "")
        | Some prefix -> ("dative", prefix)
    in
    let slot8 = match verb.slot8 with 
        | None -> ("", "")
        | Some prefix -> ("comitative", prefix)
    in
    let slot9 = match verb.slot9 with 
        | None -> ("", "")
        | Some (prefix_type, prefix) -> 
            (match prefix_type with
            | Infixes.Ablative -> ("ablative", prefix)
            | Infixes.Terminative -> ("terminative", prefix))
        in
    let slot10 = match verb.slot10 with 
        | None -> ("", "")
        | Some prefix -> ("locative", prefix)
    in
    let slot11 = match verb.slot11 with 
        | None -> ("", "")
        | Some((_, prefix)) -> ("finalPersonPrefix", prefix)
    in
    let slot12 = match verb.slot12 with 
        | None -> ("", "")
        | Some prefix -> ("stem", prefix)
    in
    let slot13 = match verb.slot13 with 
        | None -> ("", "")
        | Some prefix -> ("edMarker", prefix)
    in
    let slot14 = match verb.slot14 with 
        | None -> ("", "")
        | Some((_, prefix)) -> ("finalPersonSuffix", prefix)
    in
    let slot15 = match verb.slot15 with 
        | None -> ("", "")
        | Some prefix -> ("subordinator", prefix)
    in

    [| slot1; slot2; slot3; slot4; slot5; slot6; slot7; slot8; slot9; slot10; slot11; slot12; slot13; slot14; slot15|]
    |> Array.to_list
    |> List.filter (fun (x, y) -> (String.length x > 0 || String.length y > 0))
    |> Array.of_list

let print (verb: t): string = 
    let slot1 = match verb.slot1 with 
        | None -> ""
        | Some (_, prefix) -> prefix
    in
    let slot2 = Option.value verb.slot2 ~default:"" in
    let slot3 = Option.value verb.slot3 ~default:"" in
    let slot4 = Option.value verb.slot4 ~default:"" in
    let slot5 = Option.value verb.slot5 ~default:"" in
    let slot6 = match verb.slot6 with 
        | None -> ""
        | Some((_, prefix)) -> prefix
    in
    let slot7 = Option.value verb.slot7 ~default:"" in
    let slot8 = Option.value verb.slot8 ~default:"" in
    let slot9 = match verb.slot9 with 
        | None -> ""
        | Some((_, prefix)) -> prefix
    in
    let slot10 = Option.value verb.slot10 ~default:"" in
    let slot11 = match verb.slot11 with 
        | None -> ""
        | Some((_, prefix)) -> prefix
    in
    let slot12 = Option.value verb.slot12 ~default:"" in
    let slot13 = Option.value verb.slot13 ~default:"" in
    let slot14 = match verb.slot14 with 
        | None -> ""
        | Some((_, prefix)) -> prefix
    in
    let slot15 = Option.value verb.slot15 ~default:"" in

    [| slot1; slot2; slot3; slot4; slot5; slot6; slot7; slot8; slot9; slot10; slot11; slot12; slot13; slot14; slot15 |]
    |> Array.to_list
    |> List.filter (fun x -> String.length x > 0)
    |> String.concat ""

module Translation = struct
    let conjugate (verb_form: Constructs.conjugated_verb) (english_verb: string): string =
        let res = Array.make 4 "" in

        let subject = 
            match verb_form.subject with
            | Subject_prefix subj | Subject_suffix subj -> subj |> PersonParam.print Subject
            | _ -> ""
        in res.(0) <- subject;

        let object_ = 
            match verb_form.object_ with
            | Object_prefix obj | Object_suffix obj -> obj |> PersonParam.print Object
            | _ -> ""
        in res.(2) <- object_;

        let indirect_object = 
            match verb_form.indirect_object_prefix with
            | Some obj -> obj |> IndirectObjectPrefix.to_person |> PersonParam.print Indirect_object
            | None -> ""
        in res.(3) <- indirect_object;

        let conjugated_verb =
            match verb_form.subject with
            | Subject_prefix subj | Subject_suffix subj -> 
                (
                    match subj with
                    | PersonParam.Third_sing_human | PersonParam.Third_sing_non_human -> 
                        if String.ends_with ~suffix:"o" english_verb
                        then english_verb ^ "es"
                        else english_verb ^ "s"
                    | _ -> english_verb
                )
            | _ -> english_verb
        in res.(1) <- conjugated_verb;

        Array.to_list res |> String.concat " " |> String.trim
  
    let translate (verb: Constructs.conjugated_verb) (meaning: string option): string =
        match meaning with
        | Some m -> 
            (* isolates the verb root *)
            let re = Js.Re.fromString "\\(to ([a-z]+)\\)" in
            (
                match (Js.Re.exec ~str:m re) with
                | Some res -> (
                    (* Captures the verb root *)
                    let caps = Js.Re.captures res in
                    if Array.length caps >= 2
                    then 
                        (
                            match caps.(1) |> Js.Nullable.toOption with
                            | Some cap -> conjugate verb cap
                            | None -> verb.stem
                        )
                    else verb.stem
                    )
                | None -> verb.stem
            )
        | None -> verb.stem

end