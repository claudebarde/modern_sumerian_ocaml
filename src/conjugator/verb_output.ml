open Infixes 
open Utils

type t = {
    verb: string;
    analysis: Verb_analysis.t;
    warnings: string array;
}

type morphemes_array = string array
type morphemes_res = (morphemes_array, string) result

let add_stem (arr: morphemes_array) stem: morphemes_res =
    arr.(stem_pos) <- stem;
    Ok arr
    
let add_first_prefix (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
    | Error err -> Error err
    | Ok arr ->
        let _ = match verb.first_prefix with
        | Some FirstPrefix.Modal -> arr.(first_prefix_pos) <- "ḫa"
        | Some FirstPrefix.Negative -> arr.(first_prefix_pos) <- "nu"
        | Some FirstPrefix.Negative_nan -> arr.(first_prefix_pos) <- "nan"
        | Some FirstPrefix.Modal_ga -> arr.(first_prefix_pos) <- "ga"
        | None -> ()
    in Ok arr

let add_preformative (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
    | Error(err) -> Error(err)
    | Ok(arr) -> 
        let _ = match verb.preformative with
        | Some(A) -> arr.(preformative_pos) <- "a"
        | Some(I) -> arr.(preformative_pos) <- "i"
        | Some(U) -> arr.(preformative_pos) <- "u"
        | None -> ()
    in Ok(arr)

(* TODO: create a function to add the coordinator prefix *)

let add_ventive (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
    | Error(err) -> Error(err)
    | Ok(arr) ->
        let _ = if verb.ventive then arr.(ventive_pos) <- "mu" else ()
        in Ok(arr)

let add_adverbial (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            let _ = match verb.adverbial with
                | Some(Ablative) -> arr.(adverbial_pos) <- "ta"
                | Some(Terminative) -> arr.(adverbial_pos) <- "ši"
                | None -> ()
            in Ok(arr)

let add_middle_prefix (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            let _ = if (verb.middle_prefix) then arr.(middle_prefix_pos) <- "ba" else () in Ok(arr)

let add_initial_person_prefix (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            let _ = match verb.initial_person_prefix with
                | Some(First_sing) -> arr.(initial_person_prefix_pos) <- "ʔ"
                | Some(Second_sing) -> arr.(initial_person_prefix_pos) <- "e"
                | Some(Third_sing_human) -> arr.(initial_person_prefix_pos) <- "n"
                | Some(Third_sing_non_human) -> arr.(initial_person_prefix_pos) <- "b"
                | Some(First_plur) -> arr.(initial_person_prefix_pos) <- "mē"
                | Some(Second_plur) -> arr.(initial_person_prefix_pos) <- "enē"
                | Some(Third_plur_human) -> arr.(initial_person_prefix_pos) <- "nnē"
                | Some(Third_plur_non_human) -> arr.(initial_person_prefix_pos) <- "b"
                | None -> ()
        in Ok(arr)

let add_indirect_object_prefix (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            let _ = match verb.indirect_object_prefix with
                | Some(First_sing) -> arr.(indirect_object_prefix_pos) <- "ma"
                | Some(Second_sing) -> arr.(indirect_object_prefix_pos) <- "ra"
                | Some(Third_sing_human) -> arr.(indirect_object_prefix_pos) <- "nna"
                | Some(Third_sing_non_human) -> arr.(indirect_object_prefix_pos) <- "ba"
                | Some(First_plur) -> arr.(indirect_object_prefix_pos) <- "mē"
                | Some(Second_plur) -> arr.(indirect_object_prefix_pos) <- "ra"
                | Some(Third_plur_human) -> arr.(indirect_object_prefix_pos) <- "nnē"
                | Some(Third_plur_non_human) -> arr.(indirect_object_prefix_pos) <- "ba"
                | None -> ()
        in Ok(arr)

let add_comitative (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            if (verb.comitative)
            then let _ = arr.(comitative_pos) <- "da" in Ok(arr)
            else Ok(arr)

let add_locative (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    (* TODO: The local prefix {ni} is the only dimensional prefix that is never combined with an initial person-prefix. 20.2.1 *)
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            let _ = match verb.locative with
                | Some(In_with_initial_person) -> arr.(locative_pos) <- ""
                | Some(In_without_initial_person) -> arr.(locative_pos) <- "ni"
                | Some(On_with_initial_person) -> arr.(locative_pos) <- "bi"
                | Some(On_without_initial_person) -> arr.(locative_pos) <- "e"
                | None -> ()
        in Ok(arr)

let add_final_person_prefix (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            let _ = match verb.final_person_prefix with
                | Some(First_sing) -> arr.(final_person_prefix_pos) <- "ʔ"
                | Some(Second_sing) -> arr.(final_person_prefix_pos) <- "e"
                | Some(Third_sing_human) -> arr.(final_person_prefix_pos) <- "n"
                | Some(Third_sing_non_human) -> arr.(final_person_prefix_pos) <- "b"
                | None -> ()
        in Ok(arr)

let add_ed_marker (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            if verb.ed_marker then let _ = arr.(ed_marker_pos) <- "ed" in Ok(arr) else Ok(arr)

let add_final_person_suffix (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    match arr with
        | Error(err) -> Error(err)
        | Ok(arr) ->
            let _ = match verb.final_person_suffix with
                | Some(First_sing) -> arr.(final_person_suffix_pos) <- "en"
                | Some(Second_sing) -> arr.(final_person_suffix_pos) <- "en"
                | Some(Third_sing_human) | Some(Third_sing_non_human) ->
                    (* can be Ø or "e" *)
                    if verb.is_transitive && not verb.is_perfective
                    then arr.(final_person_suffix_pos) <- "e"
                    else arr.(final_person_suffix_pos) <- ""
                | Some(First_plur) -> arr.(final_person_suffix_pos) <- "enden"
                | Some(Second_plur) -> arr.(final_person_suffix_pos) <- "enzen"
                | Some(Third_plur_human) -> arr.(final_person_suffix_pos) <-
                    (* can be "eš" or "enē" *)
                    "eš"
                | Some(Third_plur_non_human) -> arr.(final_person_suffix_pos) <- ""
                | None -> ()
            in Ok(arr)

let add_oblique_object (verb: Constructs.conjugated_verb) (arr: morphemes_res) =
    (* 18.2.1 Expressing an oblique object is only one possible use of the final person-prefixes, and it is
    the least important one. The final person-prefixes have two other uses, which have priority *)
    match arr with
    | Error(err) -> Error(err)
    | Ok(arr) ->
        match verb.oblique_object with
        | Final_person_prefix fpp ->
            (match verb.final_person_prefix with
            | Some(_) -> Error("Oblique object would overwrite final person prefix")
            | None ->
                let _ = 
                    arr.(final_person_prefix_pos) <- 
                        (match fpp with
                        | First_sing -> "ʔ"
                        | Second_sing -> "e"
                        | Third_sing_human -> "n"
                        | Third_sing_non_human -> "b")
                in Ok(arr))
        | Initial_person_prefix ipp ->
            (match verb.initial_person_prefix with
            | Some(_) -> Error("Oblique object would overwrite initial person prefix")
            | None ->
                let _ = 
                    arr.(initial_person_prefix_pos) <- 
                        (match ipp with
                        | First_sing -> "mu"
                        | Second_sing -> "ri"
                        | Third_sing_human -> "nni"
                        | Third_sing_non_human -> "bi"
                        | First_plur -> "mē"
                        | Second_plur -> "enē"
                        | Third_plur_human -> "nnē"
                        | Third_plur_non_human -> "bi")
                in Ok(arr))
        | None -> Ok(arr)

let print (verb: Constructs.conjugated_verb): (t, string) result  =
    let warnings: string array = [||] in
    let morphemes_start = Array.make 15 "" in
    (* builds the array with all the markers *)
    let (outputRes: morphemes_res) = 
        verb.stem 
        |> add_stem morphemes_start
        |> add_first_prefix verb
        |> add_preformative verb
        |> add_ventive verb
        |> add_adverbial verb
        |> add_middle_prefix verb
        |> add_initial_person_prefix verb
        |> add_indirect_object_prefix verb
        |> add_comitative verb
        |> add_locative verb
        |> add_final_person_prefix verb
        |> add_ed_marker verb
        |> add_final_person_suffix verb
        |> add_oblique_object verb
    in

    match outputRes with
        | Error(err) -> Error(err)
        | Ok(outputArr) ->
            (* apply phonological changes to the markers *)
            let outputRes =
                (* PREFORMATIVE CONTRACTION
                TODO: 24.3.1 they are never found before a prefix with the shape /CV/.
                Instead of a vocalic prefix we then find zero, that is, no preformative at all. *)
                let outputArr = 
                    match (outputArr |> get_morpheme_at_pos preformative_pos) with
                    | Some preformative ->
                        (* find previous morpheme *)
                        (match find_previous_morpheme preformative_pos outputArr with
                            | Some((marker, _)) ->
                                (match (marker, preformative) with
                                    (* TODO: An imperfective form with {h~a} is always transitive 25.4.2 *)
                                    | ("ḫa", "i") ->
                                        (* If the verbal form begins with the vocalic prefix /ʔi/ (§24.3),
                                        /ḫa/ contracts with it. The sequence /ḫaʔi/ thus becomes /ḫē/ *)
                                    let _ = outputArr.(preformative_pos) <- "" in
                                    let _ = outputArr.(first_prefix_pos) <- "ḫē" in outputArr
                                    | ("nu", "i") ->
                                        (* 24.3.2 *)
                                        let _ = outputArr.(preformative_pos) <- "u" in outputArr
                                    | _ ->
                                        (* no change *)
                                        outputArr)
                            | None ->
                                (* 24.3.2 The prefix {ʔi} may also contract with the verbal stem,
                                if the latter has an initial glottal stop. *)
                                (match find_next_morpheme preformative_pos outputArr with
                                | Some (morpheme, marker) ->
                                    if marker == Stem 
                                        (* stem must start with CV and first consonant must be a glottal stop *)
                                    then 
                                        let stem_start_struct = String.sub (consonant_vowel_sequence morpheme) 0 2 in
                                        if stem_start_struct == "CV" && String.sub morpheme 0 1 == "ʔ"
                                        then 
                                            let prefix = String.sub morpheme 0 1 in
                                            (* removes the "i" of the preformative *)
                                            let _ = outputArr.(preformative_pos) <- prefix in
                                            (* removes the "i" of the stem *)
                                            outputArr
                                        else
                                            (* no change *)
                                            outputArr
                                    else
                                        (* no change *)
                                        outputArr
                                | None ->
                                    (* there is no marker after the preformative *)
                                    outputArr)
                        )
                    | _ -> outputArr
                in

                (* INITIAL PERSON PREFIX CHANGES *)
                let outputArr = 
                    match (
                        get_morpheme_at_pos ventive_pos outputArr, 
                        get_morpheme_at_pos initial_person_prefix_pos outputArr
                    ) with
                    | (Some ventive, Some ipp) ->
                        if (ipp == "b")
                            (* First, the prefix {b} cannot occur between the ventive prefix and a consonant (see §22.4).
                            Second, between the form /m/ of the ventive and a vowel, the prefix {b} assimilates to the /m/. *)
                        then 
                            (match find_next_morpheme initial_person_prefix_pos outputArr with
                            | Some (morpheme, _) ->
                                if (morpheme |> starts_with_consonant && ventive == "mu")
                                then
                                    let _ = outputArr.(initial_person_prefix_pos) <- "" in outputArr
                                else if (morpheme |> starts_with_vowel && ventive == "m")
                                then
                                    let _ = outputArr.(initial_person_prefix_pos) <- "m" in outputArr
                                else
                                    (* no change *)
                                    outputArr
                            | _ ->
                                (* there is no marker after the initial person prefix *)
                                outputArr
                            )
                        (* 16.2.5 In the texts of our corpus, the ventive prefix {mu} (chapter 17)
                        is always used before the initial person-prefix /ʔ/ and always has the form /mu/ *)
                        else if ipp == "ʔ"
                        then
                            let _ = outputArr.(ventive_pos) <- "mu" in outputArr
                        else
                            (* no change *)
                            outputArr
                    | _ -> outputArr
                in

                let outputArr = 
                    match get_morpheme_at_pos initial_person_prefix_pos outputArr with
                    | Some(ipp) when ipp == "e" ->
                        (* 16.2.4 The prefix {e} contracts with a preceding vowel, lengthening that vowel. *)
                        (match find_previous_morpheme initial_person_prefix_pos outputArr with
                            | Some (marker, _) ->
                                if ends_with_vowel marker
                                then
                                    (* gets the last vowel of the marker *)
                                    let last_vowel = String.sub marker ((String.length marker) - 1) (String.length marker) in
                                    let _ = outputArr.(initial_person_prefix_pos) <- last_vowel in outputArr
                                else
                                    (* no change *)
                                    outputArr
                            | None ->
                                (* there is no marker before the initial person prefix *)
                                outputArr)
                    | _ -> outputArr
                in

                (* LOCATIVE CHANGES *)
                let outputArr =
                    match get_morpheme_at_pos locative_pos outputArr with
                        | Some locative ->
                            (* 21.2 (7) Although there are many plene spellings of the type ba-a-, they
                            typically represent a sequence of two different prefixes, mostly a contraction of {ba} and {e}. *)
                            if locative == "e"
                            then
                                (* TODO: 20.3.1 If the verbal form also contains a final person-prefix, the
                                local prefix {e} cannot be used at all. *)
                                (match find_previous_morpheme locative_pos outputArr with
                                    (* 20.3.1 This /e/ has the same forms and spellings as the final
                                    person-prefix {e} of the second person.
                                    Just like the latter, it contracts with a preceding vowel,                              lengthening the preceding vowel *)
                                    | Some (morpheme, marker) ->
                                        (match marker with
                                            | MiddlePrefix when morpheme == "ba" ->
                                                let _ = outputArr.(locative_pos) <- "a" in outputArr
                                            | _ ->
                                                if morpheme |> ends_with_vowel
                                                then
                                                    (* gets the last vowel of the marker *)
                                                    let new_morpheme = String.sub morpheme ((String.length morpheme) - 1) (String.length morpheme) in
                                                    let _ = outputArr.(locative_pos) <- new_morpheme in
                                                    outputArr
                                                else
                                                    outputArr)
                                    | None -> outputArr)
                            else if locative == "bi"
                            then
                                (match find_previous_morpheme locative_pos outputArr with
                                    | Some (_, marker) -> 
                                        (match marker with
                                            | Ventive ->
                                                let _ = outputArr.(locative_pos) <- "mi" in
                                                let _ = outputArr.(ventive_pos) <- "m" in
                                                outputArr
                                            | _ -> 
                                                (match get_morpheme_at_pos final_person_prefix_pos outputArr with
                                                | Some(fpp) when fpp == "" ->
                                                    (* TODO: verify that "bi" behaves like "ni" *)
                                                    let _ = outputArr.(locative_pos) <- "b" in
                                                    outputArr
                                                | _ -> outputArr))
                                    | None -> outputArr)
                            else if locative == "ni"
                            then
                                (match get_morpheme_at_pos final_person_prefix_pos outputArr with
                                    | Some(fpp) when fpp == "" ->
                                        (* 20.2.1 In the absence of a final person-prefix, the prefix {ni} stands immediately before the stem.
                                        Since every stem begins with a consonant and a vowel (§3.10), {ni} is then found in the
                                        sequence /niCV/, where the /i/ of the prefix is lost (§3.9.4). *)
                                        let _ = outputArr.(locative_pos) <- "n" in
                                        outputArr
                                    | _ -> outputArr
                                )
                            else
                                (* no changes *)
                                outputArr
                        | _ -> outputArr
                in

                let outputArr =
                    (* FINAL PERSON PREFIX CHANGES
                    13.2.4 The prefix {e} contracts with a preceding vowel, lengthening that vowel *)
                    match get_morpheme_at_pos final_person_prefix_pos outputArr with
                    | Some(fpp) when fpp == "e" ->
                        (* find previous morpheme *)
                        (match find_previous_morpheme final_person_prefix_pos outputArr with
                            | Some (morpheme, _) ->
                                if ends_with_vowel morpheme
                                then
                                    (* gets the last vowel of the marker *)
                                    let last_vowel = String.sub morpheme ((String.length morpheme) - 1) (String.length morpheme) in
                                    let _ = outputArr.(final_person_prefix_pos) <- last_vowel in
                                    outputArr
                                else
                                    (* no change *)
                                    outputArr
                            | None ->
                                (* there is no marker before the final person prefix *)
                                outputArr)
                    | _ -> outputArr
                in

                let outputArr =
                    (* FINAL PERSON SUFFIX CONTRACTION
                    14.1 First, the /e/ contracts with a preceding vowel.
                    Secondly, the /e/ may assimilate to a stem vowel /u/ or /i/.
                    //TODO: Finally, the /e/ may be reduced in forms with the nominalizing suffix {ʔa}. *)
                    match get_morpheme_at_pos final_person_suffix_pos outputArr with
                    | None -> outputArr
                    | Some(suffix) ->
                        (* find previous morpheme *)
                        (match find_previous_morpheme final_person_suffix_pos outputArr with
                        | Some((marker, _)) ->
                            if suffix == "e"
                            then
                                (* finds the previous vowel and assimilates the "e" *)
                                (match get_morpheme_at_pos ed_marker_pos outputArr with
                                | Some _marker ->
                                    (* // CHECK: may need to assimilate the "e" anyway
                                    if the "e" of ED marker assimilated to the stem *)
                                    outputArr
                                | _ ->
                                    (* // CHECK: may need to assimilate the "e" anyway
                                    to the last vowel of the stem *)
                                    if verb.stem |> ends_with_vowel
                                    then
                                        (* gets the last vowel of the stem *)
                                        (* removes the first character of the suffix *)
                                        let last_vowel = 
                                            if String.length verb.stem > 0
                                            then String.sub verb.stem ((String.length verb.stem) - 1) (String.length verb.stem)
                                            else failwith "Verb stem is missing"
                                        in
                                        (* // CHECK: the other vowels may assimilate the "e" too *)
                                        if last_vowel == "a"
                                        then
                                            let _ = outputArr.(final_person_suffix_pos) <- last_vowel in
                                            outputArr
                                        else
                                            outputArr
                                    else
                                        outputArr)
                            else if (String.length suffix > 1 && 
                                starts_with_vowel suffix && 
                                ends_with_vowel marker)
                            then
                                (* gets the last vowel of the marker *)
                                (* removes the first character of the suffix *)
                                let _ = outputArr.(final_person_suffix_pos) = remove_first_char suffix in
                                outputArr
                            else
                                (* no change *)
                                outputArr
                        | None ->
                            (* there is no marker before the preformative *)
                            outputArr)
                in

                (* ED MARKER CONTRACTION *)
                let outputArrRes = 
                    match get_morpheme_at_pos ed_marker_pos outputArr with
                    | Some ed_marker ->
                        (* only the stem can be before the marker *)
                        (match get_morpheme_at_pos stem_pos outputArr with
                        | Some(stem) ->
                            if ends_with_vowel stem
                            then
                                (* removes the first character of the marker *)
                                let _ = outputArr.(ed_marker_pos) <- remove_first_char ed_marker in
                                Ok(outputArr)
                            else
                                (* no change *)
                                Ok(outputArr)
                        | None -> Error("No stem was set"))
                    | None -> Ok(outputArr)
                in

                match outputArrRes with
                    | Error(err) -> Error(err)
                    | Ok(outputArr) ->
                        let outputArrRes = 
                            match get_morpheme_at_pos ventive_pos outputArr with
                            | Some ventive ->
                                let iop = outputArr.(indirect_object_prefix_pos) in
                                if (iop == "ba")
                                then
                                    (* 17.2.1 After the ventive prefix (§22.2), the prefix {ba} has a slighly different form,
                                    because the /b/ of {ba} assimilates to the preceding /m/ of the ventive: *)
                                    let _ = outputArr.(indirect_object_prefix_pos) <- "ma" in
                                    let _ = outputArr.(ventive_pos) <- "m" in
                                    Ok(outputArr)
                                else
                                    let middlePrefix = outputArr.(middle_prefix_pos) in
                                    if middlePrefix == "ba"
                                    then
                                        (* 21.2 Only after the ventive prefix (§22.2), {ba} has a slightly different form,
                                        because the /b/ of {ba} assimilates to the preceding /m/ of the ventive. *)
                                        let _ = outputArr.(middle_prefix_pos) <- "ma" in
                                        let _ = outputArr.(ventive_pos) <- "m" in
                                        Ok(outputArr)
                                    else
                                        (* VENTIVE CONTRACTION *)
                                        (match find_next_morpheme ventive_pos outputArr with
                                        | Some (morpheme, marker) ->                                       
                                            (* 22.2 Before the indirect-object prefix {ra}, the oblique-object prefix {ri},
                                            and the local prefix {ni}, however, the /u/ is always retained
                                            but may assimilate to the vowel of the following syllable.                                         *)
                                            if morpheme == "ni" || morpheme == "ri"
                                            then
                                                let _ = outputArr.(ventive_pos) <- "mi" in
                                                Ok(outputArr)
                                            else if morpheme == "ra"
                                            then
                                                let _ = outputArr.(ventive_pos) <- "ma" in
                                                Ok(outputArr)
                                            else if morpheme == "bi"
                                            then
                                                (* 18.2.2 If /bi/ follows the ventive prefix, the /b/ of /bi/ assimilates to the preceding /m/: *)
                                                let _ = outputArr.(ventive_pos) <- "m" in
                                                let _ = outputArr.(marker_to_pos marker) <- "mi" in
                                                Ok(outputArr) 
                                            else
                                                (* 22.2 The basic form of the ventive prefix is /mu/, 
                                                but the /u/ of the prefix is lost in the sequence /muCV/, that is, 
                                                if followed by a consonant and a vowel (§3.9.4). *)
                                                let morph_start_struct = consonant_vowel_sequence (String.sub morpheme 0 2) in
                                                if (ventive == "mu" && morph_start_struct == "CV")
                                                then
                                                    (* removes the "u" of the ventive *)
                                                    let _ = outputArr.(ventive_pos) <- "m" in
                                                    Ok(outputArr)
                                                else
                                                    (* no change *)
                                                    Ok(outputArr)
                                        | None -> Ok(outputArr))
                            | _ -> Ok(outputArr)
                        in

                        match outputArrRes with
                        | Error(err) -> Error(err)
                        | Ok(outputArr) -> 
                            (match get_morpheme_at_pos first_prefix_pos outputArr with
                            | Some first_prefix ->
                                (match first_prefix with
                                | "nu" ->
                                    (* 25.2 If the following vowel is /a/, {nu} becomes /na/. 
                                    This /na/ is written explicitly from the Ur III period onwards. Earlier texts have nu. *)
                                    (match find_next_morpheme first_prefix_pos outputArr with
                                    | Some (morpheme, _) ->
                                        (match (String.get morpheme 0, String.get morpheme 1) with
                                        | ('b', 'a') ->
                                            let _ = outputArr.(first_prefix_pos) = "la" in
                                            Ok(outputArr)
                                        | ('b', 'i') ->
                                            let _ = outputArr.(first_prefix_pos) = "li" in
                                            Ok(outputArr)
                                        | (_, ('a' | 'i')) ->
                                            let _ = outputArr.(first_prefix_pos) = "na" in
                                            Ok(outputArr)
                                        | _ ->
                                            (* no change *)
                                            Ok(outputArr))
                                    | _ -> Ok(outputArr))
                                    (* TODO: 
                                    17.2.4 (38) Also, the negative proclitic {nu} is never written nu-ù- before {ra}, a spelling that would be
                                    expected to occur if {nu} were followed by /÷i/ (§25.2). E.g.: *)
                                | "nan" -> 
                                    (* TODO: 25.5 while negative {na(n)} is almost exclusively used in imperfective forms
                                    25.5 /nan/ is used before a single consonant (i.e., before /CV/) and 
                                    /na/ before a cluster of two consonants (i.e., before /CC/). 
                                    The form /na/ is probably also the one used before a vowel *)
                                    let temp_verb = 
                                        Array.sub outputArr 0 (Array.length outputArr - 1) 
                                        |> Array.to_list 
                                        |> String.concat "" 
                                    in
                                    let cvc_seq = consonant_vowel_sequence temp_verb in
                                    if String.sub cvc_seq 0 2 == "CC"
                                        || String.sub cvc_seq 0 1 == "V"
                                    then
                                        (* 25.5 A by-form /na/ occurs before /b/ or /m/ *)
                                        let _ = outputArr.(first_prefix_pos) <- "na" in
                                        Ok(outputArr)
                                    else
                                        (* 25.5 A by-form /nam/ occurs before /b/ or /m/ *)
                                        if String.sub temp_verb 0 1 == "b"
                                        || String.sub temp_verb 0 1 == "m"
                                        then
                                            let _ = outputArr.(first_prefix_pos) <- "nam" in
                                            Ok(outputArr)
                                        else
                                            (* no change *)
                                            Ok(outputArr)
                                | "ga" ->
                                    (* 25.6 The verbal forms with the prefix {ga} have a hybrid make-up, 
                                    just like those of the imperative (§25.3). 
                                    They have perfective stem forms (§15.3.1) 
                                    but they use the final person-prefixes as in the imperfective inflection (§15.2.3). *)
                                    if not verb.is_perfective
                                    then
                                        Error("The modal \"ga\" requires a perfective verb form")
                                    else if verb.is_transitive
                                    then
                                        (* the subject must be first person *)
                                        (match verb.final_person_prefix with
                                        | Some ps -> 
                                            (match ps with
                                            | First_sing ->
                                                (* switches the position of the object *)
                                                (match verb.final_person_suffix with
                                                    | Some fps ->
                                                        let temp_verb = { 
                                                            verb with 
                                                                final_person_prefix = Some (FinalPersonSuffix.to_final_person_prefix fps) 
                                                        } in
                                                        (match add_final_person_prefix temp_verb (Ok outputArr) with
                                                            | Error(_) -> Error("Error while switching fpp and fps with \"ga\" modal")
                                                            | Ok(newOutputArr) -> Ok(newOutputArr))
                                                    | None ->
                                                        let _ = outputArr.(final_person_prefix_pos) <- "" in
                                                        Ok(outputArr))
                                            | _ -> Error("The modal \"ga\" doesn't need a subject"))
                                        | None ->
                                            (* 25.6 The final person-prefixes can also be used to refer to the oblique object. *)
                                            (match verb.oblique_object with
                                            | None -> Ok(outputArr)
                                            | Final_person_prefix _ -> Ok(outputArr)
                                            | Initial_person_prefix ipp ->
                                                (* switches the position of the object *)
                                                let temp_verb = { 
                                                    verb with 
                                                        final_person_prefix = Some (InitialPersonPrefix.to_final_person_prefix ipp)
                                                } in
                                                (match add_final_person_prefix temp_verb (Ok outputArr) with
                                                | Error(_) -> Error("Error while switching fpp and fps with \"ga\" modal")
                                                | Ok(newOutputArr) -> Ok(newOutputArr))))
                                    else Ok(outputArr)
                                | _ -> Ok(outputArr))
                            | _ -> Ok(outputArr))
            in
            
            (* returns the final string *)
            match outputRes with
            | Error(err) -> Error(err)
            | Ok(outputArr) -> Ok { 
                verb = outputArr |> Array.to_list |> String.concat "" ;
                analysis = Verb_analysis.analyse outputArr verb (Verb_analysis.create ()) 0;
                warnings = warnings;
            }