module PersonParam = struct
    type t =
    | First_sing
    | Second_sing
    | Third_sing_human
    | Third_sing_non_human
    | First_plur
    | Second_plur
    | Third_plur_human
    | Third_plur_non_human

    type case =
    | Subject
    | Object
    | Indirect_object

    let print case person = 
        match case with
        | Subject -> 
            (match person with
            | First_sing -> "I"
            | Second_sing -> "You"
            | Third_sing_human -> "He/She"
            | Third_sing_non_human -> "It"
            | First_plur -> "We"
            | Second_plur -> "You (plural)"
            | Third_plur_human -> "They (human)"
            | Third_plur_non_human -> "They (non-human)")
        | Object ->
            (match person with
            | First_sing -> "me"
            | Second_sing -> "you"
            | Third_sing_human -> "him/her"
            | Third_sing_non_human -> "it"
            | First_plur -> "us"
            | Second_plur -> "you (plural)"
            | Third_plur_human -> "them (human)"
            | Third_plur_non_human -> "them (non-human)")
        | Indirect_object ->
            (match person with
            | First_sing -> "to me"
            | Second_sing -> "to you"
            | Third_sing_human -> "to him/her"
            | Third_sing_non_human -> "to it"
            | First_plur -> "to us"
            | Second_plur -> "to you (plural)"
            | Third_plur_human -> "to them (human)"
            | Third_plur_non_human -> "to them (non-human)")
end
    
module FirstPrefix = struct
    type t =
    | Modal
    | Negative
    | Negative_nan
    | Modal_ga
end

module Preformative = struct
    type t =
    | A
    | I
    | U  
end


module FinalPersonPrefix = struct
    type t =
    | First_sing
    | Second_sing
    | Third_sing_human
    | Third_sing_non_human

    let from_person pers = 
        match pers with
        | PersonParam.First_sing -> First_sing
        | PersonParam.Second_sing -> Second_sing
        | PersonParam.Third_sing_human -> Third_sing_human
        | PersonParam.Third_sing_non_human -> Third_sing_non_human
        | _ -> Utils.todo "Unsupported person for FinalPersonPrefix"

    let to_person pers =
        match pers with
        | First_sing -> PersonParam.First_sing
        | Second_sing -> PersonParam.Second_sing
        | Third_sing_human -> PersonParam.Third_sing_human
        | Third_sing_non_human -> PersonParam.Third_sing_non_human
end

module InitialPersonPrefix = struct
    type t =
    | First_sing
    | Second_sing
    | Third_sing_human
    | Third_sing_non_human
    | First_plur
    | Second_plur
    | Third_plur_human
    | Third_plur_non_human

    let from_person pers = 
        match pers with
        | PersonParam.First_sing -> First_sing
        | PersonParam.Second_sing -> Second_sing
        | PersonParam.Third_sing_human -> Third_sing_human
        | PersonParam.Third_sing_non_human -> Third_sing_non_human
        | PersonParam.First_plur -> First_plur
        | PersonParam.Second_plur -> Second_plur
        | PersonParam.Third_plur_human -> Third_plur_human
        | PersonParam.Third_plur_non_human -> Third_plur_non_human

    let to_person pers =
        match pers with
        | First_sing -> PersonParam.First_sing
        | Second_sing -> PersonParam.Second_sing
        | Third_sing_human -> PersonParam.Third_sing_human
        | Third_sing_non_human -> PersonParam.Third_sing_non_human
        | First_plur -> PersonParam.First_plur
        | Second_plur -> PersonParam.Second_plur
        | Third_plur_human -> PersonParam.Third_plur_human
        | Third_plur_non_human -> PersonParam.Third_plur_non_human

    let to_final_person_prefix pers = 
        match pers with
        | First_sing -> FinalPersonPrefix.First_sing
        | Second_sing -> FinalPersonPrefix.Second_sing
        | Third_sing_human -> FinalPersonPrefix.Third_sing_human
        | Third_sing_non_human -> FinalPersonPrefix.Third_sing_non_human
        | _ -> failwith "Invalid person for FinalPersonSuffix"
end
    
module IndirectObjectPrefix = struct
    type t =
    | First_sing
    | Second_sing
    | Third_sing_human
    | Third_sing_non_human
    | First_plur
    | Second_plur
    | Third_plur_human
    | Third_plur_non_human

    let to_person pers = 
        match pers with
        | First_sing -> PersonParam.First_sing
        | Second_sing -> PersonParam.Second_sing
        | Third_sing_human -> PersonParam.Third_sing_human
        | Third_sing_non_human -> PersonParam.Third_sing_non_human
        | First_plur -> PersonParam.First_plur
        | Second_plur -> PersonParam.Second_plur
        | Third_plur_human -> PersonParam.Third_plur_human
        | Third_plur_non_human -> PersonParam.Third_plur_non_human
end

module FinalPersonSuffix = struct
    type t =
    | First_sing
    | Second_sing
    | Third_sing_human
    | Third_sing_non_human
    | First_plur
    | Second_plur
    | Third_plur_human
    | Third_plur_non_human

    let to_final_person_prefix pers = 
        match pers with
        | First_sing -> FinalPersonPrefix.First_sing
        | Second_sing -> FinalPersonPrefix.Second_sing
        | Third_sing_human -> FinalPersonPrefix.Third_sing_human
        | Third_sing_non_human -> FinalPersonPrefix.Third_sing_non_human
        | _ -> Utils.todo "Invalid person for FinalPersonSuffix"

    let to_person pers = 
        match pers with
        | First_sing -> PersonParam.First_sing
        | Second_sing -> PersonParam.Second_sing
        | Third_sing_human -> PersonParam.Third_sing_human
        | Third_sing_non_human -> PersonParam.Third_sing_non_human
        | First_plur -> PersonParam.First_plur
        | Second_plur -> PersonParam.Second_plur
        | Third_plur_human -> PersonParam.Third_plur_human
        | Third_plur_non_human -> PersonParam.Third_plur_non_human
end

type adverbial_prefix =
| Ablative
| Terminative

type locative_prefix =
| In_with_initial_person
| In_without_initial_person
| On_with_initial_person
| On_without_initial_person

type oblique_object =
| Final_person_prefix of FinalPersonPrefix.t
| Initial_person_prefix of InitialPersonPrefix.t
| None

type subject =
| Subject_prefix of PersonParam.t
| Subject_suffix of PersonParam.t
| None

type object_ =
| Object_prefix of PersonParam.t
| Object_suffix of PersonParam.t
| None