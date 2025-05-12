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
        | _ -> failwith "Invalid person for FinalPersonPrefix"
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
        | _ -> failwith "Invalid person for FinalPersonSuffix"
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