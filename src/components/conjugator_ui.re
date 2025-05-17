[@mel.module "../styles/Conjugator.module.scss"] external css: Js.t({..}) = "default"; 

type prefix =
    Modal
  | Negative
  | NegativeNan
  | Ventive
  | Comitative
  | Ablative
  | Terminative
  | MiddlePrefix
  | LocativeIn
  | LocativeOn;
  
type modal_prefix = HA | NAN | NU;

type verb_data = { label: string, value: string, imperfective: Conjugator.ipfv_stem };

module ReactSelect = {
    open Conjugator;

    type select_option = {
        label: string,
        value: string
    }

    let person_param_to_option = (pp: PersonParam.t): select_option => {
        switch pp {
        | PersonParam.First_sing => {label: "I", value: "first-sing"}
        | PersonParam.Second_sing => {label: "You (sing)", value: "second-sing"}
        | PersonParam.Third_sing_human => {label: "He/She", value: "third-sing-human"}
        | PersonParam.Third_sing_non_human => {label: "It", value: "third-sing-nonhuman"}
        | PersonParam.First_plur => {label: "We", value: "first-plur"}
        | PersonParam.Second_plur => {label: "You (plur)", value: "second-plur"}
        | PersonParam.Third_plur_human => {label: "They (human)", value: "third-plur-human"}
        | PersonParam.Third_plur_non_human => {label: "They (non-human)", value: "third-plur-nonhuman"}
        }
    };

    [@mel.module "react-select"] [@react.component]
    external make: (
        ~options: array<select_option>,
        ~value: Js.Nullable.t<select_option>,
        ~onChange: select_option => unit,
        ~isDisabled: bool
    ) => React.element = "default";
};

[@react.component]
let make = () => {
    let (error, set_error) = React.useState(_ => None);
    let (verb_stem, set_verb_stem) = React.useState(_ => Js.Nullable.null);
    let (verb_form, set_verb_form) = React.useState(_ => None);
    let (is_perfective, set_is_perfective) = React.useState(_ => None);
    let (is_transitive, set_is_transitive) = React.useState(_ => None);
    let (preformative, set_preformative) = React.useState(_ => None);
    let (modal_prefix, set_modal_prefix) = React.useState(_ => None);
    let (ventive, set_ventive) = React.useState(_ => false);
    let (comitative, set_comitative) = React.useState(_ => false);
    let (ablative, set_ablative) = React.useState(_ => false);
    let (terminative, set_terminative) = React.useState(_ => false);
    let (locative, set_locative) = React.useState(_ => None);
    let (middle_prefix, set_middle_prefix) = React.useState(_ => false);
    let (initial_person_prefix, set_initial_person_prefix) = React.useState(_ => Js.Nullable.null);
    let (subject, set_subject) = React.useState(_ => Js.Nullable.null);
    let (object_, set_object) = React.useState(_ => Js.Nullable.null);
    let (indirect_object, set_indirect_object) = React.useState(_ => Js.Nullable.null);
    let (is_modal_open, set_is_modal_open) = React.useState(_ => false);

    let available_verbs: array(verb_data) = [|
        {label: "ak (to do)", value: {js|ʔak|js}, imperfective: Other({js|ʔak|js}) },
        {label: {js|ĝen (to go)|js}, value: {js|ĝen|js}, imperfective: Other({js|ĝen|js})},
        {label: "gu (to eat)", value: "gu", imperfective: Other("gu")},
        {label: {js|naĝ (to drink)|js}, value: {js|naĝ|js}, imperfective: Other("na-na")},
        {label: "sar (to write)", value: "sar", imperfective: Other("sar")},
        {label: {js|šum (to give)|js}, value: {js|šum|js}, imperfective: Other({js|šum|js})},
        {label: "tuku (to have)", value: "tuku", imperfective: Other("tuku")},
    |];

    let verb_options: array(ReactSelect.select_option) = Array.map(
        (verb: verb_data): ReactSelect.select_option => {
            {label: verb.label, value: verb.value}
        },
        available_verbs
    );

    let pronoun_options: array(ReactSelect.select_option) = [|
        {label: "I", value: "first-sing"},
        {label: "You (sing)", value: "second-sing"},
        {label: "He/She", value: "third-sing-human"},
        {label: "It", value: "third-sing-nonhuman"},
        {label: "We", value: "first-plur"},
        {label: "You (plur)", value: "second-plur"},
        {label: "They (human)", value: "third-plur-human"},
        {label: "They (non-human)", value: "third-plur-nonhuman"},
    |];

    let set_new_verb_stem = (value: ReactSelect.select_option): unit => {
        set_verb_stem(_ => Js.Nullable.return(value));
        set_verb_form(_ => Some(Conjugator.create(value.value)));
        set_error(_ => None);
    };

    let change_pronoun = (value: ReactSelect.select_option, pronoun: string) => {
        if (Option.is_none(is_perfective) && Option.is_none(is_transitive)) {
            set_error(_ => Some("Aspect and transitivity must be selected"))
        } else {
            switch (pronoun, value.value |> Web_utils.pronoun_to_person_param) {
                | ("initial-person-prefix", Some(person_param)) => {
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                set_initial_person_prefix(_ => person_param |> Js.Nullable.return)
                                Some(Conjugator.set_initial_person_prefix(verb, person_param))
                            }
                            | None => None
                        }
                    })
                }
                | ("subject", Some(person_param)) => {
                    set_verb_form(prev_verb_form => {
                        switch (prev_verb_form) {
                            | Some(verb) => {
                                set_error(_ => None)
                                set_subject(_ => person_param |> Js.Nullable.return)
                                try (
                                    Conjugator.set_subject(verb, person_param)
                                    ->Result.get_ok
                                    ->Some
                                ) {
                                    | Conjugator__Utils.Todo(err) => {
                                        set_error(_ => Some(err))
                                        prev_verb_form
                                    }
                                }
                            }
                            | None => None
                        }
                    })
                }
                | ("object", Some(person_param)) => {
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                set_object(_ => person_param |> Js.Nullable.return)
                                Some(Conjugator.set_object(verb, person_param))
                            }
                            | None => None
                        }
                    })
                }
                | ("indirect-object", Some(person_param)) => {
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                set_indirect_object(_ => person_param |> Js.Nullable.return)
                                Some(Conjugator.set_indirect_object(verb, person_param))
                            }
                            | None => None
                        }
                    })
                }
                | _ => ()
            }
        }
    };

    let change_preformative = (ev: React.Event.Form.t) => {
        if (Option.is_none(is_perfective) && Option.is_none(is_transitive)) {
            set_error(_ => Some("Aspect and transitivity must be selected"))
        } else {
            let target = React.Event.Form.target(ev)
            let value: string = target##value
            let preformative = switch value {
                | "preformative-a" => Some(Conjugator.Preformative.A)
                | "preformative-i" => Some(Conjugator.Preformative.I)
                | "preformative-u" => Some(Conjugator.Preformative.U)
                | _ => None
            }
            set_verb_form(prev_verb_form => {
                switch (prev_verb_form, preformative) {
                    | (Some(verb), Some(preformative)) => {
                        set_error(_ => None)     
                        set_preformative(_ => Some(preformative))
                        Some(Conjugator.set_preformative(verb, preformative))
                    }
                    | (None, _) => {
                        set_error(_ => Some("No verb stem selected"))
                        None
                    }
                    | _ => None
                }
            })
        }
    };

    let change_prefix = (value: prefix, checked: bool) => {
        if (Js.Nullable.isNullable(verb_stem)) {
            set_error(_ => Some("No verb stem selected"))
        } else if (Option.is_none(is_perfective) && Option.is_none(is_transitive)) {
            set_error(_ => Some("Aspect and transitivity must be selected"))
        } else {
            switch value {
                | Modal => {
                    set_modal_prefix(_ => Some(HA))
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                if (checked) {
                                    Some(Conjugator.set_modal(verb))
                                } else {
                                    Some(Conjugator.reset_modal(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | Negative => {
                    set_modal_prefix(_ => Some(NU))
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                if (checked) {
                                    Some(Conjugator.set_negative(verb))
                                } else {
                                    Some(Conjugator.reset_negative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | NegativeNan => {
                    set_modal_prefix(_ => Some(NAN))
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                if (checked) {
                                    Some(Conjugator.set_negative_nan(verb))
                                } else {
                                    Some(Conjugator.reset_negative_nan(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | Ventive => {
                    set_ventive(_ => checked)
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                if (checked) {
                                    Some(Conjugator.set_ventive(verb))
                                } else {
                                    Some(Conjugator.reset_ventive(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | Comitative => {
                    set_comitative(_ => checked)
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                if (checked) {
                                    Some(Conjugator.set_comitative(
                                        verb, 
                                        initial_person_prefix |> Js.Nullable.toOption)
                                    )
                                } else {
                                    Some(Conjugator.reset_comitative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | Ablative => {
                    set_ablative(_ => checked)
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                if (checked) {
                                    Some(Conjugator.set_ablative(verb, initial_person_prefix |> Js.Nullable.toOption))
                                } else {
                                    Some(Conjugator.reset_ablative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | Terminative => {
                    set_terminative(_ => checked)
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                if (checked) {
                                    Some(Conjugator.set_terminative(verb, initial_person_prefix |> Js.Nullable.toOption))
                                } else {
                                    Some(Conjugator.reset_terminative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | MiddlePrefix => {
                    set_middle_prefix(_ => checked)
                    set_verb_form(prev_verb_form => {
                        switch prev_verb_form {
                            | Some(verb) => {
                                set_error(_ => None)
                                if (checked) {
                                    Some(Conjugator.set_middle_prefix(verb))
                                } else {
                                    Some(Conjugator.reset_middle_prefix(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | LocativeIn => {
                    set_locative(_ => if (checked) { Some("IN") } else { None })
                    set_verb_form(prev_verb_form => {
                        set_error(_ => None)
                        switch prev_verb_form {
                            | Some(verb) => {
                                if (checked) {
                                    Some(Conjugator.set_locative_in(verb, None))
                                } else {
                                    Some(Conjugator.reset_locative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | LocativeOn => {
                    set_locative(_ => if (checked) { Some("ON") } else { None })
                    set_verb_form(prev_verb_form => {
                        set_error(_ => None)
                        switch prev_verb_form {
                            | Some(verb) => {
                                if (checked) {
                                    Some(Conjugator.set_locative_on(verb, None))
                                } else {
                                    Some(Conjugator.reset_locative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
            }
        }
    };

    <>
        <div className=css##conjugator>
            <div className=css##selectors>
                <div className=css##firstColumn>
                    <div>
                        <span>
                            {"Verb Stem" |> React.string}
                        </span>
                        <ReactSelect 
                            options={verb_options}
                            value={verb_stem} 
                            onChange={set_new_verb_stem} 
                            isDisabled={false}
                        />
                    </div>
                    <div>
                        <p>
                            {"Aspect" |> React.string}
                        </p>
                        <div className=css##withLabels>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="perfective" 
                                    value="is_perfective" 
                                    checked={switch is_perfective {
                                        | Some(true) => true
                                        | Some(false) => false
                                        | None => false
                                    }}
                                    onChange={_ => {
                                        set_verb_form(prev_verb_form => {
                                            switch prev_verb_form {
                                                | Some(verb) => {
                                                    set_error(_ => None)
                                                    set_is_perfective(_ => Some(true))
                                                    // finds the perfective stem
                                                    let stem = switch (verb_stem |> Js.Nullable.toOption,) {
                                                        | Some(verb_data) => verb_data.value
                                                        | None => ""
                                                    }

                                                    switch (subject |> Js.Nullable.toOption, object_ |> Js.Nullable.toOption) {
                                                        | (Some(subj), Some(obj)) => {
                                                            try (verb
                                                            ->Conjugator.set_stem(stem)
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_perfective
                                                            ->Conjugator.set_subject(subj)
                                                            ->Result.get_ok
                                                            ->Conjugator.set_object(obj)
                                                            ->Some) {
                                                                | Conjugator__Utils.Todo(err) => {
                                                                    set_error(_ => Some(err))
                                                                    prev_verb_form
                                                                }
                                                            }
                                                        }
                                                        | (Some(subj), _) => {
                                                            try (verb
                                                            ->Conjugator.set_stem(stem)
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_perfective
                                                            ->Conjugator.set_subject(subj)
                                                            ->Result.get_ok
                                                            ->Some) {
                                                                | Conjugator__Utils.Todo(err) => {
                                                                    set_error(_ => Some(err))
                                                                    prev_verb_form
                                                                }
                                                            }
                                                        }
                                                        | (_, Some(obj)) => 
                                                            verb
                                                            ->Conjugator.set_stem(stem)
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_perfective
                                                            ->Conjugator.set_object(obj)
                                                            ->Some
                                                        | _ => verb
                                                            ->Conjugator.set_stem(stem)
                                                            ->Conjugator.is_perfective
                                                            ->Some
                                                    }
                                                }
                                                | None => None
                                            }
                                        })
                                    }} 
                                />
                                {"Perfective" |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="perfective" 
                                    value="isImperfective"
                                    checked={switch is_perfective {
                                        | Some(true) => false
                                        | Some(false) => true
                                        | None => false
                                    }}
                                    onChange={_ => {
                                        set_verb_form(prev_verb_form => {
                                            switch prev_verb_form {
                                                | Some(verb) => {
                                                    set_error(_ => None)
                                                    set_is_perfective(_ => Some(false))
                                                    // finds the imperfective stem
                                                    let ipfv_stem = switch (Array.find_opt(
                                                        av_verb => av_verb.value == verb.stem, 
                                                        available_verbs
                                                    )) {
                                                        | Some(verb_data) => Some(verb_data.imperfective)
                                                        | None => None
                                                    }

                                                    switch (subject |> Js.Nullable.toOption, object_ |> Js.Nullable.toOption) {
                                                        | (Some(subj), Some(obj)) => {
                                                            try (verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_imperfective(ipfv_stem)
                                                            ->Conjugator.set_subject(subj)
                                                            ->Result.get_ok
                                                            ->Conjugator.set_object(obj)
                                                            ->Some) {
                                                                | Conjugator__Utils.Todo(err) => {
                                                                    set_error(_ => Some(err))
                                                                    prev_verb_form
                                                                }
                                                            }
                                                        }
                                                        | (Some(subj), _) => 
                                                            try (verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_imperfective(ipfv_stem)
                                                            ->Conjugator.set_subject(subj)
                                                            ->Result.get_ok
                                                            ->Some) {
                                                                | Conjugator__Utils.Todo(err) => {
                                                                    set_error(_ => Some(err))
                                                                    prev_verb_form
                                                                }
                                                            }
                                                        | (_, Some(obj)) => 
                                                            verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_imperfective(ipfv_stem)
                                                            ->Conjugator.set_object(obj)
                                                            ->Some
                                                        | _ => Some(Conjugator.is_imperfective(verb, ipfv_stem))
                                                    }
                                                }
                                                | None => None
                                            }
                                        })
                                    }} 
                                />
                                {"Imperfective" |> React.string}
                            </label>
                        </div>
                    </div>
                    <div>
                        <p>
                            {"Transitivity" |> React.string}
                        </p>
                        <div className=css##withLabels>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="transitivity" 
                                    value="is_transitive" 
                                    checked={switch is_transitive {
                                        | Some(true) => true
                                        | Some(false) => false
                                        | None => false
                                    }}
                                    onChange={_ => {
                                        set_verb_form(prev_verb_form => {
                                            switch prev_verb_form {
                                                | Some(verb) => {
                                                    set_error(_ => None)
                                                    set_is_transitive(_ => Some(true))
                                                    switch (subject |> Js.Nullable.toOption, object_ |> Js.Nullable.toOption) {
                                                        | (Some(subj), Some(obj)) =>
                                                            try (verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_transitive
                                                            ->Conjugator.set_subject(subj)
                                                            ->Result.get_ok
                                                            ->Conjugator.set_object(obj)
                                                            ->Some) {
                                                                | Conjugator__Utils.Todo(err) => {
                                                                    set_error(_ => Some(err))
                                                                    prev_verb_form
                                                                }
                                                            }
                                                        | (Some(subj), _) => 
                                                            try (verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_transitive
                                                            ->Conjugator.set_subject(subj)
                                                            ->Result.get_ok
                                                            ->Some) {
                                                                | Conjugator__Utils.Todo(err) => {
                                                                    set_error(_ => Some(err))
                                                                    prev_verb_form
                                                                }
                                                            }
                                                        | (_, Some(obj)) => 
                                                            verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_transitive
                                                            ->Conjugator.set_object(obj)
                                                            ->Some
                                                        | _ => Some(Conjugator.is_transitive(verb))
                                                    }
                                                }
                                                | None => None
                                            }
                                        })
                                    }} 
                                />
                                {"Transitive" |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="transitivity" 
                                    value="is_intransitive" 
                                    checked={switch is_transitive {
                                        | Some(true) => false
                                        | Some(false) => true
                                        | None => false
                                    }}
                                    onChange={_ => {
                                        set_verb_form(prev_verb_form => {
                                            switch prev_verb_form {
                                                | Some(verb) => {
                                                    set_error(_ => None)
                                                    set_is_transitive(_ => Some(false))
                                                    switch (subject |> Js.Nullable.toOption, object_ |> Js.Nullable.toOption) {
                                                        | (Some(subj), Some(obj)) =>
                                                            try (verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_intransitive
                                                            ->Conjugator.set_subject(subj)
                                                            ->Result.get_ok
                                                            ->Conjugator.set_object(obj)
                                                            ->Some) {
                                                                | Conjugator__Utils.Todo(err) => {
                                                                    set_error(_ => Some(err))
                                                                    prev_verb_form
                                                                }
                                                            }
                                                        | (Some(subj), _) => 
                                                            try (verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_intransitive
                                                            ->Conjugator.set_subject(subj)
                                                            ->Result.get_ok
                                                            ->Some) {
                                                                | Conjugator__Utils.Todo(err) => {
                                                                    set_error(_ => Some(err))
                                                                    prev_verb_form
                                                                }
                                                            }
                                                        | (_, Some(obj)) => 
                                                            verb
                                                            ->Conjugator.reset_subject_object
                                                            ->Conjugator.is_intransitive
                                                            ->Conjugator.set_object(obj)
                                                            ->Some
                                                        | _ => Some(Conjugator.is_intransitive(verb))
                                                    }
                                                }
                                                | None => None
                                            }
                                        })
                                    }} 
                                />
                                {"Intransitive" |> React.string}
                            </label>
                        </div>
                    </div>
                    <div>
                        <span>
                            {"Subject" |> React.string}
                        </span>
                        <ReactSelect 
                            options={pronoun_options} 
                            value={
                                switch (subject |> Js.Nullable.toOption) {
                                    | Some(pp) => ReactSelect.person_param_to_option(pp) |> Js.Nullable.return
                                    | _ => Js.Nullable.null
                                }
                            }
                            onChange={change_pronoun(_, "subject")}
                            isDisabled={
                                is_transitive |> Option.is_none 
                                || is_perfective |> Option.is_none 
                                || Js.Nullable.isNullable(verb_stem)    
                            } 
                        />
                    </div>
                    <div>
                        <span>
                            {"Object" |> React.string}
                        </span>
                        <ReactSelect 
                            options={pronoun_options} 
                            value={
                                switch (object_ |> Js.Nullable.toOption) {
                                    | Some(pp) => ReactSelect.person_param_to_option(pp) |> Js.Nullable.return
                                    | _ => Js.Nullable.null
                                }
                            }
                            onChange={change_pronoun(_, "object")}
                            isDisabled={
                                is_transitive |> Option.is_none 
                                || is_perfective |> Option.is_none 
                                || Js.Nullable.isNullable(verb_stem)    
                            } 
                        />
                    </div>
                    <div>
                        <span>
                            {"Indirect Object" |> React.string}
                        </span>
                        <ReactSelect 
                            options={pronoun_options}
                            value={
                                switch (indirect_object |> Js.Nullable.toOption) {
                                    | Some(pp) => ReactSelect.person_param_to_option(pp) |> Js.Nullable.return
                                    | _ => Js.Nullable.null
                                }
                            }
                            onChange={change_pronoun(_, "indirect-object")}
                            isDisabled={
                                is_transitive |> Option.is_none  
                                || is_perfective |> Option.is_none 
                                || Js.Nullable.isNullable(verb_stem)    
                            } 
                        />
                    </div>
                </div>
                <div className=css##secondColumn>
                    <div>
                        <p>
                            {"Preformative" |> React.string}
                        </p>
                        <div className=css##withLabels>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="preformative" 
                                    value="preformative-a" 
                                    checked={switch preformative {
                                        | Some(Conjugator.Preformative.A) => true
                                        | _ => false
                                    }}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={change_preformative}
                                    onClick={_ => {
                                        set_verb_form(prev_verb_form => {
                                            switch prev_verb_form {
                                                | Some(verb) when preformative == Some(Conjugator.Preformative.A) => {
                                                    set_error(_ => None)
                                                    set_preformative(_ => None)
                                                    Some(Conjugator.reset_preformative(verb))
                                                }
                                                | _ => prev_verb_form
                                            }
                                        })
                                    }}
                                />
                                {"A" |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="preformative" 
                                    value="preformative-i" 
                                    checked={switch preformative {
                                        | Some(Conjugator.Preformative.I) => true
                                        | _ => false
                                    }}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={change_preformative}
                                    onClick={_ => {
                                        set_verb_form(prev_verb_form => {
                                            switch prev_verb_form {
                                                | Some(verb) when preformative == Some(Conjugator.Preformative.I) => {
                                                    set_error(_ => None)
                                                    set_preformative(_ => None)
                                                    Some(Conjugator.reset_preformative(verb))
                                                }
                                                | _ => prev_verb_form
                                            }
                                        })
                                    }}
                                />
                                {"I" |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="preformative" 
                                    value="preformative-u" 
                                    checked={switch preformative {
                                        | Some(Conjugator.Preformative.U) => true
                                        | _ => false
                                    }}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={change_preformative}
                                    onClick={_ => {
                                        set_verb_form(prev_verb_form => {
                                            switch prev_verb_form {
                                                | Some(verb) when preformative == Some(Conjugator.Preformative.U) => {
                                                    set_error(_ => None)
                                                    set_preformative(_ => None)
                                                    Some(Conjugator.reset_preformative(verb))
                                                }
                                                | _ => prev_verb_form
                                            }
                                        })
                                    }}
                                />
                                {"U" |> React.string}
                            </label>
                        </div>
                    </div>
                    <div>
                        <span>
                            {"Initial Person Prefix" |> React.string}
                        </span>
                        <ReactSelect 
                            options={pronoun_options} 
                            value={
                                switch (initial_person_prefix |> Js.Nullable.toOption) {
                                    | Some(pp) => ReactSelect.person_param_to_option(pp) |> Js.Nullable.return
                                    | _ => Js.Nullable.null
                                }
                            }
                            onChange={change_pronoun(_, "initial-person-prefix")}
                            isDisabled={
                                is_transitive |> Option.is_none 
                                || is_perfective |> Option.is_none
                                || Js.Nullable.isNullable(verb_stem)    
                            } 
                        />
                    </div>
                    <div>
                        <p>
                            {"Dimensional Prefixes" |> React.string}
                        </p>
                        <div className=css##withLabels>
                            <label>
                                <input 
                                    type_="checkbox" 
                                    name="comitative" 
                                    value="comitative" 
                                    checked={comitative}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(Comitative, checked)
                                    }}
                                />
                                {"DA" |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="checkbox" 
                                    name="ablative" 
                                    value="ablative" 
                                    checked={ablative}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(Ablative, checked)
                                    }}
                                />
                                {"TA" |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="checkbox" 
                                    name="terminative" 
                                    value="terminative" 
                                    checked={terminative}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(Terminative, checked)
                                    }}
                                />
                                {{js|ŠI|js} |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="checkbox" 
                                    name="locative-in" 
                                    value="locative-in" 
                                    checked={locative === Some("IN")}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(LocativeIn, checked)
                                    }}
                                />
                                {"NI" |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="checkbox" 
                                    name="locative-on" 
                                    value="locative-on" 
                                    checked={locative === Some("ON")}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(LocativeOn, checked)
                                    }}
                                />
                                {"E" |> React.string}
                            </label>
                        </div>
                    </div>
                    <div>
                        <p>
                            {"Modal Prefixes" |> React.string}
                        </p>
                        <div className=css##withLabels>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="modalPrefixes" 
                                    value="modal"
                                    checked={modal_prefix == Some(HA)}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(Modal, checked)
                                    }}
                                />
                                {{js|ḪA|js} |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="modalPrefixes" 
                                    value="negative-nan" 
                                    checked={modal_prefix == Some(NAN)}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(NegativeNan, checked)
                                    }}
                                />
                                {"NAN" |> React.string}
                            </label>
                            <label>
                                <input 
                                    type_="radio" 
                                    name="modalPrefixes" 
                                    value="negative" 
                                    checked={modal_prefix == Some(NU)}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(Negative, checked)
                                    }}
                                />
                                {"NU" |> React.string}
                            </label>
                        </div> 
                    </div>
                    <div>
                        <p>
                            {"Other Prefixes" |> React.string}
                        </p>                   
                        <div className=css##withLabels>   
                            <label>
                                <input 
                                    type_="checkbox" 
                                    name="ventive" 
                                    value="ventive" 
                                    checked={ventive}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(Ventive, checked)
                                    }}
                                />
                                {"MU" |> React.string}
                            </label>                     
                            <label>
                                <input 
                                    type_="checkbox" 
                                    name="middle-prefix" 
                                    value="middle-prefix" 
                                    checked={middle_prefix}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Js.Nullable.isNullable(verb_stem)    
                                    }
                                    onChange={ev => {
                                        let target = React.Event.Form.target(ev)
                                        let checked: bool = target##checked
                                        change_prefix(MiddlePrefix, checked)
                                    }}
                                />
                                {"BA" |> React.string}
                            </label>
                        </div>
                    </div>
                    {
                        switch (verb_stem |> Js.Nullable.toOption) {
                            | Some(stem) => {
                                switch (Web_utils.EpsdDict.get_epsd_link(stem.value)) {
                                | Some(link) => <div>
                                    <p>
                                        <a href={link} target="_blank" className=css##epsdLink>
                                            {"EPSD Link" |> React.string}
                                        </a>
                                    </p>
                                </div>
                                | None => React.null
                            }
                            }
                            | _ => React.null
                        }
                    }
                </div>
            </div>
            <div className=css##result>
                {
                    switch ((verb_form), error) {
                    | (_, Some(err)) => {
                        <span className=css##error>
                            {err |> React.string}
                        </span>
                    }
                    | (Some(verb), None) => <Web_utils.BuildResults verb={verb} />
                    | (None, None) => <span>{"No Selected Verb" |> React.string}</span>
                    }
                }
            </div>
            <div className=css##buttons>
                <button onClick={_ => {
                    set_verb_stem(_ => Js.Nullable.null)
                    set_verb_form(_ => None)
                    set_is_perfective(_ => None)
                    set_is_transitive(_ => None)
                    set_preformative(_ => None)
                    set_modal_prefix(_ => None)
                    set_ventive(_ => false)
                    set_comitative(_ => false)
                    set_ablative(_ => false)
                    set_terminative(_ => false)
                    set_middle_prefix(_ => false)
                    set_initial_person_prefix(_ => Js.Nullable.null)
                    set_subject(_ => Js.Nullable.null)
                    set_object(_ => Js.Nullable.null)
                    set_indirect_object(_ => Js.Nullable.null)
                    set_error(_ => None)
                }}>
                    {"Clear" |> React.string}
                </button>
                <button>
                    {"Copy" |> React.string}
                </button>
                <button 
                    onClick={_ => 
                    set_is_modal_open(_ => true)}
                    disabled={
                        is_transitive |> Option.is_none 
                        || is_perfective |> Option.is_none
                        || Js.Nullable.isNullable(verb_stem)    
                    }
                    >
                    {"Report an error" |> React.string}
                </button>
            </div>
        </div>
        <Modal is_open={is_modal_open} close={() => set_is_modal_open(_ => false)} >
            <Verb_error_form verb={verb_form} />
        </Modal>
    </>
};