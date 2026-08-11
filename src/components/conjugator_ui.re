[@mel.module "../styles/Conjugator.module.scss"] external css: Js.t({..}) = "default"; 

open Web_utils;

type prefix =
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

let verb_dictionary_value = (verb: verb_data): string =>
    switch verb.kind {
    | Simple => verb.stem
    | Compound(element) => element.value ++ " " ++ verb.stem
    };

let verb_fixed_element = (
    verb: verb_data,
): option((string, array(string))) =>
    switch verb.kind {
    | Simple => None
    | Compound(element) => Some((element.value, element.cuneiforms))
    };

module Utils = {
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
};

[@react.component]
let make = () => {
    open Bindings.Mui;

    let (error, set_error) = React.useState(_ => None);
    let (verb_stem, set_verb_stem) = React.useState(_ => None);
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
    let (subject, set_subject) = React.useState(_ => None);
    let (object_, set_object) = React.useState(_ => None);
    let (indirect_object, set_indirect_object) = React.useState(_ => None);
    let (is_modal_open, set_is_modal_open) = React.useState(_ => false);

    let marginTop = "20px";
    let is_mobile = UseMediaQuery.use("(max-width:599px)");

    let available_verbs: array(verb_data) = SumerianVerbs.verbs;

    let pronoun_options: array(Utils.select_option) = [|
        {label: "I", value: "first-sing"},
        {label: "You (sing)", value: "second-sing"},
        {label: "He/She", value: "third-sing-human"},
        {label: "It", value: "third-sing-nonhuman"},
        {label: "We", value: "first-plur"},
        {label: "You (plur)", value: "second-plur"},
        {label: "They (human)", value: "third-plur-human"},
        {label: "They (non-human)", value: "third-plur-nonhuman"},
    |];

    let pronoun_object_options: array(Utils.select_option) = [|
        {label: "Me", value: "first-sing"},
        {label: "You (sing)", value: "second-sing"},
        {label: "Him/Her", value: "third-sing-human"},
        {label: "It", value: "third-sing-nonhuman"},
        {label: "Us", value: "first-plur"},
        {label: "You (plur)", value: "second-plur"},
        {label: "Them (human)", value: "third-plur-human"},
        {label: "Them (non-human)", value: "third-plur-nonhuman"},
    |];

    let change_pronoun = (value: option(Utils.select_option), pronoun: string) => {
        if (Option.is_none(is_perfective) && Option.is_none(is_transitive)) {
            set_error(_ => Some("Aspect and transitivity must be selected"))
        } else {
            switch value {
                | None => {
                    switch pronoun {
                    | "initial-person-prefix" => {
                        set_initial_person_prefix(_ => Js.Nullable.null)
                        set_verb_form(prev_verb_form => {
                            switch prev_verb_form {
                                | Some(verb) => Some(Conjugator.reset_initial_person_prefix(verb))
                                | None => None
                            }
                        })
                    }
                    | "subject" => {
                        set_subject(_ => None)
                        set_verb_form(prev_verb_form => {
                            switch prev_verb_form {
                                | Some(verb) => Some(Conjugator.reset_subject(verb))
                                | None => None
                            }
                        })
                    }
                    | "object" => {
                        set_object(_ => None)
                        set_verb_form(prev_verb_form => {
                            switch prev_verb_form {
                                | Some(verb) => Some(Conjugator.reset_object(verb))
                                | None => None
                            }
                        })
                    }
                    | "indirect-object" => {
                        set_indirect_object(_ => None)
                        set_verb_form(prev_verb_form => {
                            switch prev_verb_form {
                                | Some(verb) => Some(Conjugator.reset_indirect_object(verb))
                                | None => None
                            }
                        })
                    }
                    | _ => ()
                    }
                }
                | Some(value) => {
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
                                    set_subject(_ => Some(person_param))
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
                                    set_object(_ => Some(person_param))
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
                                    set_indirect_object(_ => Some(person_param))
                                    Some(Conjugator.set_indirect_object(verb, person_param))
                                }
                                | None => None
                            }
                        })
                    }
                    | _ => ()
                }
                }
            }
        }
    };

    let change_preformative = value => {
        if (Option.is_none(verb_stem)) {
            set_error(_ => Some("No verb stem selected"))
        } else if (Option.is_none(is_perfective) && Option.is_none(is_transitive)) {
            set_error(_ => Some("Aspect and transitivity must be selected"))
        } else {
            set_verb_form(prev_verb_form => {
                switch (prev_verb_form, value) {
                    | (Some(verb), Some(selected_preformative)) => {
                        set_error(_ => None)     
                        set_preformative(_ => Some(selected_preformative))
                        Some(Conjugator.set_preformative(verb, selected_preformative))
                    }
                    | (Some(verb), None) => {
                        set_error(_ => None)
                        set_preformative(_ => None)
                        Some(Conjugator.reset_preformative(verb))
                    }
                    | (None, _) => {
                        set_error(_ => Some("No verb stem selected"))
                        None
                    }
                }
            })
        }
    };

    let change_modal = (value: option(modal_prefix)) => {
        if (Option.is_none(verb_stem)) {
            set_error(_ => Some("No verb stem selected"))
        } else if (Option.is_none(is_perfective) && Option.is_none(is_transitive)) {
            set_error(_ => Some("Aspect and transitivity must be selected"))
        } else {
            set_verb_form(prev_verb_form => {
                switch (prev_verb_form, value) {
                    | (Some(verb), Some(modal_prefix)) => {
                        set_error(_ => None)
                        set_modal_prefix(_ => Some(modal_prefix))
                        let selected_modal =
                            switch modal_prefix {
                            | HA => Conjugator.FirstPrefix.Modal
                            | NU => Conjugator.FirstPrefix.Negative
                            | NAN => Conjugator.FirstPrefix.Negative_nan
                            };
                        Some(Conjugator.set_modal(verb, selected_modal))
                    }
                    | (Some(verb), None) => {
                        set_error(_ => None)
                        set_modal_prefix(_ => None)
                        Some(Conjugator.reset_modal(verb))
                    }
                    | (None, _) => {
                        set_error(_ => Some("No verb stem selected"))
                        None
                    }
                }
            })
        }
    };

    let change_prefix = (value: prefix, checked: bool) => {
        if (Option.is_none(verb_stem)) {
            set_error(_ => Some("No verb stem selected"))
        } else if (Option.is_none(is_perfective) && Option.is_none(is_transitive)) {
            set_error(_ => Some("Aspect and transitivity must be selected"))
        } else {
            switch value {
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
                                    set_terminative(_ => false)
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
                                    set_ablative(_ => false)
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

    let reset = () => {
        set_verb_stem(_ => None)
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
        set_subject(_ => None)
        set_object(_ => None)
        set_indirect_object(_ => None)
        set_error(_ => None)
    };

    let set_new_verb_stem = (value: option(verb_data)): unit => {
        reset();
        set_verb_stem(_ => value);
        switch value {
        | Some(verb_data) => {
            let new_verb = Conjugator.create(verb_data.stem);
            let new_verb =
                verb_data.transitive
                    ? Conjugator.is_transitive(new_verb)
                    : Conjugator.is_intransitive(new_verb);
            let new_verb = Conjugator.is_perfective(new_verb);
            set_verb_form(_ => Some(new_verb));
            set_is_transitive(_ => Some(verb_data.transitive));
            set_is_perfective(_ => Some(true));
            set_error(_ => None);
        }
        | None => ()
        };
    };

    let switch_transitive = (checked: option(bool)) => {
        let apply_transitivity = verb =>
            switch checked {
            | Some(true) => Conjugator.is_transitive(verb)
            | Some(false) => Conjugator.is_intransitive(verb)
            | None => verb
            };

        set_verb_form(prev_verb_form => {
            switch prev_verb_form {
            | Some(verb) => {
                set_error(_ => None)
                set_is_transitive(_ => checked)
                switch (subject, object_) {
                    | (Some(subj), Some(obj)) =>
                        try (verb
                        ->Conjugator.reset_subject_object
                        ->apply_transitivity
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
                        ->apply_transitivity
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
                        ->apply_transitivity
                        ->Conjugator.set_object(obj)
                        ->Some
                    | _ => Some(apply_transitivity(verb))
                }
            }
            | None => None
            }
        })
    };

    let switch_perfective = (value: option(bool)) =>
        switch value {
        // An exclusive ToggleButtonGroup returns None when its selected button
        // is clicked again. Keep the current aspect in that case.
        | None => ()
        | Some(_) =>
            set_verb_form(prev_verb_form => {
                switch prev_verb_form {
                | Some(verb) => {
                    set_error(_ => None)
                    set_is_perfective(_ => value)

                    let apply_aspect = verb =>
                        switch (value, verb_stem) {
                        | (Some(true), Some(verb_data)) =>
                            verb
                            ->Conjugator.set_stem(verb_data.stem)
                            ->Conjugator.is_perfective
                        | (Some(false), Some(verb_data)) =>
                            verb
                            ->Conjugator.set_stem(verb_data.stem)
                            ->Conjugator.is_imperfective(Some(verb_data.imperfective))
                        | _ => verb
                        };

                    switch (subject, object_) {
                        | (Some(subj), Some(obj)) => {
                            try (verb
                            ->Conjugator.reset_subject_object
                            ->apply_aspect
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
                            ->Conjugator.reset_subject_object
                            ->apply_aspect
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
                            ->Conjugator.reset_subject_object
                            ->apply_aspect
                            ->Conjugator.set_object(obj)
                            ->Some
                        | _ => verb
                            ->apply_aspect
                            ->Some
                    }
                }
                | None => None
                }
            })
        };

    let copy_result_cuneiforms = () => {
        switch (verb_form, verb_stem) {
        | (Some(verb), Some(selected_verb)) => {
            switch (Conjugator.print(verb, None)) {
            | Ok({verb: conjugated_verb, _}) => {
                let cuneiforms =
                    Web_utils.build_result_cuneiform_string(
                        conjugated_verb,
                        verb.stem,
                        selected_verb.stem,
                        selected_verb.stem_cuneiforms,
                        Some(selected_verb.imperfective),
                        verb_fixed_element(selected_verb),
                    );
                let _ =
                    cuneiforms
                    |> Bindings.Browser.Clipboard.write_text
                    |> Js.Promise.catch(error => {
                        Js.log2(
                            "Could not copy the conjugated cuneiforms:",
                            error,
                        );
                        Js.Promise.resolve();
                    });
                ()
            }
            | Error(error) =>
                Js.log2(
                    "Could not generate the conjugated cuneiforms:",
                    error,
                )
            }
        }
        | _ => ()
        }
    };

    <Container className=css##mainContainer>
        <h1>{"Sumerian Verb Conjugator"|>React.string}</h1>
        <Grid 
            container=true
            spacing=`Object(Grid.ResponsiveSize.make(~xs=2, ~sm=2, ~md=6, ()))  
            sx={{"marginTop": marginTop}}
        >
            <Grid 
                size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=12, ~md=6, ()))
            >
                <Box sx={{"display": "flex", "alignItems": "center", "gap": "10px"}}>
                    <Autocomplete
                        autoHighlight=true
                        options=available_verbs
                        groupBy={verb => verb.firstLetter |> Js.String.toUpperCase}
                        getOptionLabel=(verb => verb.label)
                        value={
                            switch verb_stem {
                            | Some(verb) => Js.Nullable.return(verb)
                            | None => Js.Nullable.null
                            }
                        }
                        onChange={(_event, newValue) =>
                            set_new_verb_stem(
                                newValue |> Js.Nullable.toOption,
                            )
                        }
                        renderInput={params =>
                            React.cloneElement(
                                <TextField label={"Verb Stem" |> React.string} />,
                                params,
                            )
                        }
                        renderOption={(props, option, _state, _ownerState) =>
                            React.cloneElement(
                                <li key={verb_dictionary_value(option) ++ option.label}>
                                    {
                                    switch option.kind {
                                    | Simple => option.stem_cuneiforms
                                    | Compound(element) =>
                                        Array.concat([
                                            element.cuneiforms,
                                            option.stem_cuneiforms,
                                        ])
                                    }
                                    |> Array.mapi((index, cuneiform) =>
                                        <span
                                            key={
                                                verb_dictionary_value(option)
                                                ++ "-"
                                                ++ Int.to_string(index)
                                                ++ "-"
                                                ++ cuneiform
                                            }
                                            className="cuneiforms x-small"
                                        >
                                            {cuneiform |> React.string}
                                        </span>
                                    )
                                    |> React.array}
                                    <span style={ReactDOM.Style.make(~marginLeft="15px", ())}>
                                        {(option.label ++ " (to " ++ option.meaning ++ ")") |> React.string}
                                    </span>
                                </li>,
                                props,
                            )
                        }
                        sx={{"width": "100%", "backgroundColor": "white"}}
                        size=`small
                    />
                    <span className=css##noWrap>
                        {
                            switch (verb_stem) {
                                | Some(stem) => {
                                    switch (
                                        Web_utils.EpsdDict.get_epsd_link(
                                            verb_dictionary_value(stem),
                                        )
                                    ) {
                                    | Some(link) => 
                                        <a href={link} target="_blank" className=css##epsdLink>
                                            {"EPSD Link" |> React.string}
                                        </a>
                                    | None => React.null
                                }
                                }
                                | _ => React.null
                            }
                        }
                    </span>
                </Box>
                <Grid container=true spacing=`Number(2) sx={{"marginTop": marginTop}}>
                    // DESKTOP VIEW
                    <Grid size=`Number(6) className=css##onlyDesktop>
                        <InputLabel id="transitivity-label">
                            {"Transitivity" |> React.string}
                        </InputLabel>
                        <ToggleButtonGroup
                            value=is_transitive
                            exclusive=true
                            color=Color.primary
                            onChange={(_event, checked) => switch_transitive(checked)} 
                        >
                            <ToggleButton value=Some(true) size=`small>
                                {"Transitive" |> React.string}
                            </ToggleButton>
                            <ToggleButton value=Some(false) size=`small>
                                {"Intransitive" |> React.string}
                            </ToggleButton>
                        </ToggleButtonGroup>
                    </Grid>
                    <Grid size=`Number(6) className=css##onlyDesktop>
                        <InputLabel id="aspect-label">
                            {"Aspect" |> React.string}
                        </InputLabel>
                        <ToggleButtonGroup
                            value=is_perfective
                            exclusive=true
                            color=Color.primary
                            onChange={(_event, value) => switch_perfective(value)}
                        >
                            <ToggleButton value=Some(true) size=`small>
                                {"Perfective" |> React.string}
                            </ToggleButton>
                            <ToggleButton value=Some(false) size=`small>
                                {"Imperfective" |> React.string}
                            </ToggleButton>
                        </ToggleButtonGroup>
                    </Grid>
                    // MOBILE VIEW
                    <Grid size=`Number(6) className=css##onlyMobile>
                        <FormGroup>
                            <FormControlLabel
                                control={
                                    <Switch
                                        checked={is_transitive |> Option.value(~default=false)}
                                        disabled={verb_stem |> Option.is_none}
                                        onChange={event => {
                                            let checked =
                                                React.Event.Form.target(event)##checked;
                                            switch_transitive(Some(checked));
                                        }}
                                    />
                                }
                                label={
                                    switch is_transitive {
                                    | Some(true) => "Transitive"
                                    | Some(false) => "Intransitive"
                                    | None => "Transitivity"
                                    } |> React.string
                                }
                            />
                        </FormGroup>
                    </Grid>
                    <Grid size=`Number(6) className=css##onlyMobile>
                        <FormGroup >
                            <FormControlLabel
                                control={
                                    <Switch
                                        checked={is_perfective |> Option.value(~default=false)}
                                        disabled={verb_stem |> Option.is_none}
                                        onChange={event => {
                                            let checked =
                                                React.Event.Form.target(event)##checked;
                                            switch_perfective(Some(checked));
                                        }}
                                    />
                                }
                                label={
                                    switch is_perfective {
                                    | Some(true) => "Perfective"
                                    | Some(false) => "Imperfective"
                                    | None => "Aspect"
                                    } |> React.string
                                }
                            />
                        </FormGroup>
                    </Grid>
                </Grid>
                <Grid 
                    container=true 
                    spacing=`Number(2) 
                    direction=`row
                    sx={{"width": "100%", "justifyContent": "space-between", "alignItems": "center", "marginTop": marginTop}}
                >
                    <Grid size=`Number(4)>
                        <FormControl 
                            fullWidth=true 
                            size=`small 
                            disabled={verb_stem |> Option.is_none}
                        >
                            <InputLabel id="subject-label">
                                {"Subject" |> React.string}
                            </InputLabel>
                            <Select
                                label={"Subject" |> React.string}
                                labelId="subject-label"
                                value={
                                    switch subject {
                                    | Some(pp) =>
                                        pp
                                        |> Utils.person_param_to_option
                                        |> option =>
                                            Select.Value.fromString(option.value)
                                    | None => Select.Value.fromString("")
                                    }
                                }
                                onChange={(event, _) => {
                                    let selected_value = event##target##value;
                                    switch (
                                        pronoun_options
                                        |> Array.find_opt(
                                            (option: Utils.select_option) =>
                                                option.value === selected_value
                                        )
                                    ) {
                                    | Some(option) =>
                                        change_pronoun(Some(option), "subject")
                                    | None => change_pronoun(None, "subject")
                                    };
                                }}
                                sx={{"backgroundColor": "white"}}
                            >
                                <MenuItem 
                                    value="" 
                                    key="none"
                                >
                                    <i>{"None" |> React.string}</i>
                                </MenuItem>
                                {
                                    pronoun_options
                                    |> Array.map((option: Utils.select_option) => {
                                        <MenuItem value=option.value key=option.value>
                                            {option.label |> React.string}
                                        </MenuItem>
                                    })
                                    |> React.array
                                }
                            </Select>
                        </FormControl>
                    </Grid>
                    <Grid size=`Number(4)>
                        <FormControl 
                            fullWidth=true 
                            size=`small
                            disabled={
                                (verb_stem |> Option.is_none) 
                                || (is_transitive |> Option.is_none) 
                                || (is_transitive === Option.Some(false))
                            }
                        >
                            <InputLabel id="object-label">
                                {"Object" |> React.string}
                            </InputLabel>
                            <Select
                                label={"Object" |> React.string}
                                labelId="object-label"
                                value={
                                    switch object_ {
                                    | Some(pp) =>
                                        pp
                                        |> Utils.person_param_to_option
                                        |> option =>
                                            Select.Value.fromString(option.value)
                                    | None => Select.Value.fromString("")
                                    }
                                }
                                onChange={(event, _) => {
                                    let selected_value = event##target##value;
                                    switch (
                                        pronoun_options
                                        |> Array.find_opt(
                                            (option: Utils.select_option) =>
                                                option.value === selected_value
                                        )
                                    ) {
                                    | Some(option) =>
                                        change_pronoun(Some(option), "object")
                                    | None => change_pronoun(None, "object")
                                    };
                                }}
                                sx={{"backgroundColor": "white"}}
                            >
                                <MenuItem 
                                    value="" 
                                    key="none"
                                >
                                    <i>{"None" |> React.string}</i>
                                </MenuItem>
                                {
                                    pronoun_options
                                    |> Array.map((option: Utils.select_option) => {
                                        <MenuItem value=option.value key=option.value>
                                            {option.label |> React.string}
                                        </MenuItem>
                                    })
                                    |> React.array
                                }
                            </Select>
                        </FormControl>
                    </Grid>
                    <Grid size=`Number(4)>
                        <FormControl 
                            fullWidth=true 
                            size=`small
                            disabled={verb_stem |> Option.is_none}
                        >
                            <InputLabel id="indirect-object-label">
                                {"Indirect Object" |> React.string}
                            </InputLabel>
                            <Select
                                label={"Indirect Object" |> React.string}
                                labelId="indirect-object-label"                                
                                value={
                                    switch indirect_object {
                                    | Some(pp) =>
                                        pp
                                        |> Utils.person_param_to_option
                                        |> option =>
                                            Select.Value.fromString(option.value)
                                    | None => Select.Value.fromString("")
                                    }
                                }
                                onChange={(event, _) => {
                                    let selected_value = event##target##value;
                                    switch (
                                        pronoun_options
                                        |> Array.find_opt(
                                            (option: Utils.select_option) =>
                                                option.value === selected_value
                                        )
                                    ) {
                                    | Some(option) =>
                                        change_pronoun(Some(option), "indirect-object")
                                    | None =>
                                        change_pronoun(None, "indirect-object")
                                    };
                                }}
                                sx={{"backgroundColor": "white"}}
                            >
                                <MenuItem 
                                    value="" 
                                    key="none"
                                >
                                    <i>{"None" |> React.string}</i>
                                </MenuItem>
                                {
                                    pronoun_options
                                    |> Array.map((option: Utils.select_option) => {
                                        <MenuItem value=option.value key=option.value>
                                            {option.label |> React.string}
                                        </MenuItem>
                                    })
                                    |> React.array
                                }
                            </Select>
                        </FormControl>
                    </Grid>
                </Grid>
                
            </Grid>
            <Grid 
                size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=12, ~md=6, ()))
            >
                <Grid container=true spacing=`Number(2)>
                    <Grid size=`Number(6)>
                        <FormControl>
                            <FormLabel id="preformative-label">
                                {"Preformative" |> React.string}
                            </FormLabel>
                            <RadioGroup
                                row=true
                                ariaLabelledby="preformative-label"
                                name="preformative"
                                value={
                                    switch preformative {
                                    | Some(Conjugator.Preformative.A) => "preformative-a"
                                    | Some(Conjugator.Preformative.I) => "preformative-i"
                                    | Some(Conjugator.Preformative.U) => "preformative-u"
                                    | None => ""
                                    }
                                }
                                onChange={ev => {
                                    let target = React.Event.Form.target(ev)
                                    switch target##value {
                                    | "preformative-a" =>
                                        change_preformative(Some(Conjugator.Preformative.A))
                                    | "preformative-i" =>
                                        change_preformative(Some(Conjugator.Preformative.I))
                                    | "preformative-u" =>
                                        change_preformative(Some(Conjugator.Preformative.U))
                                    | _ => ()
                                    }
                                }}
                                sx={{"padding": "8px"}}
                            >
                                <FormControlLabel
                                    value="preformative-a"
                                    control={
                                        <Radio
                                            size={is_mobile ? `small : `medium}
                                            onClick={_ =>
                                                switch preformative {
                                                | Some(Conjugator.Preformative.A) =>
                                                    change_preformative(None)
                                                | _ => ()
                                                }
                                            }
                                        />
                                    }
                                    label={"A" |> React.string}                                    
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                                <FormControlLabel
                                    value="preformative-i"
                                    control={
                                        <Radio
                                            size={is_mobile ? `small : `medium}
                                            onClick={_ =>
                                                switch preformative {
                                                | Some(Conjugator.Preformative.I) =>
                                                    change_preformative(None)
                                                | _ => ()
                                                }
                                            }
                                        />
                                    }
                                    label={"I" |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                                <FormControlLabel
                                    value="preformative-u"
                                    control={
                                        <Radio
                                            size={is_mobile ? `small : `medium}
                                            onClick={_ =>
                                                switch preformative {
                                                | Some(Conjugator.Preformative.U) =>
                                                    change_preformative(None)
                                                | _ => ()
                                                }
                                            }
                                        />
                                    }
                                    label={"U" |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                            </RadioGroup>
                        </FormControl>
                    </Grid>
                    <Grid size=`Number(6)>
                        <FormControl>
                            <FormLabel id="modal-prefix-label">
                                {"Modal Prefix" |> React.string}
                            </FormLabel>
                            <RadioGroup
                                row=true
                                ariaLabelledby="modal-prefix-label"
                                name="modal-prefix"
                                value={
                                    switch modal_prefix {
                                    | Some(HA) => "modal-ha"
                                    | Some(NU) => "modal-nu"
                                    | Some(NAN) => "modal-nan"
                                    | None => ""
                                    }
                                }
                                onChange={ev => {
                                    let target = React.Event.Form.target(ev)
                                    switch target##value {
                                    | "modal-ha" => change_modal(Some(HA))
                                    | "modal-nu" => change_modal(Some(NU))
                                    | "modal-nan" => change_modal(Some(NAN))
                                    | _ => ()
                                    }
                                }}
                                sx={{"padding": "8px"}}
                            >
                                <FormControlLabel
                                    value="modal-ha"
                                    control={
                                        <Radio
                                            size={is_mobile ? `small : `medium}
                                            onClick={_ =>
                                                switch modal_prefix {
                                                | Some(HA) => change_modal(None)
                                                | _ => ()
                                                }
                                            }
                                        />
                                    }
                                    label={{js|ḪA|js} |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                                <FormControlLabel
                                    value="modal-nu"
                                    control={
                                        <Radio
                                            size={is_mobile ? `small : `medium}
                                            onClick={_ =>
                                                switch modal_prefix {
                                                | Some(NU) => change_modal(None)
                                                | _ => ()
                                                }
                                            }
                                        />
                                    }
                                    label={"NU" |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                                <FormControlLabel
                                    value="modal-nan"
                                    control={
                                        <Radio
                                            size={is_mobile ? `small : `medium}
                                            onClick={_ =>
                                                switch modal_prefix {
                                                | Some(NAN) => change_modal(None)
                                                | _ => ()
                                                }
                                            }
                                        />
                                    }
                                    label={"NAN" |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                            </RadioGroup>
                        </FormControl>
                    </Grid>
                </Grid>
                <Grid container=true spacing=`Number(0)>
                    <Grid size=`Number(8)>
                        <FormControl fullWidth=true>
                            <FormLabel id="dimensional-prefixes">
                                {"Dimensional Prefixes" |> React.string}
                            </FormLabel>
                            <FormGroup
                                row=true
                                ariaLabelledby="dimensional-prefixes"
                            >
                            <FormControlLabel 
                                control={
                                    <Checkbox 
                                        checked={comitative}
                                        size={is_mobile ? `small : `medium}
                                        onChange={ev => {
                                            let target = React.Event.Form.target(ev)
                                            let checked: bool = target##checked
                                            change_prefix(Comitative, checked)
                                        }}
                                        disabled={
                                            is_transitive |> Option.is_none 
                                            || is_perfective |> Option.is_none
                                            || Option.is_none(verb_stem)    
                                        }
                                    />
                                }
                                label={"DA" |> React.string}
                            />
                            <FormControlLabel 
                                control={
                                    <Checkbox 
                                        checked={ablative}
                                        size={is_mobile ? `small : `medium}
                                        onChange={ev => {
                                            let target = React.Event.Form.target(ev)
                                            let checked: bool = target##checked
                                            change_prefix(Ablative, checked)
                                        }}
                                        disabled={
                                            is_transitive |> Option.is_none 
                                            || is_perfective |> Option.is_none
                                            || Option.is_none(verb_stem)    
                                        }
                                    />
                                }
                                label={"TA" |> React.string}
                            />
                            <FormControlLabel 
                                control={
                                    <Checkbox 
                                        checked={terminative}
                                        size={is_mobile ? `small : `medium}
                                        onChange={ev => {
                                            let target = React.Event.Form.target(ev)
                                            let checked: bool = target##checked
                                            change_prefix(Terminative, checked)
                                        }}
                                        disabled={
                                            is_transitive |> Option.is_none 
                                            || is_perfective |> Option.is_none
                                            || Option.is_none(verb_stem)    
                                        }
                                    />
                                }
                                label={{js|ŠI|js} |> React.string}
                            />
                            <FormControlLabel 
                                control={
                                    <Checkbox 
                                        checked={locative === Some("IN")}
                                        size={is_mobile ? `small : `medium}
                                        onChange={ev => {
                                            let target = React.Event.Form.target(ev)
                                            let checked: bool = target##checked
                                            change_prefix(LocativeIn, checked)
                                        }}
                                        disabled={
                                            is_transitive |> Option.is_none 
                                            || is_perfective |> Option.is_none
                                            || Option.is_none(verb_stem)    
                                        }
                                    />
                                }
                                label={"NI" |> React.string}
                            />
                            <FormControlLabel 
                                control={
                                    <Checkbox 
                                        checked={locative === Some("ON")}
                                        size={is_mobile ? `small : `medium}
                                        onChange={ev => {
                                            let target = React.Event.Form.target(ev)
                                            let checked: bool = target##checked
                                            change_prefix(LocativeOn, checked)
                                        }}
                                        disabled={
                                            is_transitive |> Option.is_none 
                                            || is_perfective |> Option.is_none
                                            || Option.is_none(verb_stem)    
                                        }
                                    />
                                }
                                label={"E" |> React.string}
                            />
                            </FormGroup>
                        </FormControl>
                    </Grid>
                    <Grid size=`Number(4)>
                        <FormControl 
                            fullWidth=true 
                            size=`small 
                            error={
                                (comitative || ablative || terminative || locative === Some("IN") || locative === Some("ON"))
                                && Js.Nullable.toOption(initial_person_prefix) |> Option.is_none
                            }
                            disabled={
                                (is_transitive |> Option.is_none) 
                                || (is_perfective |> Option.is_none)
                                || (verb_stem |> Option.is_none)
                                || (!comitative && !ablative && !terminative && (locative |> Option.is_none))
                            }
                        >
                            <InputLabel id="initial-person-prefix-label">
                                {"Initial Person Prefix" |> React.string}
                            </InputLabel>
                            <Select
                                label={"Initial Person Prefix" |> React.string}
                                labelId="initial-person-prefix-label"
                                value={
                                    switch (initial_person_prefix |> Js.Nullable.toOption) {
                                    | Some(pp) =>
                                        pp
                                        |> Utils.person_param_to_option
                                        |> option =>
                                            Select.Value.fromString(option.value)
                                    | None => Select.Value.fromString("")
                                    }
                                }
                                onChange={(event, _) => {
                                    let selected_value = event##target##value;
                                    switch (
                                        pronoun_object_options
                                        |> Array.find_opt(
                                            (option: Utils.select_option) =>
                                                option.value === selected_value
                                        )
                                    ) {
                                    | Some(option) =>
                                        change_pronoun(
                                            Some(option),
                                            "initial-person-prefix",
                                        )
                                    | None => ()
                                    };
                                }}
                                sx={{"backgroundColor": "white"}}
                            >
                                {
                                    pronoun_object_options
                                    |> Array.map((option: Utils.select_option) => {
                                        <MenuItem value=option.value key=option.value>
                                            {option.label |> React.string}
                                        </MenuItem>
                                    })
                                    |> React.array
                                }
                            </Select>
                        </FormControl>
                    </Grid>
                </Grid>
                <Box sx={{"marginTop": marginTop}}>
                    <FormControl fullWidth=true>
                        <FormLabel id="other-prefixes-label">
                            {"Other Prefixes" |> React.string}
                        </FormLabel>
                        <FormGroup
                            row=true
                            ariaLabelledby="other-prefixes-label"
                        >
                            <FormControlLabel 
                                control={
                                    <Checkbox 
                                        checked={ventive}
                                        size={is_mobile ? `small : `medium}
                                        onChange={ev => {
                                            let target = React.Event.Form.target(ev)
                                            let checked: bool = target##checked
                                            change_prefix(Ventive, checked)
                                        }}
                                        disabled={
                                            is_transitive |> Option.is_none 
                                            || is_perfective |> Option.is_none
                                            || Option.is_none(verb_stem)    
                                        }
                                    />
                                }
                                label={"MU" |> React.string}
                            />
                            <FormControlLabel 
                                control={
                                    <Checkbox 
                                        checked={middle_prefix}
                                        size={is_mobile ? `small : `medium}
                                        onChange={ev => {
                                            let target = React.Event.Form.target(ev)
                                            let checked: bool = target##checked
                                            change_prefix(MiddlePrefix, checked)
                                        }}
                                        disabled={
                                            is_transitive |> Option.is_none 
                                            || is_perfective |> Option.is_none
                                            || Option.is_none(verb_stem)    
                                        }
                                    />
                                }
                                label={"BA" |> React.string}
                            />
                        </FormGroup>
                    </FormControl>
                </Box>
            </Grid>
            <Container sx={{"display": "flex", "flexDirection": "column", "alignItems": "center", "marginTop": marginTop}}>
                <div className=css##result>
                    {
                        switch ((verb_form), error) {
                        | (_, Some(err)) => {
                            <span className=css##error>
                                {err |> React.string}
                            </span>
                        }
                        | (Some(verb), None) => 
                            <Web_utils.BuildResults
                                verb={verb}
                                lexicalStem={
                                    switch verb_stem {
                                    | Some(selected_verb) =>
                                        selected_verb.stem
                                    | None => verb.stem
                                    }
                                }
                                stemCuneiforms={
                                    switch verb_stem {
                                    | Some(selected_verb) =>
                                        selected_verb.stem_cuneiforms
                                    | None => [||]
                                    }
                                }
                                imperfectiveStem={
                                    switch verb_stem {
                                    | Some(selected_verb) =>
                                        Some(selected_verb.imperfective)
                                    | None => None
                                    }
                                }
                                fixedElement={
                                    switch verb_stem {
                                    | Some(selected_verb) =>
                                        verb_fixed_element(selected_verb)
                                    | None => None
                                    }
                                }
                                meaning={
                                    switch (verb_stem) {
                                        | Some(verb) => Some(verb.meaning)
                                        | None => None
                                    }
                                }
                            />
                        | (None, None) => <span>{"No Selected Verb" |> React.string}</span>
                        }
                    }
                </div>
                <div className=css##buttons>
                    <Button 
                        variant=`contained 
                        onClick={_ => reset()}
                        disabled={verb_form |> Option.is_none}
                    >
                        {"Reset" |> React.string}
                    </Button>
                    <Button 
                        variant=`contained
                        onClick={_ => copy_result_cuneiforms()}
                        disabled={verb_form |> Option.is_none}
                    >
                        {"Copy" |> React.string}
                    </Button>
                    <Button
                        variant=`contained
                        onClick={_ => set_is_modal_open(_ => true)}
                        disabled={
                            is_transitive |> Option.is_none 
                            || is_perfective |> Option.is_none
                            || Option.is_none(verb_stem)    
                        }
                        >
                        {"Report an error" |> React.string}
                    </Button>
                </div>
            </Container>
        </Grid>
        <Modal is_open={is_modal_open} close={() => set_is_modal_open(_ => false)} >
            <Verb_error_form verb={verb_form} />
        </Modal>
    </Container>
};
