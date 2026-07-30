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
    let (_indirect_object, set_indirect_object) = React.useState(_ => None);
    let (is_modal_open, set_is_modal_open) = React.useState(_ => false);

    let marginTop = "20px";

    let available_verbs: array(verb_data) = [|
        {label: "ak (to do)", value: {js|ʔak|js}, imperfective: Other({js|ʔak|js}) },
        {label: {js|ĝen (to go)|js}, value: {js|ĝen|js}, imperfective: Other({js|ĝen|js})},
        {label: "gu (to eat)", value: "gu", imperfective: Other("gu")},
        {label: {js|naĝ (to drink)|js}, value: {js|naĝ|js}, imperfective: Other("na-na")},
        {label: "sar (to write)", value: "sar", imperfective: Other("sar")},
        {label: {js|šum (to give)|js}, value: {js|šum|js}, imperfective: Other({js|šum|js})},
        {label: "tuku (to have)", value: "tuku", imperfective: Other("tuku")},
    |];

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

    let change_pronoun = (value: Utils.select_option, pronoun: string) => {
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
        if (Option.is_none(verb_stem)) {
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
            set_verb_form(_ => Some(Conjugator.create(verb_data.value)));
            set_error(_ => None);
        }
        | None => ()
        };
    };

    <Container className=css##mainContainer>
        <h1>{"Sumerian Verb Conjugator"|>React.string}</h1>
        <Grid container=true spacing=`Number(6) sx={{"marginTop": marginTop}}>
            <Grid size=`Number(6)>
                <FormControl fullWidth=true>
                    <InputLabel id="verb-stem-label">
                        {"Select a verb stem" |> React.string}
                    </InputLabel>
                    <Select
                        label={"Select a verb stem" |> React.string}
                        labelId="verb-stem-label"
                        value={
                            switch verb_stem {
                            | Some(verb) =>
                                Select.Value.fromString(verb.value)
                            | None => Select.Value.fromString("")
                            }
                        }
                        onChange={(event, _) => {
                            let selected_value = event##target##value;
                            let selected_verb =
                                available_verbs
                                |> Array.find_opt((verb: verb_data) =>
                                    verb.value === selected_value
                                );
                            set_new_verb_stem(selected_verb);
                        }}
                        sx={{"backgroundColor": "white"}}
                    >
                        {
                            available_verbs
                            |> Array.map((verb: verb_data) => {
                                <MenuItem value=verb.value key=verb.value>
                                    {verb.label |> React.string}
                                </MenuItem>
                            })
                            |> React.array
                        }
                    </Select>
                </FormControl>
                <Grid container=true spacing=`Number(2) sx={{"marginTop": marginTop}}>
                    <Grid size=`Number(6)>
                        <InputLabel id="transitivity-label">
                            {"Transitivity" |> React.string}
                        </InputLabel>
                        <ToggleButtonGroup
                            value=is_transitive
                            exclusive=true
                            onChange={(_event, checked) => {
                                set_verb_form(prev_verb_form => {
                                    switch prev_verb_form {
                                    | Some(verb) => {
                                        set_error(_ => None)
                                        set_is_transitive(_ => checked)
                                        switch (subject, object_) {
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
                        >
                            <ToggleButton value=Some(true) size=`small>
                                {"Transitive" |> React.string}
                            </ToggleButton>
                            <ToggleButton value=Some(false) size=`small>
                                {"Intransitive" |> React.string}
                            </ToggleButton>
                        </ToggleButtonGroup>
                    </Grid>
                    <Grid size=`Number(6)>
                        <InputLabel id="aspect-label">
                            {"Aspect" |> React.string}
                        </InputLabel>
                        <ToggleButtonGroup
                            value=is_perfective
                            exclusive=true
                            onChange={(_event, value) => {
                                set_verb_form(prev_verb_form => {
                                    switch prev_verb_form {
                                    | Some(verb) => {
                                        set_error(_ => None)
                                        set_is_perfective(_ => value)
                                        // finds the perfective stem
                                        let stem = switch (verb_stem,) {
                                            | Some(verb_data) => verb_data.value
                                            | None => ""
                                        }

                                        switch (subject, object_) {
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
                        >
                            <ToggleButton value=Some(true) size=`small>
                                {"Perfective" |> React.string}
                            </ToggleButton>
                            <ToggleButton value=Some(false) size=`small>
                                {"Imperfective" |> React.string}
                            </ToggleButton>
                        </ToggleButtonGroup>
                    </Grid>
                </Grid>
                <Grid 
                    container=true 
                    spacing=`Number(2) 
                    direction=`row
                    sx={{"width": "100%", "justifyContent": "space-between", "alignItems": "center", "marginTop": marginTop}}
                >
                    <Grid size=`Number(4)>
                        <FormControl fullWidth=true>
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
                                        change_pronoun(option, "subject")
                                    | None => ()
                                    };
                                }}
                                sx={{"backgroundColor": "white"}}
                            >
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
                        <FormControl fullWidth=true>
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
                                        change_pronoun(option, "object")
                                    | None => ()
                                    };
                                }}
                                sx={{"backgroundColor": "white"}}
                            >
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
                        <FormControl fullWidth=true>
                            <InputLabel id="indirect-object-label">
                                {"Indirect Object" |> React.string}
                            </InputLabel>
                            <Select
                                label={"Indirect Object" |> React.string}
                                labelId="indirect-object-label"
                                value={
                                    switch _indirect_object {
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
                                        change_pronoun(option, "indirect-object")
                                    | None => ()
                                    };
                                }}
                                sx={{"backgroundColor": "white"}}
                            >
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
                <Grid container=true spacing=`Number(2)  sx={{"marginTop": marginTop}}>
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
                                onChange={change_preformative}
                                sx={{"padding": "8px"}}
                            >
                                <FormControlLabel
                                    value="preformative-a"
                                    control={<Radio />}
                                    label={"A" |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                                <FormControlLabel
                                    value="preformative-i"
                                    control={<Radio />}
                                    label={"I" |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                                <FormControlLabel
                                    value="preformative-u"
                                    control={<Radio />}
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
                                    let checked: bool = target##checked
                                    change_prefix(Modal, checked)
                                }}
                                sx={{"padding": "8px"}}
                            >
                                <FormControlLabel
                                    value="modal-ha"
                                    control={<Radio />}
                                    label={{js|ḪA|js} |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                                <FormControlLabel
                                    value="modal-nu"
                                    control={<Radio />}
                                    label={"NU" |> React.string}
                                    disabled={
                                        is_transitive |> Option.is_none 
                                        || is_perfective |> Option.is_none
                                        || Option.is_none(verb_stem)
                                    }
                                />
                                <FormControlLabel
                                    value="modal-nan"
                                    control={<Radio />}
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
            </Grid>
            <Grid size=`Number(6)>
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
                        <FormControl fullWidth=true>
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
                                    pronoun_options
                                    |> Array.find_opt(
                                        (option: Utils.select_option) =>
                                            option.value === selected_value
                                    )
                                ) {
                                | Some(option) =>
                                    change_pronoun(
                                        option,
                                        "initial-person-prefix",
                                    )
                                | None => ()
                                };
                            }}
                            disabled={
                                is_transitive |> Option.is_none 
                                || is_perfective |> Option.is_none
                                || Option.is_none(verb_stem)    
                            }
                            sx={{"backgroundColor": "white"}}
                            >
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
                <Box sx={{"marginTop": marginTop}}>
                    {
                        switch (verb_stem) {
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
                </Box>
            </Grid>
            <Grid size=`Number(12)>
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
                                meaning={
                                    switch (verb_stem) {
                                        | Some(m) => Some(m.label)
                                        | None => None
                                    }
                                }
                            />
                        | (None, None) => <span>{"No Selected Verb" |> React.string}</span>
                        }
                    }
                </div>
                <div className=css##buttons>
                    <button className="button" onClick={_ => reset()}>
                        {"Clear" |> React.string}
                    </button>
                    <button className="button">
                        {"Copy" |> React.string}
                    </button>
                    <button 
                        className="button"
                        onClick={_ => 
                        set_is_modal_open(_ => true)}
                        disabled={
                            is_transitive |> Option.is_none 
                            || is_perfective |> Option.is_none
                            || Option.is_none(verb_stem)    
                        }
                        >
                        {"Report an error" |> React.string}
                    </button>
                </div>
            </Grid>
        </Grid>
        // <div className=css##conjugator>
        //     <div className=css##selectors>
        //         <div className=css##firstColumn>
                    
        //         </div>
        //         <div className=css##secondColumn>

        //         </div>
        //     </div>
            
        // </div>
        <Modal is_open={is_modal_open} close={() => set_is_modal_open(_ => false)} >
            <Verb_error_form verb={verb_form} />
        </Modal>
    </Container>
};
