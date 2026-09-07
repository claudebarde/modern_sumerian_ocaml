[@mel.module "./sumerian_verbs.json"] external sumerianVerbsJson: Js.Json.t = "default";

type fixed_element = {
    value: string,
    cuneiforms: array(string),
};

type verb_kind =
    | Simple
    | Compound(fixed_element);

type verb_data = {
    label: string,
    meaning: string,
    english: Conjugator.english_verb,
    stem: string,
    stem_cuneiforms: array(string),
    kind: verb_kind,
    imperfective: Conjugator.ipfv_stem,
    transitive: bool,
    notes: array(string),
    firstLetter: string,
};

module SumerianVerbs = {
    let get_field = (object_, field) => Js.Dict.get(object_, field);

    let get_string = (object_, field) =>
        switch (get_field(object_, field)) {
        | Some(value) => Js.Json.decodeString(value)
        | None => None
        };

    let get_boolean = (object_, field) =>
        switch (get_field(object_, field)) {
        | Some(value) => Js.Json.decodeBoolean(value)
        | None => None
        };

    let get_string_array = (object_, field) =>
        switch (get_field(object_, field)) {
        | Some(value) =>
            switch (Js.Json.decodeArray(value)) {
            | Some(values) =>
                let rec decode = (index, strings) =>
                    if (index >= Array.length(values)) {
                        Some(strings |> Stdlib.List.rev |> Array.of_list);
                    } else {
                        switch (Js.Json.decodeString(values[index])) {
                        | Some(value) => decode(index + 1, [value, ...strings])
                        | None => None
                        };
                    };
                decode(0, []);
            | None => None
            }
        | None => None
        };

    let parse_kind = json =>
        switch (Js.Json.decodeObject(json)) {
        | Some(object_) =>
            switch (get_string(object_, "type")) {
            | Some("simple") => Some(Simple)
            | Some("compound") =>
                switch (
                    get_string(object_, "value"),
                    get_string_array(object_, "cuneiforms"),
                ) {
                | (Some(value), Some(cuneiforms)) =>
                    Some(Compound({value, cuneiforms}))
                | _ => None
                }
            | _ => None
            }
        | None => None
        };

    let parse_imperfective = json =>
        switch (Js.Json.decodeObject(json)) {
        | Some(object_) =>
            switch (get_string(object_, "type")) {
            | Some("ed_marker") => Some(Conjugator.Ed_marker)
            | Some("reduplicate") =>
                Some(
                    Conjugator.Reduplicate(
                        get_string(object_, "value"),
                    ),
                )
            | Some("other") =>
                switch (get_string(object_, "value")) {
                | Some(value) => Some(Conjugator.Other(value))
                | None => None
                }
            | _ => None
            }
        | None => None
        };

    let parse_english_verb = json =>
        switch (Js.Json.decodeObject(json)) {
        | Some(object_) =>
            let complement =
                switch (get_field(object_, "complement")) {
                | Some(value) =>
                    switch (Js.Json.decodeString(value)) {
                    | Some(complement) => Some(Some(complement))
                    | None => None
                    }
                | None => Some(None)
                };
            let complement_placement =
                switch (get_string(object_, "complement_placement")) {
                | Some("after_object") => Some(Conjugator.After_object)
                | Some("after_verb") | None => Some(Conjugator.After_verb)
                | Some(_) => None
                };

            switch (
                get_string(object_, "lemma"),
                complement,
                complement_placement,
            ) {
            | (Some(lemma), Some(complement), Some(complement_placement)) =>
                Some(({
                    lemma,
                    complement,
                    complement_placement,
                }: Conjugator.english_verb))
            | _ => None
            }
        | None => None
        };

    let parse_verb = json =>
        switch (Js.Json.decodeObject(json)) {
        | Some(object_) =>
            let kind =
                switch (get_field(object_, "kind")) {
                | Some(value) => parse_kind(value)
                | None => None
                };
            let imperfective =
                switch (get_field(object_, "imperfective")) {
                | Some(value) => parse_imperfective(value)
                | None => None
                };
            let english =
                switch (get_field(object_, "english")) {
                | Some(value) => parse_english_verb(value)
                | None => None
                };
            switch (
                get_string(object_, "label"),
                get_string(object_, "meaning"),
                english,
                get_string(object_, "stem"),
                get_string_array(object_, "stem_cuneiforms"),
                kind,
                imperfective,
                get_boolean(object_, "transitive"),
                get_string_array(object_, "notes"),
            ) {
            | (
                Some(label),
                Some(meaning),
                Some(english),
                Some(stem),
                Some(stem_cuneiforms),
                Some(kind),
                Some(imperfective),
                Some(transitive),
                Some(notes),
              ) =>
                Some({
                    label,
                    meaning,
                    english,
                    stem,
                    stem_cuneiforms,
                    kind,
                    imperfective,
                    transitive,
                    notes,
                    firstLetter: label |> Js.String.charAt(~index=0),
                })
            | _ => None
            }
        | None => None
        };

    let parse_result = (json: Js.Json.t): result(array(verb_data), string) =>
        switch (Js.Json.decodeArray(json)) {
        | Some(rows) =>
            let rec decode = (index, verbs) =>
                if (index >= Array.length(rows)) {
                    Ok(verbs |> Stdlib.List.rev |> Array.of_list);
                } else {
                    switch (parse_verb(rows[index])) {
                    | Some(verb) => decode(index + 1, [verb, ...verbs])
                    | None =>
                        Error(
                            "Invalid Sumerian verb data at array index "
                            ++ Js.Int.toString(index),
                        )
                    };
                };
            decode(0, []);
        | None => Error("The Sumerian verbs JSON root must be an array")
        };

    let parse = (json: Js.Json.t): array(verb_data) =>
        switch (parse_result(json)) {
        | Ok(verbs) => {
            let _ = verbs |> Array.sort((a, b) => String.compare(a.label, b.label));
            verbs
        }
        | Error(message) => Js.Exn.raiseError(message)
        };

    let verbs: array(verb_data) = parse(sumerianVerbsJson);
};