external toNumber: (string) => int = "Number";
[@mel.module "./cuneiform_code_points.json"] external cuneiformCodePoints: Js.Json.t = "default";

type codePointData = {
    codepoint: string,
    name: string,
};
type codePoints = array(codePointData);
type jsonCuneiformData = { codepoints: codePoints};
type cuneiformData = (string, string); // (Unicode code point, sound)

[@mel.scope "JSON"] external parseCuneiformCodePoints: string => jsonCuneiformData = "parse";

let search_cuneiforms = (words: array(string)): array((string, option(list(string)))) => {
    let cuneiformData: jsonCuneiformData = cuneiformCodePoints |> Js.Json.stringify |> parseCuneiformCodePoints;
    words |> Array.map(word => {
        // remove the glottal stop
        let formattedWord = 
            word
            |> Js.String.replaceByRe(~regexp=Js.Re.fromString({js|ʔ|js}), ~replacement="")
            |> Js.String.toUpperCase;
        switch (Array.find_opt(item => item.name == formattedWord, cuneiformData.codepoints)) {
            | Some(codePointData) => (word, Some([codePointData.codepoint]))
            | None => 
                // checks if the word is in the fallback dictionary
                switch (Js.Dict.get(Custom_dict.fallbackDict, word)) {
                | Some(code_list) => (word, Some(code_list))
                | None => (word, None)
                }
        }
    })
};

let display_cuneiforms = (words: array(string)):  array(cuneiformData) => {
    words |> search_cuneiforms |> Array.map(((word, codePoint)) => {
        switch codePoint {
        | Some(code_list) => {
            let code = 
                code_list 
                |> List.map(code => code |> toNumber |> Js.String.fromCodePoint)
                |> Array.of_list
                |> Js.Array.join(~sep="");
            (code, {j|$word|j})
        }
        | None => (word, word)
        }
    })
};

let pronoun_to_person_param = (pronoun: string): option(Conjugator.PersonParam.t) => {
    switch pronoun {
    | "first-sing" => Some(Conjugator.PersonParam.First_sing)
    | "second-sing" => Some(Conjugator.PersonParam.Second_sing)
    | "third-sing-human" => Some(Conjugator.PersonParam.Third_sing_human)
    | "third-sing-nonhuman" => Some(Conjugator.PersonParam.Third_sing_non_human)
    | "first-plur" => Some(Conjugator.PersonParam.First_plur)
    | "second-plur" => Some(Conjugator.PersonParam.Second_plur)
    | "third-plur-human" => Some(Conjugator.PersonParam.Third_plur_human)
    | "third-plur-nonhuman" => Some(Conjugator.PersonParam.Third_plur_non_human)
    | _ => None
    }
};

let parse_verb_syllables = (word: string, stem: string): array(string) => {
    let regex = [%re "/[^aeiu]*[aeiu]+(?:[^aeiu]*$|[^aeiu](?=[^aeiu]))?/gi"];
    let vowels_regex = [%re "/(?<=[aeiu])(?=[aeiu])/gi"];
    let cvc_regex = [%re "/([^aeiu])([aeiu])([^aeiu])/gi"];
    // makes sure that the array will be of length 2
    if (!Js.String.includes(~search=stem, word)) {
        [||]
    } else {
        let syllables = Js.String.split(~sep=stem, word);
        if (Array.length(syllables) != 2) {
            [||]
        } else {
            let (before_stem, after_stem) = (syllables[0], syllables[1]);
            let res_before: array(string) =
                switch (Js.String.match(~regexp=regex, before_stem)) {
                    | Some(matches) => {
                        Array.map(match => {
                            switch match {
                                | Some(m) => m
                                | None => ""
                            }
                        }, matches)
                    }
                    | None => [||]
                };

            let res_after: array(string) =
                switch (Js.String.match(~regexp=regex, after_stem)) {
                    | Some(matches) => {
                        Array.map(match => {
                            switch match {
                                | Some(m) => m
                                | None => ""
                            }
                        }, matches)
                    }
                    | None => [||]
                };

            let formatting = (syllables: array(string)): array(string) => {
                if (Array.length(syllables) == 0) {
                    [||]
                } else {
                    let res: array(string) = syllables
                    // splits clusters of vowels
                    |> Array.map(syll => syll |> Js.String.splitByRe(~regexp=vowels_regex))
                    |> Array.to_list
                    |> Array.concat
                    |> Array.map(syll => switch syll {
                        | Some(syll) => syll
                        | None => ""
                    })
                    // splits CVC clusters
                    |> Array.map(syll => 
                        switch (Js.String.match(~regexp=cvc_regex, syll)) {
                        | Some(matches) => 
                            matches |> Array.map(match => {
                                let match = switch match {
                                    | Some(m) => Js.String.split(~sep="", m)
                                    | None => [||]
                                }
                                if (Array.length(match) != 3) {
                                    [||]
                                } else {
                                    let (first, middle, last) = (match[0], match[1], match[2]);
                                    // returns the two new syllables
                                    [| first ++ middle, middle ++ last |]
                                }
                            })
                            |> Array.to_list
                            |> Array.concat
                        | None => [| syll |]
                    })
                    |> Array.to_list
                    |> Array.concat

                    res
                }
            };

            Array.concat([formatting(res_before), [|stem|], formatting(res_after)])
        }
    }
};

let replace_with_unicode = (word: string): string => {
    let glottal_stop = [%re "/x/gi"];
    let sh_replacement = [%re "/sj/gi"];
    let h_replacement = [%re "/hj/gi"];
    let g_replacement = [%re "/gj/gi"];
    word 
    |> Js.String.replaceByRe(~regexp=glottal_stop, ~replacement={js|ʔ|js})
    |> Js.String.replaceByRe(~regexp=sh_replacement, ~replacement={js|š|js})
    |> Js.String.replaceByRe(~regexp=h_replacement, ~replacement={js|ḫ|js})
    |> Js.String.replaceByRe(~regexp=g_replacement, ~replacement={js|ĝ|js})
}

module BuildResults = {
    [@mel.module "../styles/Conjugator.module.scss"] external css: Js.t({..}) = "default";
    [@react.component]
    let make = (~verb: Conjugator.t, ~meaning: option(string)) => {
        switch (Conjugator.print(verb, meaning)) {
            | Ok({verb: conjugatedVerb, analysis, translation, _}) => {[|
                <div className={css##verbResult} key="verbResults">
                    <span style=(ReactDOM.Style.make(~fontSize="1.2rem", ())) key="verbForm">
                        {conjugatedVerb |> React.string}
                    </span>
                    <span key="cuneiforms">
                        {
                            parse_verb_syllables(conjugatedVerb, verb.stem)
                            |> display_cuneiforms
                            |> Array.mapi((i, (codePoint, word)) => {
                                <Cuneiform_char
                                    key={codePoint ++ word ++ Int.to_string(i)}
                                    codePoint={codePoint}
                                    pronunciation={word}
                                />
                            })
                            |> React.array
                        }
                    </span>
                    <span>
                        {"(" ++ translation ++ ")" |> React.string}
                    </span>
                </div>,
                <table key="verbAnalysis">
                    <tbody>
                        <tr>
                            {analysis |> Conjugator__Verb_analysis.output |> Array.map(
                                ((output_type, _)) => {
                                    <th key={output_type}>
                                        {
                                            switch output_type {
                                                | "middlePrefix" => "Middle Prefix"
                                                | "initialPersonPrefix" => "Initial Person Prefix"
                                                | "finalPersonPrefix" => "Final Person Prefix"
                                                | "edMarker" => "ED Marker"
                                                | "finalPersonSuffix" => "Final Person Suffix"
                                                | _ => {
                                                    let first_char = output_type |> Js.String.charAt(~index=0) |> Js.String.toUpperCase;
                                                    let rest = output_type |> Js.String.slice(~start=1) |> Js.String.toLowerCase;
                                                    first_char ++ rest
                                                }
                                            }|> React.string
                                        }
                                    </th>
                                },
                            )|> React.array}
                        </tr>
                        <tr>
                            {analysis |> Conjugator__Verb_analysis.output |> Array.mapi(
                                (i, (_, value)) => {
                                    <td key={value ++ Int.to_string(i)}>
                                        {value |> React.string}
                                    </td>
                                },
                            )|> React.array}
                        </tr>
                    </tbody>
                </table>,
                <span key="cuneiformWarning" style=(ReactDOM.Style.make(~fontSize="0.6rem", ~fontStyle="italic", ()))>
                    {"The cuneiforms are auto-generated and may not be historically accurate"|> React.string}
                </span>
            |] |> React.array}
            | Error(err) => 
                <span className=css##error>
                    {err |> React.string}
                </span>
        }
    };
};

module EpsdDict = {
    [@mel.module] external epsdDict: Js.Json.t = "./epsd_links.json"

    type epsdData = {
        word: string,
        ref: string,
    };
    type t = array(epsdData);
    type defaultJsonImport = {
        default: t,
    };

    [@mel.scope "JSON"] external parseEpsdDict: string => defaultJsonImport = "parse"

    let get_epsd_link = (word: string): option(string) => {
        let {default: dict} = epsdDict |> Js.Json.stringify |> parseEpsdDict
        let epsdDict = 
            dict
            |> Array.map((item) => (item.word, item.ref))
            |> Js.Dict.fromArray

        switch (Js.Dict.get(epsdDict, word)) {
        | Some(ref) => Some({j|https://oracc.museum.upenn.edu/epsd2/sux/$(ref)|j})
        | None => None
        }
    }
}