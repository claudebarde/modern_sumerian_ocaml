[@mel.module "../styles/Dictionary.module.scss"] external css: Js.t({..}) = "default"; 

type select_lang_options = EngToSum | SumToEng;
type selected_search_shape = ExactWord | Contains;

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (selected_lang, set_selected_lang) = React.useState(_ => EngToSum);
    let selected_lang_value =
        switch selected_lang {
        | EngToSum => Select.Value.fromString("eng-to-sum")
        | SumToEng => Select.Value.fromString("sum-to-eng")
        };
    let select_lang_options_to_string = (option: select_lang_options): string =>
        switch option {
        | EngToSum => "eng-to-sum"
        | SumToEng => "sum-to-eng"
        };
    let (selected_search_shape, set_selected_search_shape) = React.useState(_ => ExactWord);
    let selected_search_shape_value =
        switch selected_search_shape {
        | ExactWord => Select.Value.fromString("exact-word")
        | Contains => Select.Value.fromString("contains")
        };
    let selected_search_shape_to_string = (option: selected_search_shape): string =>
        switch option {
        | ExactWord => "exact-word"
        | Contains => "contains"
        };

    let (word, set_word) = React.useState(_ => "");
    let (searching, set_searching) = React.useState(_ => false);
    /* Temporary fixture rows for styling the search-results DOM. */
    // let dummy_search_results: array(Supabase.dictionary_row) = [|
    //     {
    //         id: "dummy-1",
    //         marker: Supabase.A,
    //         headword: "lugal",
    //         word: "lugal",
    //         translation: "king",
    //         part_of_speech: "noun",
    //         meanings: [|"king", "ruler"|],
    //         forms: [|"lugal"|],
    //         cuneiforms: [|"𒈗"|],
    //         dc_title: "lugal",
    //     },
    //     {
    //         id: "dummy-2",
    //         marker: Supabase.E,
    //         headword: "é",
    //         word: "é",
    //         translation: "house; temple",
    //         part_of_speech: "noun",
    //         meanings: [|"house", "temple"|],
    //         forms: [|"é"|],
    //         cuneiforms: [|"𒂍"|],
    //         dc_title: "e",
    //     },
    //     {
    //         id: "dummy-3",
    //         marker: Supabase.N,
    //         headword: "du₃",
    //         word: "du₃",
    //         translation: "to build",
    //         part_of_speech: "verb",
    //         meanings: [|"build", "erect"|],
    //         forms: [|"du₃"|],
    //         cuneiforms: [|"𒆕"|],
    //         dc_title: "du3",
    //     },
    // |];
    let (search_results, set_search_results) =
        React.useState(_ => (None: option(array(Supabase.dictionary_row))));

    let search_word = () => {
        if (word |> Js.String.trim |> Js.String.length === 0) {
            set_search_results(_ => None);
        } else {
            set_searching(_ => true);
            set_search_results(_ => None);
            let word_to_search = 
                word 
                |> Js.String.trim 
                |> Js.String.toLowerCase 
                |> Web_utils.Format.from_standard_to_phonetic;
            Js.log("Searching for word: " ++ word_to_search);
            // Implement the search logic here, possibly using Supabase client
            let column = switch selected_lang {
                | EngToSum => "translation"
                | SumToEng => "word"
            };
            let filter = switch (selected_lang, selected_search_shape) {
                | (SumToEng, ExactWord) =>
                    Supabase.Filter.ilike_any(
                        ~column,
                        ~values=Web_utils.Format.with_g_variants(word_to_search),
                        ~contains=false,
                    )
                | (SumToEng, Contains) =>
                    Supabase.Filter.ilike_any(
                        ~column,
                        ~values=Web_utils.Format.with_g_variants(word_to_search),
                        ~contains=true,
                    )
                | (_, ExactWord) => Supabase.Filter.ilike(~column, ~value=word_to_search)
                | (_, Contains) => Supabase.Filter.ilike(~column, ~value=("%" ++ word_to_search ++ "%"))
            };
            let _ = 
                Supabase.client 
                |> Supabase.Query.from("dictionary")
                |> Supabase.Query.select("*")
                |> filter
                |> Supabase.Modifier.order(~column="icount", ~options=Some({ascending: false}))
                |> Js.Promise.then_(res => {
                    // Js.log("Search result: " ++ Js.Json.stringify(res));
                    let decoded = Supabase.Response.decode(res);
                    set_search_results(_ => Some(decoded.data));
                    set_searching(_ => false);
                    Js.Promise.resolve();
                })
                |> Js.Promise.catch(err => {
                    set_searching(_ => false);
                    Js.log2("Error during search:", err);
                    Js.Promise.resolve();
                });
        }
    };

    <div className=css##dictionary>
        <h1>
            {
                switch selected_lang {
                | EngToSum => "English > Sumerian Dictionary" |> React.string
                | SumToEng => "Sumerian > English Dictionary" |> React.string
                }
            }
        </h1>
        <div className=css##searchBar>
            <Select
                autoWidth=true
                value={selected_lang_value}
                onChange={(event, _) => {
                    let value = event##target##value;
                    let new_lang = switch value {
                        | "eng-to-sum" => EngToSum
                        | "sum-to-eng" => SumToEng
                        | _ => selected_lang
                    };
                    set_selected_lang(_ => new_lang);
                }}
                sx={{"backgroundColor": "white"}}
            >
                <MenuItem value={select_lang_options_to_string(EngToSum)}>
                    {"English to Sumerian" |> React.string}
                </MenuItem>
                <MenuItem value={select_lang_options_to_string(SumToEng)}>
                    {"Sumerian to English" |> React.string}
                </MenuItem>
            </Select>
            <TextField
                type_="text"
                fullWidth=false
                autoFocus=true
                placeholder="Search a word..."
                label={switch selected_lang {
                    | EngToSum => "English Word" |> React.string
                    | SumToEng => "Sumerian Word" |> React.string
                }}
                value={word}
                onChange={event => set_word(_ => event -> React.Event.Form.target##value)}
                onKeyDown={event =>
                    if (React.Event.Keyboard.key(event) === "Enter") {
                        React.Event.Keyboard.preventDefault(event);
                        search_word();
                    }
                }
                sx={{"backgroundColor": "white", "width": "300px"}}
                variant=`outlined
            />
            <Select
                autoWidth=true
                value={selected_search_shape_value}
                onChange={(event, _) => {
                    let value = event##target##value;
                    let new_search_shape = switch value {
                        | "exact-word" => ExactWord
                        | "contains" => Contains
                        | _ => selected_search_shape
                    };
                    set_selected_search_shape(_ => new_search_shape);
                }}
                sx={{"backgroundColor": "white"}}
            >
                <MenuItem value={selected_search_shape_to_string(ExactWord)}>
                    {"Exact Word" |> React.string}
                </MenuItem>
                <MenuItem value={selected_search_shape_to_string(Contains)}>
                    {"Contains" |> React.string}
                </MenuItem>
            </Select>
            <Button className="button" onClick={_ => search_word()}>
                {searching ? <TablerReact.IconRefresh className=css##refreshIcon size=20 /> : <TablerReact.IconSearch size=20 />}
            </Button>
        </div>
        <div className=css##resultsContainer>
        {
            switch search_results {
            | Some(results) when (word |> String.length > 0) =>
                if (Array.length(results) === 0) {
                    <div>{"No results found." |> React.string}</div>
                } else {
                    <table className=css##resultsList>
                        <thead>
                            <tr>
                                <th>{"Cuneiforms" |> React.string}</th>
                                <th>{"Marker" |> React.string}</th>
                                <th>{"Word" |> React.string}</th>
                                <th>{"Translation" |> React.string}</th>
                                <th>{"Part of Speech" |> React.string}</th>
                                <th>{"Count" |> React.string}</th>
                                <th>{"More info" |> React.string}</th>
                            </tr>
                        </thead>
                        <tbody>
                            {results
                            |> Array.map((result: Supabase.dictionary_row) =>
                                <tr key={result.id}>
                                    <td>
                                        <strong className="cuneiforms small">{
                                            Array.length(result.cuneiforms) > 0
                                            ? result.cuneiforms[0] |> React.string
                                            : "X" |> React.string
                                        }</strong>
                                    </td>
                                    <td>
                                        {switch result.marker {
                                        | Supabase.A => "Ancien Sumerian" |> React.string
                                        | Supabase.E => "Modern Extension" |> React.string
                                        | Supabase.N => "Native Neologism" |> React.string
                                        | Supabase.C => "Calque" |> React.string
                                        | Supabase.L_Akk => "Akkadian Loanword" |> React.string
                                        | Supabase.L_Anc => "Ancien Loanword" |> React.string
                                        | Supabase.L_Mod => "Modern Loanword" |> React.string
                                        | Supabase.X => "Uncertain" |> React.string
                                        }}
                                    </td>
                                    <td>
                                        <strong>{result.word |> Web_utils.Format.from_phonetic_to_standard |> React.string}</strong>
                                    </td>
                                    <td>
                                        {result.translation |> React.string}
                                    </td>
                                    <td>
                                        {switch result.part_of_speech {
                                            | "N" => "Noun" 
                                            | "V/t" => "Transitive Verb"
                                            | "V/i" => "Intransitive Verb"
                                            | "AJ" => "Adjective"
                                            | _ => result.part_of_speech
                                        } |> React.string}
                                    </td>
                                    <td>
                                        {result.icount |> Js.Int.toString |> React.string}
                                    </td>
                                    <td>
                                        {
                                            switch result.marker {
                                                | Supabase.A => {
                                                    <a href={"https://oracc.museum.upenn.edu/epsd2/sux/" ++ result.id} target="_blank" rel="noopener noreferrer">
                                                        {"EPSD2 link" |> React.string}
                                                    </a>
                                                }
                                                | _ => React.null
                                            }
                                        }
                                    </td>
                                </tr>
                            )
                            |> React.array}
                        </tbody>
                    </table>
                }
            | _ => <div>{searching ? "Searching..." |> React.string : "Enter a word to search." |> React.string}</div>
            }
        }
        </div>
    </div>
}
