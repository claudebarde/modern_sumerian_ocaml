[@mel.module "../styles/Dictionary.module.scss"] external css: Js.t({..}) = "default"; 

module SelectLang = {
    type lang_option = EngToSum | SumToEng;

    type select_option = {
        label: string,
        value: lang_option
    };

    [@mel.module "react-select"] [@react.component]
    external make: (
        ~options: array<select_option>,
        ~value: select_option,
        ~onChange: select_option => unit,
        ~isDisabled: bool,
        ~isSearchable: bool,
    ) => React.element = "default";
};

module SelectSearchShape = {
    type search_option = ExactWord | Contains;

    type select_option = {
        label: string,
        value: search_option
    };

    [@mel.module "react-select"] [@react.component]
    external make: (
        ~options: array<select_option>,
        ~value: select_option,
        ~onChange: select_option => unit,
        ~isDisabled: bool,
        ~isSearchable: bool,
    ) => React.element = "default";
};

[@react.component]
let make = () => {
    open Bindings;

    let language_options: array(SelectLang.select_option) = [|
        {label: "English-Sumerian", value: SelectLang.EngToSum},
        {label: "Sumerian-English", value: SelectLang.SumToEng},
    |];

    let search_shape_options: array(SelectSearchShape.select_option) = [|
        {label: "Exact Word", value: SelectSearchShape.ExactWord},
        {label: "Contains", value: SelectSearchShape.Contains},
    |];

    let (selected_lang, set_selected_lang) = React.useState(_ => language_options[0]);
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
    let (selected_search_shape, set_selected_search_shape) = React.useState(_ => search_shape_options[0]);

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
            let column = switch selected_lang.value {
                | SelectLang.EngToSum => "translation"
                | SelectLang.SumToEng => "word"
            };
            let filter = switch (selected_lang.value, selected_search_shape.value) {
                | (SelectLang.SumToEng, SelectSearchShape.ExactWord) =>
                    Supabase.Filter.ilike_any(
                        ~column,
                        ~values=Web_utils.Format.with_g_variants(word_to_search),
                        ~contains=false,
                    )
                | (SelectLang.SumToEng, SelectSearchShape.Contains) =>
                    Supabase.Filter.ilike_any(
                        ~column,
                        ~values=Web_utils.Format.with_g_variants(word_to_search),
                        ~contains=true,
                    )
                | (_, SelectSearchShape.ExactWord) => Supabase.Filter.ilike(~column, ~value=word_to_search)
                | (_, SelectSearchShape.Contains) => Supabase.Filter.ilike(~column, ~value=("%" ++ word_to_search ++ "%"))
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
            {selected_lang.value === SelectLang.EngToSum 
            ? "English > Sumerian Dictionary" |> React.string 
            : "Sumerian > English Dictionary" |> React.string}
        </h1>
        <SelectLang 
            options={language_options}
            value={selected_lang} 
            onChange={option => set_selected_lang(_previous => option)}
            isDisabled={false}
            isSearchable={false}
        />
        <div className=css##searchBar>
            <input 
                type_="text" 
                placeholder="Search a word..." 
                value={word} 
                onChange={event => set_word(_ => event -> React.Event.Form.target##value)}
                onKeyDown={event =>
                    if (React.Event.Keyboard.key(event) === "Enter") {
                        React.Event.Keyboard.preventDefault(event);
                        search_word();
                    }
                }
            />
            <SelectSearchShape
                options={search_shape_options}
                value={selected_search_shape}
                onChange={option => set_selected_search_shape(_previous => option)}
                isDisabled={false}
                isSearchable={false}
            />
            <button onClick={_ => search_word()}>
                {searching ? <TablerReact.IconRefresh className=css##refreshIcon size=20 /> : <TablerReact.IconSearch size=20 />}
            </button>
        </div>
        <div className=css##resultsContainer>
        {
            switch search_results {
            | None => <div>{searching ? "Searching..." |> React.string : "Enter a word to search." |> React.string}</div>
            | Some(results) => 
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
            }
        }
        </div>
    </div>
}
