[@mel.module "../styles/Dictionary.module.scss"] external css: Js.t({..}) = "default"; 

let rows_for_page = (~page: int, ~rows_per_page: int, rows: array('a)): array('a) => {
    let start = page * rows_per_page;
    rows
    |> Js.Array.slice(
        ~start,
        ~end_=start + rows_per_page,
    );
};

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
    let (rowsPerPage, setRowsPerPage) = React.useState(_ => 5);
    let (page, setPage) = React.useState(_ => 0);
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
    //         icount: 42,
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
    //         icount: 17,
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
    //         icount: 5,
    //     },
    //     {
    //         id: "dummy-4",
    //         marker: Supabase.X,
    //         headword: "gud",
    //         word: "gud",
    //         translation: "ox; bull",
    //         part_of_speech: "noun",
    //         meanings: [|"ox", "bull"|],
    //         forms: [|"gud"|],
    //         cuneiforms: [|"𒄖"|],
    //         dc_title: "gud",
    //         icount: 3,
    //     },
    //     {
    //         id: "dummy-5",
    //         marker: Supabase.L_Akk,
    //         headword: "šarru",
    //         word: "šarru",
    //         translation: "king (Akkadian loanword)",
    //         part_of_speech: "noun",
    //         meanings: [|"king", "ruler"|],
    //         forms: [|"šarru"|],
    //         cuneiforms: [|"𒈗"|],
    //         dc_title: "sarru",
    //         icount: 1,
    //     },
    //     {
    //         id: "dummy-6",
    //         marker: Supabase.C,
    //         headword: "kur",
    //         word: "kur",
    //         translation: "mountain; foreign land (calque)",
    //         part_of_speech: "noun",
    //         meanings: [|"mountain", "foreign land"|],
    //         forms: [|"kur"|],
    //         cuneiforms: [|"𒆳"|],
    //         dc_title: "kur",
    //         icount: 0,
    //     },
    //     {
    //         id: "dummy-7",
    //         marker: Supabase.L_Mod,
    //         headword: "computer",
    //         word: "computer",
    //         translation: "computer (modern loanword)",
    //         part_of_speech: "noun",
    //         meanings: [|"computer"|],
    //         forms: [|"computer"|],
    //         cuneiforms: [||],
    //         dc_title: "computer",
    //         icount: 0,
    //     }
    // |];
    let (search_results, set_search_results) =
        React.useState(_ => (None: option(array(Supabase.dictionary_row))));
        // React.useState(_ => Some(dummy_search_results));

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
                    setPage(_ => 0);
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

    let handleChangePage = (_event, newPage) => {
        setPage(_ => newPage);
    };

    let handleChangeRowsPerPage = event => {
        setRowsPerPage(_ =>
            event
            |> React.Event.Form.target
            |> target => target##value
        );
        setPage(_ => 0);
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
        <Stack 
            spacing=`Object(Stack.ResponsiveSpacing.make(~xs=2, ~md=3, ()))
            direction=`Object(Stack.ResponsiveDirection.make(~xs=`column, ~md=`row, ()))
        >
            <Select
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
            <Button 
                variant=`contained 
                size=`large
                onClick={_ => search_word()}
            >
                {searching ? <TablerReact.IconRefresh className=css##refreshIcon size=20 /> : <TablerReact.IconSearch size=20 />}
            </Button>
        </Stack>
        <div className=css##resultsContainer>
        {
            switch search_results {
            | Some(results) when (word |> String.length > 0) =>
                if (Array.length(results) === 0) {
                    <div>{"No results found." |> React.string}</div>
                } else {
                    <>
                        <TableContainer
                            className=css##tableContainer
                            component=RootComponent.reactComponent(Paper.make)
                        >
                            <div className=css##tableScroll>
                                <Table stickyHeader=true className=css##resultsList>
                                <TableHead>
                                    <TableRow>
                                        <TableCell>{"Cuneiforms" |> React.string}</TableCell>
                                        <TableCell>{"Marker" |> React.string}</TableCell>
                                        <TableCell>{"Word" |> React.string}</TableCell>
                                        <TableCell>{"Translation" |> React.string}</TableCell>
                                        <TableCell>{"Part of Speech" |> React.string}</TableCell>
                                        <TableCell>{"Count" |> React.string}</TableCell>
                                        <TableCell>{"More info" |> React.string}</TableCell>
                                    </TableRow>
                                </TableHead>
                                <TableBody>
                                {
                                    results
                                    |> rows_for_page(
                                        ~page,
                                        ~rows_per_page=rowsPerPage,
                                    )
                                    |> Array.map((result: Supabase.dictionary_row) =>
                                        <TableRow key={result.id}>
                                            <TableCell>
                                                <strong className="cuneiforms small">{
                                                    Array.length(result.cuneiforms) > 0
                                                    ? result.cuneiforms[0] |> React.string
                                                    : "X" |> React.string
                                                }</strong>  
                                            </TableCell>
                                            <TableCell>
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
                                            </TableCell>
                                            <TableCell>
                                                <strong>{result.word |> Web_utils.Format.from_phonetic_to_standard |> React.string}</strong>
                                            </TableCell>
                                            <TableCell>
                                                {result.translation |> React.string}
                                            </TableCell>
                                            <TableCell>
                                                {switch result.part_of_speech {
                                                    | "N" => "Noun" 
                                                    | "V/t" => "Transitive Verb"
                                                    | "V/i" => "Intransitive Verb"
                                                    | "AJ" => "Adjective"   
                                                    | _ => result.part_of_speech
                                                } |> React.string}
                                            </TableCell>
                                            <TableCell>
                                                {result.icount |> Js.Int.toString |> React.string}
                                            </TableCell>
                                            <TableCell>
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
                                            </TableCell>
                                        </TableRow>
                                    )
                                    |> React.array
                                }
                                </TableBody>
                                </Table>
                            </div>
                            <TablePagination
                                className=css##pagination
                                rowsPerPageOptions={[|5, 10, 25|]}
                                component={RootComponent.htmlElement("div")}
                                count={Array.length(results)}
                                rowsPerPage={rowsPerPage}
                                page={page}
                                onPageChange={handleChangePage}
                                onRowsPerPageChange={handleChangeRowsPerPage}
                            />
                        </TableContainer>
                        <div className=css##resultsMobile>
                            <Stack 
                                spacing=`Number(2)
                                direction=`column
                            >
                                <Box>
                                    {
                                        Array.length(results) === 0
                                        ? React.null
                                        : (
                                            <Typography variant=Typography.Variant.h6>
                                                {(
                                                    (Array.length(results) |> Js.Int.toString) 
                                                    ++ " result" 
                                                    ++ (Array.length(results) > 1 ? "s" : "")) |> React.string}
                                            </Typography>
                                        )
                                    }
                                </Box>
                                {
                                    results
                                    |> Array.map((result: Supabase.dictionary_row) => 
                                        <Card>
                                            <CardHeader
                                                avatar={
                                                    <strong className="cuneiforms small">{
                                                        Array.length(result.cuneiforms) > 0
                                                        ? result.cuneiforms[0] |> React.string
                                                        : "X" |> React.string
                                                    }</strong>
                                                }
                                                title={
                                                    <Typography variant=Typography.Variant.h6>
                                                        {result.word |> Web_utils.Format.from_phonetic_to_standard |> React.string}
                                                    </Typography>
                                                }
                                                subheader={(
                                                    result.translation 
                                                    ++ switch result.part_of_speech {
                                                    | "N" => " (Noun)" 
                                                    | "V/t" => " (Transitive Verb)"
                                                    | "V/i" => " (Intransitive Verb)"
                                                    | "AJ" => " (Adjective)"
                                                    | _ => " (" ++ result.part_of_speech ++ ")"
                                                }) |> React.string}
                                            />
                                            <CardContent sx={{"display": "flex", "justifyContent": "space-between"}}>
                                                <div>
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
                                                </div>
                                                <div>
                                                    {(result.icount |> Js.Int.toString) 
                                                    ++ " occurrence" 
                                                    ++ (result.icount > 1 ? "s" : "")
                                                    |> React.string}
                                                </div>
                                            </CardContent>
                                            <CardActions>
                                                {
                                                    switch result.marker {
                                                        | Supabase.A => {
                                                            <Button
                                                                size=`small
                                                                href={"https://oracc.museum.upenn.edu/epsd2/sux/" ++ result.id}
                                                                target="_blank"
                                                                rel="noopener noreferrer"
                                                            >
                                                                {"EPSD2 link" |> React.string}
                                                            </Button>
                                                        }
                                                        | _ => React.null
                                                    }
                                                }
                                            </CardActions>
                                        </Card>
                                    )
                                    |> React.array
                                }
                            </Stack>
                        </div>
                    </>
                }
            | _ => <div>{searching ? "Searching..." |> React.string : "Enter a word to search." |> React.string}</div>
            }
        }
        </div>
    </div>
}
