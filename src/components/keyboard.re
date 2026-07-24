[@mel.module "../styles/Keyboard.module.scss"] external css: Js.t({..}) = "default"; 

module BrowserClipboard = {
    [@mel.scope ("navigator", "clipboard")]
    external write_text: string => Js.Promise.t(unit) = "writeText";
};

type cuneiform_selection = {
    id: string,
    cuneiforms: array(string),
    word: string,
    icount: int,
};

[@react.component]
let make = () => {
    open Bindings;

    // let dummy_cuneiform_selection: array(cuneiform_selection) = [|
    //     {
    //         id: "dummy-1",
    //         cuneiforms: [|"𒆕"|],
    //         word: "du₃",
    //         icount: 42,
    //     },
    //     {
    //         id: "dummy-2",
    //         cuneiforms: [|"𒆕", "𒆖"|],
    //         word: "du₄",
    //         icount: 17,
    //     },
    //     {
    //         id: "dummy-3",
    //         cuneiforms: [|"𒆕", "𒆖"|],
    //         word: "du₅",
    //         icount: 8,
    //     },
    // |];

    let (input, set_input) = React.useState(_ => None);
    let (cuneiform_display, set_cuneiform_display) = React.useState(_ => None);
    let (phonetic_display, set_phonetic_display) = React.useState(_ => None);
    let (cuneiform_selection, set_cuneiform_selection) = React.useState(_ => (None: option(array(cuneiform_selection))));
    let (active_cuneiform_selection, set_active_cuneiform_selection) =
        React.useState(_ => (None: option(cuneiform_selection)));
    let (has_word_delimiter, set_has_word_delimiter) = React.useState(_ => true);
    let (dictionary_search, set_dictionary_search) = React.useState(_ => false);
    let (keyboard_dictionary, set_keyboard_dictionary) = React.useState(_ => (None: option(LocalStorage.keyboard)));

    let latest_search_id = React.useRef(0);

    let curate_cuneiforms = (selections: array(cuneiform_selection)): array(cuneiform_selection) => {
        // Remove duplicates cuneiforms
        let unique_selections = 
            selections
            |> Array.mapi((selection_index, selection) => {
                Array.mapi((cuneiform_index, cuneiform) => {
                    id:
                        selection.id
                        ++ "-"
                        ++ Js.Int.toString(selection_index)
                        ++ "-"
                        ++ Js.Int.toString(cuneiform_index),
                    cuneiforms: [|cuneiform|],
                    word: selection.word |> Js.String.toLowerCase,
                    icount: selection.icount,
                }, selection.cuneiforms)
            })
            |> Array.to_list
            |> Array.concat
            |> Array.to_list
            |> List.sort((a, b) => b.icount - a.icount)
            |> List.fold_left((acc, selection) => {
                if (List.exists(sel => sel.cuneiforms[0] === selection.cuneiforms[0], acc)) {
                    acc
                } else {
                    [selection, ...acc]
                }
            }, [])
            |> List.rev
            |> Array.of_list;

        Js.log(unique_selections);
        unique_selections
    };

    let search_word = (~request_id: int, user_input: string) => {
        let vowels = [|"a", "e", "i", "u"|];
        // First, it looks into the localStorage dictionary to see if the word exists there. 
        let _results = 
            switch keyboard_dictionary {
            | Some(dictionary) => {
                switch (Js.Dict.get(
                    dictionary,
                    user_input 
                    |> Js.String.trim 
                    |> Js.String.toLowerCase 
                    |> Web_utils.Format.from_standard_to_phonetic,
                )) {
                | Some(cuneiforms) => {
                    // If the word exists in the localStorage dictionary, it is added to cuneiform_selection
                    let decodedCuneiforms: array(cuneiform_selection) = 
                        cuneiforms
                        |> Array.mapi((index, cuneiform,) => ({
                            id: "local-" ++ Js.Int.toString(index),
                            cuneiforms: [|cuneiform|],
                            word: user_input |> Js.String.trim |> Js.String.toLowerCase,
                            icount: 0,
                        }: cuneiform_selection));
                    let curatedCuneiforms = curate_cuneiforms(decodedCuneiforms);
                    if (Array.length(curatedCuneiforms) > 0) {
                        set_cuneiform_selection(_ => Some(curatedCuneiforms));
                        set_active_cuneiform_selection(_ => Some(curatedCuneiforms[0]));
                    } else {
                        set_cuneiform_selection(_ => Some([||]));
                        set_active_cuneiform_selection(_ => None);
                    };
                }
                | None => ()
                }
            }
            | None => ()
            };
        // Then a request is made to the Supabase database to fetch the cuneiforms for the word.
        let word_to_search = 
            user_input
            |> Js.String.trim 
            |> Js.String.toLowerCase 
            |> Web_utils.Format.from_standard_to_phonetic;
        if ((word_to_search |> Js.String.length === 1 && Array.mem(word_to_search, vowels)) 
            || (word_to_search |> Js.String.length > 1)) {
            Js.log("Searching for word: " ++ word_to_search);
            let _ = 
                Supabase.client 
                |> Supabase.Query.from("dictionary")
                |> Supabase.Query.select("*")
                |> Supabase.Filter.ilike_any(
                    ~column="word",
                    ~values=Web_utils.Format.with_g_variants(word_to_search),
                    ~contains=false,
                )
                |> Supabase.Modifier.order(~column="icount", ~options=Some({ascending: false}))
                |> Js.Promise.then_(res => {
                    if (request_id === latest_search_id.current) {
                        let decoded = Supabase.Response.decode(res);
                        let decodedCuneiforms: array(cuneiform_selection) = 
                            decoded.data 
                            |> Array.map((row: Supabase.dictionary_row) => ({
                                id: row.id, 
                                cuneiforms: row.cuneiforms, 
                                word: row.word,  
                                icount: row.icount
                            }: cuneiform_selection));
                        let curatedCuneiforms = curate_cuneiforms(decodedCuneiforms);
                        if (Array.length(curatedCuneiforms) > 0) {
                            // the cuneiform selection from Supabase is added to the cuneiform selection from localStorage, and duplicates are removed
                            set_cuneiform_selection(prev => {
                                let combined = switch prev {
                                | Some(prev_selections) => {
                                    // filters the Supabase results to exclude cuneiforms that are already present in the localStorage results
                                    Array.concat([prev_selections, curatedCuneiforms])
                                    |> Array.fold_left((acc, selection) => {
                                        if (Array.exists(sel => sel.cuneiforms[0] === selection.cuneiforms[0], acc)) {
                                            acc
                                        } else {
                                            Array.concat([acc, [|selection|]])
                                        }
                                    }, [||])
                                }
                                | None => curatedCuneiforms
                                };
                                Some(combined);
                            });
                            Js.log(active_cuneiform_selection);
                            switch active_cuneiform_selection {
                                | Some(_) => ()
                                | None => {
                                    set_active_cuneiform_selection(prev => {
                                        switch prev {
                                        | Some(_) => prev
                                        | None => Some(curatedCuneiforms[0])
                                        }
                                    });
                                }
                            }
                        } 
                        // else {
                        //     set_cuneiform_selection(_ => Some([||]));
                        //     set_active_cuneiform_selection(_ => None);
                        // };
                        set_dictionary_search(_ => false);
                    };
                    Js.Promise.resolve();
                })
                |> Js.Promise.catch(err => {
                    if (request_id === latest_search_id.current) {
                        set_cuneiform_selection(_ => None);
                        set_active_cuneiform_selection(_ => None);
                        set_dictionary_search(_ => false);
                        Js.log2("Error during search:", err);
                    };
                    Js.Promise.resolve();
                });
        } else {
            set_cuneiform_selection(_ => None);
            set_active_cuneiform_selection(_ => None);
            set_dictionary_search(_ => false);
        };
    };

    React.useEffect1(() => {
        // on load, the page will fetch the "keyboard" dictionary from the local storage
        // and loads it into the keyboard_dictionary state
        let _ = switch (LocalStorage.get_item("keyboard")) {
        | Some(value) =>
            switch (LocalStorage.decode_keyboard(value)) {
            | Some(keyboard) => set_keyboard_dictionary(_ => Some(keyboard))
            | None => set_keyboard_dictionary(_ => None)
            }
        | None => set_keyboard_dictionary(_ => None)
        };

        None
    }, [||])

    React.useEffect1(() => {
        latest_search_id.current = latest_search_id.current + 1;
        let request_id = latest_search_id.current;
        switch input {
        | Some(value) =>
            set_dictionary_search(_ =>
                value |> Js.String.trim |> Js.String.length > 0
            )
        | None => set_dictionary_search(_ => false)
        };
        let timeout_id = Js.Global.setTimeout(~f=() => {
            switch input {
            | Some(value) => search_word(~request_id, value)
            | None => {
                set_cuneiform_selection(_ => None);
                set_active_cuneiform_selection(_ => None);
                set_dictionary_search(_ => false);
            }
            };
        }, 300);

        Some(() => Js.Global.clearTimeout(timeout_id));
    }, [|input|]);

    let copy_cuneiform_display = () => {
        switch cuneiform_display {
        | Some(display) when Array.length(display) > 0 => {
            let text =
                display
                |> Array.map(value =>
                    if (value === "wd") {
                        has_word_delimiter ? Js.String.fromCodePoint(0x00B7) : "";
                    } else {
                        value;
                    }
                )
                |> Js.Array.join(~sep="");
            let _ =
                text
                |> BrowserClipboard.write_text
                |> Js.Promise.catch(error => {
                    Js.log2("Could not copy the cuneiform text:", error);
                    Js.Promise.resolve();
                });
        }
        | _ => ()
        };
    };

    <div className=css##keyboardContainer>
        <h1>{"Sumerian Keyboard"|>React.string}</h1>
        <div className=css##cuneiformDisplayContainer>
            <div className=css##cuneiformDisplay>
                {
                    switch cuneiform_display {
                    | Some(display) => 
                        if (Array.length(display) > 0) {
                            display
                            |> Array.mapi((index, cuneiform) => {
                                if (cuneiform === "wd" && has_word_delimiter) {
                                    <span 
                                        key={Js.Int.toString(index) ++ "-" ++ cuneiform} 
                                        style=(ReactDOM.Style.make(~fontSize="2.5rem", ()))
                                    >
                                        {React.string(Js.String.fromCodePoint(0x00B7))}
                                    </span>
                                } else if (cuneiform === "wd" && !has_word_delimiter) {
                                    <span key={Js.Int.toString(index) ++ "-" ++ cuneiform}>{React.string("")}</span>
                                } else {
                                    <span key={Js.Int.toString(index) ++ "-" ++ cuneiform} className="cuneiforms">{cuneiform |> React.string}</span>
                                }
                            })
                            |> React.array
                        } else {
                            <div>{"Nothing to show yet." |> React.string}</div>
                        }
                    | None => <div>{"Nothing to show yet." |> React.string}</div>
                    }
                }
            </div>
            <div className=css##cuneiformDisplayButtons>
                <button
                    className="small"
                    ariaLabel="Copy cuneiform text"
                    title="Copy cuneiform text"
                    onClick={_ => copy_cuneiform_display()}
                >
                    <TablerReact.IconCopy size=15 stroke=3.0 />
                </button>
            </div>
        </div>
        <div className=css##phoneticDisplay>
            {switch phonetic_display {
                | Some(value) => 
                    if (Array.length(value) > 0) {
                        value
                        |> Array.mapi((index, phonetic) => {
                            if (phonetic === "wd") {
                                <span key={Js.Int.toString(index) ++ "-" ++ phonetic} className="phonetic">{React.string(" ")}</span>
                            } else {
                                <span 
                                    key={Js.Int.toString(index) ++ "-" ++ phonetic} 
                                    className="phonetic"
                                >{
                                    phonetic 
                                    |> Web_utils.Format.from_phonetic_to_standard 
                                    |> React.string
                                }
                                </span>
                            }
                        })
                        |> React.array
                    } else {
                        React.string("Nothing to show yet.")
                    }
                | None => React.string("Nothing to show yet.")
            }}
        </div>
        <div className=css##cuneiformSelection>
        // This is where the cuneiform selection area will be implemented. 
        // It will allow users to select cuneiform characters to input into the cuneiform display area.
            {
                switch cuneiform_selection {
                | Some(selections) => 
                    if (Array.length(selections) === 0) {
                        <div>{"No cuneiforms found for the input." |> React.string}</div>
                    } else {
                        {
                            selections
                            |> Array.mapi((index, selection: cuneiform_selection) => {
                                <div 
                                    key={selection.id} 
                                    className=css##cuneiformSelectionItem
                                    ariaSelected={switch active_cuneiform_selection {
                                        | Some(active) => active.id === selection.id
                                        | None => index === 0
                                    }}
                                    onClick={_ => {
                                        set_active_cuneiform_selection(_ => Some(selection));
                                    }}
                                >
                                    <strong className="cuneiforms">
                                        {selection.cuneiforms[0] |> React.string}
                                    </strong>
                                </div>
                            }
                            )
                            |> React.array
                        }
                    }
                | None => <div>{"Enter a word to search for its cuneiforms." |> React.string}</div>
                }
            }
        </div>
        <div className=css##controls>
            <div>
                <input 
                    type_="checkbox" 
                    id="wordDelimiter" 
                    name="wordDelimiter" 
                    checked=has_word_delimiter 
                    onChange={event => set_has_word_delimiter(_ => event -> React.Event.Form.target##checked)} 
                />
                <label htmlFor="wordDelimiter">{"Add word delimiter" |> React.string}</label>
            </div>
            <button onClick={_ => {
                set_cuneiform_display(_ => None);
                set_phonetic_display(_ => None);
                set_input(_ => None);
                set_cuneiform_selection(_ => None);
                set_active_cuneiform_selection(_ => None);
            }}>
                {"Clear" |> React.string}
            </button>
        </div>
        <div className=css##typingArea>
            <input 
                type_="text" 
                placeholder="Type here..." 
                autoFocus=true
                value={switch input {
                    | Some(value) => value
                    | None => ""
                }}
                onChange={event => {
                    let value = event -> React.Event.Form.target##value;
                    set_input(_ => Some(value));
                }}
                onKeyDown={event =>
                    if (React.Event.Keyboard.key(event) === "Enter") {
                        React.Event.Keyboard.preventDefault(event);
                        // when the user presses Enter, the current active cuneiform selection will be added to the cuneiform display area
                        // and the word will be added to the phonetic display area
                        // before clearing the input field and resetting the cuneiform selection
                        switch active_cuneiform_selection {
                        | Some(active) => {
                            set_cuneiform_display((prev: option(array(string))) => {
                                let new_display = switch prev {
                                    | Some(display) => Array.concat([display, [|active.cuneiforms[0]|]])
                                    | None => [|active.cuneiforms[0]|]
                                };
                                Some(new_display);
                            });
                            set_phonetic_display((prev: option(array(string))) => {
                                let new_display = switch prev {
                                    | Some(display) => Array.concat([display, [|active.word|]])
                                    | None => [|active.word|]
                                };
                                Some(new_display);
                            });
                            set_input(_ => None);
                            set_cuneiform_selection(_ => None);
                            set_active_cuneiform_selection(_ => None);
                        }
                        | None => ()
                        }
                        // Then, the cuneiform and its phonetic value are added to the localStorage dictionary if they are not already present
                        switch input {
                        | Some(value) => {
                            let trimmed_value = 
                                value 
                                |> Js.String.trim 
                                |> Js.String.toLowerCase 
                                |> Web_utils.Format.from_standard_to_phonetic;
                            switch keyboard_dictionary {
                            | Some(dictionary) => {
                                // the dictionary exists, so we check if the word is already present
                                switch (Js.Dict.get(dictionary, trimmed_value)) {
                                | Some(cuneiforms) => {
                                    switch active_cuneiform_selection {
                                    | Some(active) => {
                                        if (Array.mem(active.cuneiforms[0], cuneiforms)) {
                                            // if the cuneiform doesn't exist in the value array, we add it
                                            if (Array.mem(active.cuneiforms[0], cuneiforms)) {
                                                Js.log("Cuneiform already exists in the dictionary.");
                                                ()
                                            } else {
                                                let new_cuneiforms = Array.concat([cuneiforms, [|active.cuneiforms[0]|]]);
                                                Js.Dict.set(dictionary, trimmed_value, new_cuneiforms);
                                                let _ = LocalStorage.set_item("keyboard", LocalStorage.encode_keyboard(dictionary));
                                                set_keyboard_dictionary(_ => Some(dictionary));
                                                Js.log("Added cuneiform to the dictionary.");
                                            }
                                        } else {
                                            let new_cuneiforms = Array.concat([cuneiforms, [|active.cuneiforms[0]|]]);
                                            Js.Dict.set(dictionary, trimmed_value, new_cuneiforms);
                                            let _ = LocalStorage.set_item("keyboard", LocalStorage.encode_keyboard(dictionary));
                                            set_keyboard_dictionary(_ => Some(dictionary));
                                            Js.log("Added cuneiform to the dictionary.");
                                        }
                                    }
                                    | None => ()
                                    }
                                }
                                | None => {
                                    switch active_cuneiform_selection {
                                    | Some(active) => {
                                        Js.Dict.set(dictionary, trimmed_value, [|active.cuneiforms[0]|]);
                                        let _ = LocalStorage.set_item("keyboard", LocalStorage.encode_keyboard(dictionary));
                                        set_keyboard_dictionary(_ => Some(dictionary));
                                        Js.log("Added new word and cuneiform to the dictionary.");
                                    }
                                    | None => ()
                                    }
                                }
                            }
                            }
                            | None => {
                                // the dictionary doesn't exist, so we create it and add the word and cuneiform
                                let new_dictionary = Js.Dict.empty();
                                switch active_cuneiform_selection {
                                | Some(active) => { 
                                    Js.Dict.set(new_dictionary, trimmed_value, [|active.cuneiforms[0]|]);
                                    let _ = LocalStorage.set_item("keyboard", LocalStorage.encode_keyboard(new_dictionary));
                                    set_keyboard_dictionary(_ => Some(new_dictionary));
                                    Js.log("Created new dictionary and added word and cuneiform.");
                                }
                                | None => ()
                                }
                            }
                        }
                        }
                        | None => ()
                        }
                    } else if (React.Event.Keyboard.key(event) === " ") {
                        React.Event.Keyboard.preventDefault(event);
                        // returns if there is no value in cuneiform display and phonetic display
                        switch (cuneiform_display, phonetic_display, input) {
                        | (None, _, None) => ()
                        | (_, None, None) => ()
                        | _ => {
                            // when the user presses Space, it adds a space in the cuneiform display area and the phonetic display area, and clears the input field and resets the cuneiform selection
                            // if there is already an input value, it will act as the Enter key and add the current active cuneiform selection to the display areas before adding the space
                            // "wd" is "word delimiter"
                            switch active_cuneiform_selection {
                            | Some(active) => {
                                set_cuneiform_display((prev: option(array(string))) => {
                                    let new_display = switch prev {
                                        | Some(display) => Array.concat([display, [|active.cuneiforms[0]|], [|"wd"|]])
                                        | None => [|active.cuneiforms[0], "wd"|]
                                    };
                                    Some(new_display);
                                });
                                set_phonetic_display((prev: option(array(string))) => {
                                    let new_display = switch prev {
                                        | Some(display) => Array.concat([display, [|active.word|], [|"wd"|]])
                                        | None => [|active.word, "wd"|]
                                    };
                                    Some(new_display);
                                });
                                set_input(_ => None);
                                set_cuneiform_selection(_ => None);
                                set_active_cuneiform_selection(_ => None);
                            }
                            | None => {
                                set_cuneiform_display((prev: option(array(string))) => {
                                    let new_display = switch prev {
                                        | Some(display) => Array.concat([display, [|"wd"|]])
                                        | None => [|"wd"|]
                                    };
                                    Some(new_display);
                                });
                                set_phonetic_display((prev: option(array(string))) => {
                                    let new_display = switch prev {
                                        | Some(display) => Array.concat([display, [|"wd"|]])
                                        | None => [|"wd"|]
                                    };
                                    Some(new_display);
                                });
                                set_input(_ => None);
                                set_cuneiform_selection(_ => None);
                                set_active_cuneiform_selection(_ => None);
                            }
                            }
                        }
                        }
                    } else if (React.Event.Keyboard.key(event) === "ArrowLeft") {
                        React.Event.Keyboard.preventDefault(event);
                        // moves the active selection to the left
                        set_active_cuneiform_selection(prev =>  
                            switch prev {
                            | Some(active) =>
                                switch cuneiform_selection {
                                | Some(selections) =>
                                    let current_index = Array.find_index(sel => sel.id === active.id, selections);
                                    switch current_index {
                                        | Some(index) =>
                                            if (index > 0) {
                                                Some(selections[index - 1])
                                            } else if (Array.length(selections) > 0) {
                                                Some(selections[Array.length(selections) - 1])
                                            } else {
                                                None
                                            }
                                        | None => Some(active)
                                    }
                                | None => None
                                }
                            | None => 
                                switch cuneiform_selection {
                                | Some(selections) => 
                                    if (Array.length(selections) > 0) {
                                        Some(selections[0])
                                    } else {
                                        None
                                    }
                                | None => None
                                }
                            }
                        );
                    } else if (React.Event.Keyboard.key(event) === "ArrowRight") {
                        React.Event.Keyboard.preventDefault(event);
                        // moves the active selection to the right
                        set_active_cuneiform_selection(prev =>  
                            switch prev {
                            | Some(active) =>
                                switch cuneiform_selection {
                                | Some(selections) =>
                                    let current_index = Array.find_index(sel => sel.id === active.id, selections);
                                    switch current_index {
                                        | Some(index) =>
                                            if (index < Array.length(selections) - 1) {
                                                Some(selections[index + 1])
                                            } else if (Array.length(selections) > 0) {
                                                Some(selections[0])
                                            } else {
                                                None
                                            }
                                        | None => Some(active)
                                    }
                                | None => None
                                }
                            | None => 
                                switch cuneiform_selection {
                                | Some(selections) => 
                                    if (Array.length(selections) > 0) {
                                        Some(selections[0])
                                    } else {
                                        None
                                    }
                                | None => None
                                }
                            }
                        );
                    }
                }
            />
            {dictionary_search ? 
            <TablerReact.IconRefresh 
                className={css##refreshIcon ++ " " ++ css##active}
                size=20 
                stroke=3.0 
                /> : 
            <TablerReact.IconRefresh className=css##refreshIcon size=20 stroke=3.0 />}
        </div>
    </div>
}
