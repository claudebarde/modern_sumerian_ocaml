[@mel.module "../styles/Keyboard.module.scss"] external css: Js.t({..}) = "default"; 
external dom_element_from_event_target: Js.t({..}) => Dom.element = "%identity";

module ScrollableElement = {
    type scroll_options = {
        behavior: string,
        block: string,
        inline: string,
    };

    [@mel.obj]
    external make_scroll_options:
        (~behavior: string, ~block: string, ~inline: string, unit)
        => scroll_options = "";

    [@mel.send] [@mel.return nullable]
    external query_selector:
        (~selector: string, [@mel.this] Dom.element)
        => option(Dom.element) = "querySelector";

    [@mel.send]
    external scroll_into_view:
        (~options: scroll_options, [@mel.this] Dom.element)
        => unit = "scrollIntoView";
};

module Determinatives = {
    type determinative = Digir | Ki | Gesh | Iri | Kush | Uruda | Mush | Mul | Id | Na | Lu | Iti | Sar | Ku | Mushen;

    type select_option = {
        label: string,
        value: determinative,
        symbol: string,
        phonetic: string,
    };

    type determinatives_group = {
        label: string,
        options: array(select_option),
    };

    // [@mel.module "react-select"] [@react.component]
    // external make: (
    //     ~options: array(determinatives_group),
    //     ~value: Js.Nullable.t(select_option),
    //     ~placeholder: string,
    //     ~onChange: select_option => unit,
    //     ~isDisabled: bool,
    //     ~isSearchable: bool,
    // ) => React.element = "default";
};

type cuneiform_selection = {
    id: string,
    cuneiforms: array(string),
    word: string,
    icount: int,
    part_of_speech: string,
    translation: string
};

type composed_sign = {
    cuneiform: string,
    phonetic: string,
    part_of_speech: string,
};

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let determinative_groups: array(Determinatives.determinatives_group) = [|
        {
            label: "Front",
            options: [|
                {label: {js|𒀭 (diĝir)|js}, value: Digir, symbol: {js|𒀭|js}, phonetic: {js|diĝir|js}},
                {label: {js|𒄑 (ĝesh)|js}, value: Gesh, symbol: {js|𒄑|js}, phonetic: {js|ĝesh|js}},
                {label: {js|𒇽 (lu)|js}, value: Lu, symbol: {js|𒇽|js}, phonetic: {js|lu|js}},
                {label: {js|𒌷 (iri)|js}, value: Iri, symbol: {js|𒌷|js}, phonetic: {js|iri|js}},
                {label: {js|𒍏 (uruda)|js}, value: Uruda, symbol: {js|𒍏|js}, phonetic: {js|uruda|js}},
                {label: {js|𒉌𒌓 (na)|js}, value: Na, symbol: {js|𒉌𒌓|js}, phonetic: {js|na|js}},
                {label: {js|𒋢 (kush)|js}, value: Kush, symbol: {js|𒋢|js}, phonetic: {js|kush|js}},
                {label: {js|𒈲 (mush)|js}, value: Mush, symbol: {js|𒈲|js}, phonetic: {js|mush|js}},
                {label: {js|𒀯 (mul)|js}, value: Mul, symbol: {js|𒀯|js}, phonetic: {js|mul|js}},
                {label: {js|𒀀 (id)|js}, value: Id, symbol: {js|𒀀|js}, phonetic: {js|id|js}},
                {label: {js|𒌗 (iti)|js}, value: Iti, symbol: {js|𒌗|js}, phonetic: {js|iti|js}},
            |],
        },
        {
            label: "End",
            options: [|
                {label: {js|𒆠 (ki)|js}, value: Ki, symbol: {js|𒆠|js}, phonetic: {js|ki|js}},
                {label: {js|𒊬 (sar)|js}, value: Sar, symbol: {js|𒊬|js}, phonetic: {js|sar|js}},
                {label: {js|𒄩 (ku)|js}, value: Ku, symbol: {js|𒄩|js}, phonetic: {js|ku|js}},
                {label: {js|𒄷 (mushen)|js}, value: Mushen, symbol: {js|𒄷|js}, phonetic: {js|mushen|js}},
            |],
        },
    |];
    let determinative_options =
        determinative_groups
        |> Array.map((group: Determinatives.determinatives_group) =>
            group.options
        )
        |> Array.to_list
        |> Array.concat;

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
    let (cuneiform_display, set_cuneiform_display) =
        React.useState(_ => (None: option(array(composed_sign))));
    let (cursor_pos, set_cursor_pos) = React.useState(_ => 0);
    let (cuneiform_selection, set_cuneiform_selection) = React.useState(_ => (None: option(array(cuneiform_selection))));
    let (active_cuneiform_selection, set_active_cuneiform_selection) =
        React.useState(_ => (None: option(cuneiform_selection)));
    let (has_word_divider, set_has_word_divider) = React.useState(_ => true);
    let (cuneiform_copy_tooltip_open, set_cuneiform_copy_tooltip_open) =
        React.useState(_ => false);
    let (transliteration_copy_tooltip_open, set_transliteration_copy_tooltip_open) =
        React.useState(_ => false);
    let (visual_aid, set_visual_aid) = React.useState(_ => false);
    let (dictionary_search, set_dictionary_search) = React.useState(_ => false);
    let (keyboard_dictionary, set_keyboard_dictionary) = React.useState(_ => (None: option(LocalStorage.keyboard)));
    let (selected_determinative, set_selected_determinative) =
        React.useState(_ => "");
    // let (how_to_drawer, set_how_to_drawer) = React.useState(_ => false);
    let (determinatives_menu_anchor, set_determinatives_menu_anchor) =
        React.useState(() =>
            (Js.Nullable.null: Js.Nullable.t(Dom.element))
        );
    let determinatives_menu_open =
        !Js.Nullable.isNullable(determinatives_menu_anchor);

    let latest_search_id = React.useRef(0);
    let cuneiform_selection_ref:
        React.ref(Js.nullable(Dom.element)) =
        React.useRef(Js.Nullable.null);
    let cuneiform_copy_tooltip_timeout =
        React.useRef((None: option(Js.Global.timeoutId)));
    let transliteration_copy_tooltip_timeout =
        React.useRef((None: option(Js.Global.timeoutId)));

    let is_mobile = UseMediaQuery.use("(max-width:599px)");

    React.useEffect0(() =>
        Some(() => {
            switch cuneiform_copy_tooltip_timeout.current {
            | Some(timeout_id) => Js.Global.clearTimeout(timeout_id)
            | None => ()
            };
            switch transliteration_copy_tooltip_timeout.current {
            | Some(timeout_id) => Js.Global.clearTimeout(timeout_id)
            | None => ()
            };
        })
    );

    // let toggleHowToDrawer = () => set_how_to_drawer(prev => !prev);

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
                    part_of_speech: selection.part_of_speech,
                    translation: selection.translation,
                }, selection.cuneiforms)
            })
            |> Array.to_list
            |> Array.concat
            |> Array.to_list
            |> Stdlib.List.sort((a, b) => {
                let word_order =
                    a.word |> Js.String.localeCompare(~other=b.word);
                if (word_order < 0.0) {
                    -1
                } else if (word_order > 0.0) {
                    1
                } else {
                    0
                };
            })
            |> Stdlib.List.fold_left((acc, selection) => {
                if (Stdlib.List.exists(sel => sel.cuneiforms[0] === selection.cuneiforms[0], acc)) {
                    acc
                } else {
                    [selection, ...acc]
                }
            }, [])
            |> Stdlib.List.rev
            |> Array.of_list;

        unique_selections
    };

    let search_word = (~request_id: int, user_input: string) => {
        set_cuneiform_selection(_ => None);

        let vowels = [|"a", "e", "i", "u"|];
        let formatted_input =
            user_input
            |> Js.String.replaceByRe(
                ~regexp=Js.Re.fromStringWithFlags("-", ~flags="g"),
                ~replacement=" ",
            )
            |> Js.String.trim 
            |> Js.String.toLowerCase 
            |> Web_utils.Format.from_standard_to_phonetic;
        // First, it looks into the localStorage dictionary to see if the word exists there. 
        let active_selection: option(cuneiform_selection) = 
            switch keyboard_dictionary {
            | Some(dictionary) => {
                switch (Js.Dict.get(
                    dictionary,
                    formatted_input,
                )) {
                | Some(entries) => {
                    // If the word exists in the localStorage dictionary, it is added to cuneiform_selection
                    let decodedCuneiforms: array(cuneiform_selection) = 
                        entries
                        |> Array.mapi((index, entry: LocalStorage.keyboard_entry) => ({
                            id: "local-" ++ Js.Int.toString(index),
                            cuneiforms: [|entry.cuneiform|],
                            word: formatted_input,
                            icount: entry.icount,
                            part_of_speech: entry.part_of_speech,
                            translation: entry.translation,
                        }: cuneiform_selection));
                    let curatedCuneiforms = curate_cuneiforms(decodedCuneiforms);
                    if (Array.length(curatedCuneiforms) > 0) {
                        set_cuneiform_selection(_ => Some(curatedCuneiforms));
                        set_active_cuneiform_selection(_ => Some(curatedCuneiforms[0]));
                        Some(curatedCuneiforms[0])
                    } else {
                        set_cuneiform_selection(_ => Some([||]));
                        set_active_cuneiform_selection(_ => None);
                        None
                    };
                }
                | None => None
                }
            }
            | None => None
            };
        // Then a request is made to the Supabase database to fetch the cuneiforms for the word.
        let word_to_search = formatted_input;
        if ((word_to_search |> Js.String.length === 1 && Array.mem(word_to_search, vowels)) 
            || (word_to_search |> Js.String.length > 1)) {
            let _ = 
                Supabase.client 
                |> Supabase.Query.from("dictionary")
                |> Supabase.Query.select("*")
                |> Supabase.Filter.starts_with_any(
                    ~column="word",
                    ~values=Web_utils.Format.with_g_variants(word_to_search),
                )
                |> Supabase.Modifier.limit(~count=Config.max_keyboard_search_results)
                |> Supabase.Modifier.order(~column="icount", ~options=Some({ascending: false}))
                |> Js.Promise.then_(res => {
                    if (request_id === latest_search_id.current) {
                        let decoded = Supabase.Response.decode(res);
                        // Js.log(decoded.data)
                        let decodedCuneiforms: array(cuneiform_selection) = 
                            decoded.data 
                            |> Array.map((row: Supabase.dictionary_row) => ({
                                id: row.id, 
                                cuneiforms: row.cuneiforms, 
                                word: row.word,  
                                icount: row.icount,
                                part_of_speech: row.part_of_speech,
                                translation: row.translation
                            }: cuneiform_selection));
                        let curatedCuneiforms = curate_cuneiforms(decodedCuneiforms);
                        if (Array.length(curatedCuneiforms) > 0) {
                            // the cuneiform selection from Supabase is added to the cuneiform selection from localStorage, and duplicates are removed
                            let combined = switch active_selection {
                                | Some(prev_selection) => {
                                    // filters the Supabase results to exclude cuneiforms that are already present in the localStorage results
                                    Array.concat([[|prev_selection|], curatedCuneiforms])
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
                            set_cuneiform_selection(_ => Some(combined));
                            switch active_cuneiform_selection {
                                | Some(active) => {
                                    if ((active.word |> Web_utils.Format.from_phonetic_to_standard) !== formatted_input) {
                                        set_active_cuneiform_selection(_ => Some(combined[0]));
                                    } else {
                                        ()
                                    }
                                }
                                | None => {
                                    set_active_cuneiform_selection(prev => {
                                        switch prev {
                                        | Some(_) => prev
                                        | None => Some(combined[0])
                                        }
                                    });
                                }
                            }
                        } else {
                            set_cuneiform_selection(_ => Some([||]));
                            set_active_cuneiform_selection(_ => None);
                        };
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

    React.useEffect1(() => {
        switch (
            Js.Nullable.toOption(cuneiform_selection_ref.current),
            active_cuneiform_selection,
            cuneiform_selection,
        ) {
        | (Some(container), Some(active), Some(selections)) =>
            switch (Array.find_index(selection => selection.id === active.id, selections)) {
            | Some(index) when index > 1 => {
                let previous_item_selector =
                    "#cuneiform-selection-" ++ Js.Int.toString(index - 1);
                switch (
                    container
                    |> ScrollableElement.query_selector(
                        ~selector=previous_item_selector,
                    )
                ) {
                | Some(previous_item) =>
                    previous_item
                    |> ScrollableElement.scroll_into_view(
                        ~options=ScrollableElement.make_scroll_options(
                            ~behavior="smooth",
                            ~block="nearest",
                            ~inline="start",
                            (),
                        ),
                    )
                | None => ()
                };
            }
            | _ => ()
            }
        | _ => ()
        };

        None;
    }, [|active_cuneiform_selection|]);

    let show_cuneiform_copy_tooltip = () => {
        switch cuneiform_copy_tooltip_timeout.current {
        | Some(timeout_id) => Js.Global.clearTimeout(timeout_id)
        | None => ()
        };
        set_cuneiform_copy_tooltip_open(_ => true);
        let timeout_id = Js.Global.setTimeout(
            ~f=() => {
                set_cuneiform_copy_tooltip_open(_ => false);
                cuneiform_copy_tooltip_timeout.current = None;
            },
            1500,
        );
        cuneiform_copy_tooltip_timeout.current = Some(timeout_id);
    };

    let copy_cuneiform_display = () => {
        switch cuneiform_display {
        | Some(display) when Array.length(display) > 0 => {
            let text =
                display
                |> Array.map(value =>
                    if (value.cuneiform === "wd") {
                        has_word_divider ? Js.String.fromCodePoint(0x00B7) : "";
                    } else {
                        value.cuneiform;
                    }
                )
                |> Js.Array.join(~sep="");
            let _ =
                text
                |> Browser.Clipboard.write_text
                |> Js.Promise.then_(_ => {
                    show_cuneiform_copy_tooltip();
                    Js.Promise.resolve();
                })
                |> Js.Promise.catch(error => {
                    Js.log2("Could not copy the cuneiform text:", error);
                    Js.Promise.resolve();
                });
        }
        | _ => ()
        };
    };

    let show_transliteration_copy_tooltip = () => {
        switch transliteration_copy_tooltip_timeout.current {
        | Some(timeout_id) => Js.Global.clearTimeout(timeout_id)
        | None => ()
        };
        set_transliteration_copy_tooltip_open(_ => true);
        let timeout_id = Js.Global.setTimeout(
            ~f=() => {
                set_transliteration_copy_tooltip_open(_ => false);
                transliteration_copy_tooltip_timeout.current = None;
            },
            1500,
        );
        transliteration_copy_tooltip_timeout.current = Some(timeout_id);
    };

    let copy_transliteration_display = () => {
        switch cuneiform_display {
        | Some(display) when Array.length(display) > 0 => {
            let text =
                display
                |> Array.map(value =>
                    if (value.cuneiform === "wd") {
                        " ";
                    } else {
                        value.phonetic
                        |> Js.String.replace(
                            ~search="D=",
                            ~replacement="",
                        )
                        |> Web_utils.Format.from_phonetic_to_standard;
                    }
                )
                |> Js.Array.join(~sep="")
                |> Js.String.trim;
            if (text |> Js.String.length > 0) {
                let _ =
                    text
                    |> Browser.Clipboard.write_text
                    |> Js.Promise.then_(_ => {
                        show_transliteration_copy_tooltip();
                        Js.Promise.resolve();
                    })
                    |> Js.Promise.catch(error => {
                        Js.log2("Could not copy the transliteration:", error);
                        Js.Promise.resolve();
                    });
                ();
            } else {
                ()
            };
        }
        | _ => ()
        };
    };

    let reset = () => {
        set_cuneiform_display(_ => None);
        set_cursor_pos(_ => 0);
        set_input(_ => None);
        set_cuneiform_selection(_ => None);
        set_active_cuneiform_selection(_ => None);
        set_selected_determinative(_ => "");
    };

    let select_determinative = (option: Determinatives.select_option) => {
        let next_cursor_pos = switch cuneiform_display {
        | Some(display) => Array.length(display) + 1
        | None => 1
        };
        set_cuneiform_display(prev => {
            let composed_sign: composed_sign = {
                cuneiform: option.symbol,
                phonetic: "D=" ++ option.phonetic,
                part_of_speech: "",
            };
            let new_display = switch prev {
            | Some(display) =>
                Array.concat([display, [|composed_sign|]])
            | None => [|composed_sign|]
            };
            Some(new_display);
        });
        set_cursor_pos(_ => next_cursor_pos);
        set_selected_determinative(_ => "");
        set_determinatives_menu_anchor(_ => Js.Nullable.null);
    };

    let rememberCuneiformSelection = (selection: cuneiform_selection) => {
        switch input {
        | Some(value) => {
            let word =
                value
                |> Js.String.trim
                |> Js.String.toLowerCase
                |> Web_utils.Format.from_standard_to_phonetic;
            if (word |> Js.String.length > 0) {
                let entry: LocalStorage.keyboard_entry = {
                    cuneiform: selection.cuneiforms[0],
                    icount: selection.icount,
                    part_of_speech: selection.part_of_speech,
                    translation: selection.translation,
                };
                let dictionary: LocalStorage.keyboard = switch keyboard_dictionary {
                | Some(dictionary) => dictionary
                | None => Js.Dict.empty()
                };
                let entries = switch (Js.Dict.get(dictionary, word)) {
                | Some(entries) when Array.exists(
                    (existing: LocalStorage.keyboard_entry) =>
                        existing.cuneiform === entry.cuneiform,
                    entries,
                ) =>
                    entries
                    |> Array.map((existing: LocalStorage.keyboard_entry) =>
                        existing.cuneiform === entry.cuneiform
                            ? entry
                            : existing
                    )
                | Some(entries) => Array.concat([entries, [|entry|]])
                | None => [|entry|]
                };
                Js.Dict.set(dictionary, word, entries);
                let _ = LocalStorage.set_item(
                    "keyboard",
                    LocalStorage.encode_keyboard(dictionary),
                );
                set_keyboard_dictionary(_ => Some(dictionary));
            } else {
                ()
            };
        }
        | None => ()
        }
    };

    let validateCuneiformSelection = (selection: cuneiform_selection) => {
        rememberCuneiformSelection(selection);
        let composed_sign: composed_sign = {
            cuneiform: selection.cuneiforms[0],
            phonetic: selection.word,
            part_of_speech: selection.part_of_speech,
        };
        let insertion_index = switch cuneiform_display {
        | Some(display) =>
            if (cursor_pos < 0) {
                0
            } else if (cursor_pos > Array.length(display)) {
                Array.length(display)
            } else {
                cursor_pos
            }
        | None => 0
        };
        set_cuneiform_display((prev: option(array(composed_sign))) => {
            let new_display = switch prev {
                | Some(display) => {
                    let before_cursor =
                        display
                        |> Js.Array.slice(~start=0, ~end_=insertion_index);
                    let after_cursor =
                        display
                        |> Js.Array.slice(
                            ~start=insertion_index,
                            ~end_=Array.length(display),
                        );

                    Array.concat([
                        before_cursor,
                        [|composed_sign|],
                        after_cursor,
                    ]);
                }
                | None => [|composed_sign|]
            };
            Some(new_display);
        });
        set_cursor_pos(_ => insertion_index + 1);
        set_input(_ => None);
        set_cuneiform_selection(_ => None);
        set_active_cuneiform_selection(_ => None);
    };

    let confirmCuneiformSelection = () => {
        switch active_cuneiform_selection {
        | Some(selection) => validateCuneiformSelection(selection)
        | None => ()
        }
    };

    // let composedSignColor = (part_of_speech: string) => {
    //     let color = switch part_of_speech {
    //     | "N" => Config.colors##moroccanBlue
    //     | "V/t" => Config.colors##protonRed
    //     | "V/i" => Config.colors##pacificTeal
    //     | "AJ" => Config.colors##aspenGold
    //     | _ => Config.colors##darkRift
    //     };
    //     ReactDOM.Style.make(~color=color, ());
    // };

    let remove_composed_sign = index => {
        set_cuneiform_display(previous =>
            switch previous {
            | Some(display) => {
                let updated_display =
                    display
                    |> Js.Array.filteri(
                        ~f=(_, current_index) => current_index !== index,
                    );

                Array.length(updated_display) === 0
                    ? None
                    : Some(updated_display);
            }
            | None => None
            }
        );
        set_cursor_pos(previous_cursor =>
            if (index < previous_cursor) {
                previous_cursor - 1
            } else {
                previous_cursor
            }
        );
    };

    let handleKeyDown = (event: React.Event.Keyboard.t) => {
        if (React.Event.Keyboard.key(event) === "Enter") {
            React.Event.Keyboard.preventDefault(event);
            // when the user presses Enter, the current active selection will be added to the composed display
            // before clearing the input field and resetting the cuneiform selection
            confirmCuneiformSelection();
        } else if (React.Event.Keyboard.key(event) === " ") {
            React.Event.Keyboard.preventDefault(event);
            // returns if there is no composed value and no input value
            switch (cuneiform_display, input) {
            | (None, None) => ()
            | _ => {
                // when the user presses Space, it adds a word divider and clears the input field and cuneiform selection
                // if there is already an input value, it will act as the Enter key and add the current active cuneiform selection before adding the space
                // "wd" is "word delimiter"
                let divider: composed_sign = {
                    cuneiform: "wd",
                    phonetic: "wd",
                    part_of_speech: "",
                };
                let elements_to_insert = switch active_cuneiform_selection {
                | Some(active) => {
                    let active_sign: composed_sign = {
                        cuneiform: active.cuneiforms[0],
                        phonetic: active.word,
                        part_of_speech: active.part_of_speech,
                    };
                    [|active_sign, divider|];
                }
                | None => [|divider|]
                };
                let insertion_index = switch cuneiform_display {
                | Some(display) =>
                    if (cursor_pos < 0) {
                        0
                    } else if (cursor_pos > Array.length(display)) {
                        Array.length(display)
                    } else {
                        cursor_pos
                    }
                | None => 0
                };

                set_cuneiform_display((prev: option(array(composed_sign))) => {
                    let new_display = switch prev {
                    | Some(display) => {
                        let before_cursor =
                            display
                            |> Js.Array.slice(~start=0, ~end_=insertion_index);
                        let after_cursor =
                            display
                            |> Js.Array.slice(
                                ~start=insertion_index,
                                ~end_=Array.length(display),
                            );

                        Array.concat([
                            before_cursor,
                            elements_to_insert,
                            after_cursor,
                        ]);
                    }
                    | None => elements_to_insert
                    };
                    Some(new_display);
                });
                set_cursor_pos(_ => insertion_index + Array.length(elements_to_insert));
                set_input(_ => None);
                set_cuneiform_selection(_ => None);
                set_active_cuneiform_selection(_ => None);
            }
            }
        } else if (React.Event.Keyboard.key(event) === "ArrowLeft") {
            React.Event.Keyboard.preventDefault(event);
            // moves the virtual cursor to the left
            set_cursor_pos(prev => if (prev > 0) { prev - 1 } else { 0 });
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
                                } else {
                                    Some(selections[0])
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
            // moves the virtual cursor to the right
            set_cursor_pos(prev => {
                switch cuneiform_display {
                | Some(display) =>
                    if (prev < Array.length(display)) {
                        prev + 1
                    } else {
                        Array.length(display)
                    }
                | None => 0
                }
            });
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
                                } else {
                                    Some(selections[Array.length(selections) - 1])
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
        } else if (React.Event.Keyboard.key(event) === "Backspace") {
            // input must be empty to trigger cuneiform deletion
            switch (input, cuneiform_display) {
                | (Some(value), _) when String.length(value) > 0 => ()
                | (_, Some(_)) when cursor_pos > 0 => {
                    React.Event.Keyboard.preventDefault(event);
                    // removes the sign immediately to the left of the cursor
                    remove_composed_sign(cursor_pos - 1);
                }
                | _ => ()
            }
        }
    };

    let composed_input = 
        <TextField
            autoFocus=true
            margin=`dense
            variant=`standard
            value={
                switch input {
                | Some(value) => value
                | None => ""
                }
            }
            onChange={event => {
                let value = event -> React.Event.Form.target##value |> String.trim;
                if (String.length(value) == 0) {
                    set_input(_ => None);
                } else {
                    set_input(_ => Some(value));
                }
            }}
            onKeyDown={handleKeyDown}
            sx={{
                "width": "6rem",
                "& input": {
                    "caretColor": "transparent",
                },
            }}
        />;

    let composed_cursor =
        <>
            <span
                className={css##composedSignsCursor}
                ariaHidden=true
            />
            {composed_input}
        </>;

    let cursor_element =
        is_mobile
            ? React.null
            : <React.Fragment key="composed-cursor">
                {composed_cursor}
            </React.Fragment>;

    <div className=css##keyboardContainer>
        <h1>{"Sumerian Keyboard"|>React.string}</h1>
        {
            is_mobile 
            ? <Typography variant=Typography.Variant.subtitle2 align=`center>
                {"Note: switch to desktop to enjoy all the features of the Sumerian keyboard" |> React.string}
            </Typography> 
            : React.null
        }
        <Stack 
            direction=`column 
            spacing={`Object(Stack.ResponsiveSpacing.make(~xs=4, ~sm=2, ()))} 
            sx={{"width": "100%", "alignItems": "center"}} 
            useFlexGap=true
        >
            <div className=css##cuneiformDisplay>
                <div className={css##cuneiformDisplayButtons ++ " " ++ css##onlyDesktop}>
                    // DESKTOP BUTTONS
                    <span>{"Composed text" |> React.string}</span>
                    <ButtonGroup variant=`text>
                        <Tooltip 
                            title={"Copied!" |> React.string}
                            disableFocusListener=true
                            disableHoverListener=true
                            disableTouchListener=true
                            _open=cuneiform_copy_tooltip_open
                            onClose={_ => set_cuneiform_copy_tooltip_open(_ => false)}
                        >
                            <Button
                                ariaLabel="Copy cuneiform text"
                                color=Color.primary
                                size=`small
                                type_=`button
                                startIcon={<TablerReact.IconCopy size=20 stroke=2.0 />}
                                sx={{"padding": "6px 8px", "minWidth": "0"}}
                                onClick={_ => copy_cuneiform_display()}
                            >
                                {"Cuneiform" |> React.string}
                            </Button>
                        </Tooltip>
                        <Tooltip
                            title={"Copied!" |> React.string}
                            disableFocusListener=true
                            disableHoverListener=true
                            disableTouchListener=true
                            _open=transliteration_copy_tooltip_open
                            onClose={_ => set_transliteration_copy_tooltip_open(_ => false)}
                        >
                            <Button
                                ariaLabel="Copy transliteration"
                                color=Color.primary
                                size=`small
                                type_=`button
                                startIcon={<TablerReact.IconCopy size=20 stroke=2.0 />}
                                sx={{"padding": "6px 8px", "minWidth": "0"}}
                                onClick={_ => copy_transliteration_display()}
                            >
                                {"Transliteration" |> React.string}
                            </Button>
                        </Tooltip>
                        <Button
                            ariaLabel="Reset cuneiform text"
                            color=Color.primary
                            size=`small
                            sx={{"padding": "6px 8px", "minWidth": "0"}}
                            onClick={_ => reset()}
                        >
                            <TablerReact.IconTrash size=20 stroke=2.0 />
                        </Button>
                    </ButtonGroup>
                </div>
                <Paper 
                    elevation=0
                    className={css##paper ++ " " ++ css##composedSignsPaper}
                >
                    <div className={css##composedSigns ++ (visual_aid ? " " ++ css##composedSignsVisualAid : "")}>
                    {
                        switch cuneiform_display {
                        | Some(display) => 
                            if (Array.length(display) > 0) {
                                let rendered_elements =
                                    display
                                    |> Array.mapi((index, composed_sign: composed_sign) => {
                                        let cuneiform = composed_sign.cuneiform;
                                        let phonetic = composed_sign.phonetic;
                                        let sign_element =
                                            <React.Fragment
                                                key={Js.Int.toString(index) ++ "-" ++ cuneiform}
                                            >
                                                {
                                                    if (cuneiform === "wd" && has_word_divider) {
                                                        <span className=css##composedWordDivider>
                                                            {React.string(Js.String.fromCodePoint(0x00B7))}
                                                        </span>
                                                    } else if (cuneiform === "wd" && !has_word_divider) {
                                                        <span className=css##hiddenComposedWordDivider />
                                                    } else {
                                                        let phonetic_label =
                                                            phonetic
                                                            |> Js.String.replace(
                                                                ~search="D=",
                                                                ~replacement="",
                                                            )
                                                            |> Web_utils.Format.from_phonetic_to_standard;

                                                        <Paper
                                                            className={css##composedSign ++ (visual_aid ? " " ++ css##composedSignVisualAid : "")}
                                                            elevation={visual_aid ? 1 : 0}
                                                        >
                                                            <IconButton 
                                                                className=css##composedSignDelete
                                                                size=`small
                                                                sx={{
                                                                    "backgroundColor": "primary.main",
                                                                    "color": "primary.contrastText",
                                                                    "&:hover": {
                                                                        "backgroundColor": "primary.dark",
                                                                    },
                                                                }}
                                                                onClick={_ => remove_composed_sign(index)}
                                                            >
                                                                <TablerReact.IconTrash />
                                                            </IconButton>
                                                            <Stack
                                                                sx={{"flexDirection": "column", "justifyContent": "center", "alignItems": "center"}}
                                                                spacing=`Number(0)
                                                                useFlexGap=true
                                                            >
                                                                <strong 
                                                                    className={"cuneiforms " ++ css##composedSignCuneiform}
                                                                    // style={visual_aid ? composedSignColor(composed_sign.part_of_speech) : ReactDOM.Style.make(())}
                                                                >
                                                                    {cuneiform |> React.string}
                                                                </strong>
                                                                <span className=css##composedSignPhonetic>
                                                                    {phonetic_label |> React.string}
                                                                </span>
                                                            </Stack>
                                                        </Paper>
                                                    }
                                                }
                                            </React.Fragment>;

                                        index == cursor_pos
                                            ? [|cursor_element, sign_element|]
                                            : [|sign_element|];
                                    })
                                    |> Array.to_list
                                    |> Array.concat;
                                let rendered_elements =
                                    cursor_pos == Array.length(display)
                                        ? Array.concat([rendered_elements, [|cursor_element|]])
                                        : rendered_elements;

                                rendered_elements |> React.array;
                            } else {
                                cursor_element
                            }
                            | None => cursor_element
                        }
                    }   
                    </div>
                </Paper>
                <Box className={css##cuneiformSelectionBar ++ " " ++ css##onlyDesktop}>
                    <List
                        className={css##cuneiformSelectionList}
                    >
                    {
                        switch (cuneiform_selection, active_cuneiform_selection) {
                            | (Some(selection), Some(active_selection)) => {
                                selection
                                |> Array.mapi((index, item) => 
                                    <Badge
                                        key={item.word ++ "-" ++ (index |> Int.to_string)}
                                        badgeContent={(item.icount |> Int.to_string) ++ {js|×|js} |> React.string}
                                        color=Color.primary
                                        invisible=true
                                    >
                                        <ListItem 
                                            className={css##cuneiformSelectionItem}
                                            sx={{"padding": "0px"}}
                                        >
                                            <Tooltip title={item.translation |> React.string}>
                                                <Button
                                                    variant={active_selection.id == item.id ? `contained : `outlined}
                                                    sx={{
                                                        "display": "flex",
                                                        "flexDirection": "column",
                                                        "alignItems": "flex-start",
                                                        "justifyContent": "flex-start",
                                                        "minWidth": "100px"
                                                    }}
                                                    size=`small
                                                >
                                                    <span>
                                                        <span className="cuneiforms x-small">
                                                            {item.cuneiforms[0] |> React.string}
                                                        </span>
                                                        <span>
                                                            {item.word |> React.string}
                                                        </span>
                                                        <span>
                                                            {" (" ++ (item.icount |> Int.to_string) ++ {js|×|js} ++ ")" |> React.string}
                                                        </span>
                                                    </span>
                                                </Button>
                                            </Tooltip>
                                        </ListItem>
                                    </Badge>
                                )
                                |> React.array
                            }
                            | _ => <ListItem>{"No selection" |> React.string}</ListItem>
                        }
                    }
                    </List>
                    // DETERMINATIVES DESKTOP VIEW
                    <Box 
                        className={css##paper ++ " " ++ css##searchFieldContainer}
                        sx={{
                            "display": {"xs": "none", "sm": "flex"},
                            "flexDirection": "row",
                            "justifyContent": "stretch",
                            "alignItems": "flex-start",
                            "backgroundColor": "transparent",
                            "gap": "10px"
                        }}
                    >
                        <Select
                            autoWidth=true
                            displayEmpty=true
                            renderValue={_ => "Determinatives" |> React.string}
                            value={Select.Value.fromString(selected_determinative)}
                            onChange={(event, _) => {
                                let selected_value = event##target##value;
                                switch (
                                    determinative_options
                                    |> Array.find_opt((option: Determinatives.select_option) =>
                                        option.phonetic === selected_value
                                    )
                                ) {
                                | Some(option) => select_determinative(option)
                                | None => ()
                                };
                            }}
                            sx={{"backgroundColor": "white"}}
                        >
                            <ListSubheader>{"Front" |> React.string}</ListSubheader>
                            {
                                determinative_groups[0].options
                                |> Array.map((option: Determinatives.select_option) =>
                                    <MenuItem
                                        key={option.symbol}
                                        value={option.phonetic}
                                    >
                                        <span className="cuneiforms x-small">
                                            {option.label |> React.string}
                                        </span>
                                    </MenuItem>
                                )
                                |> React.array
                            }
                            <ListSubheader>{"End" |> React.string}</ListSubheader>
                            {
                                determinative_groups[1].options
                                |> Array.map((option: Determinatives.select_option) =>
                                    <MenuItem
                                        key={option.symbol}
                                        value={option.phonetic}
                                    >
                                        <span className="cuneiforms x-small">
                                            {option.label |> React.string}
                                        </span>
                                    </MenuItem>
                                )
                                |> React.array
                            }
                        </Select>
                        {dictionary_search ? 
                            <TablerReact.IconRefresh 
                                className={css##refreshIcon ++ " " ++ css##active}
                                size=20 
                                stroke=3.0 
                                /> : 
                            <TablerReact.IconRefresh className=css##refreshIcon size=20 stroke=3.0 />
                        }
                    </Box>
                </Box>
                <div className={css##cuneiformDisplayButtons ++ " " ++ css##onlyMobile}>
                    // MOBILE BUTTONS
                    <span>
                        <FormControlLabel
                            control={
                                <Switch
                                    checked=has_word_divider
                                    color=Color.primary
                                    onChange={event => set_has_word_divider(_ => event -> React.Event.Form.target##checked)}
                                />
                            }
                            label={"Word delimiter" |> React.string}
                            labelPlacement=`end_
                        />
                    </span>
                    <ButtonGroup>
                        <Button
                            ariaLabel="Copy"
                            variant=`contained
                            color=Color.primary
                            size=`small
                            sx={{"padding": "6px 8px", "minWidth": "0"}}
                            onClick={_ => copy_cuneiform_display()}
                        >
                            <TablerReact.IconCopy size=20 stroke=2.0 />
                            <span className="cuneiform x-small">
                                {{js|𒊬|js} |> React.string}
                            </span>
                        </Button>
                        <Button
                            ariaLabel="Copy"
                            variant=`contained
                            color=Color.primary
                            size=`small
                            sx={{"padding": "6px 8px", "minWidth": "0"}}
                            onClick={_ => copy_transliteration_display()}
                        >
                            <TablerReact.IconCopy size=20 stroke=2.0 />
                            {"ABC" |> React.string}
                        </Button>
                        <Button
                            ariaLabel="Reset cuneiform text"
                            variant=`contained
                            color=Color.primary
                            size=`small
                            sx={{"padding": "6px 8px", "minWidth": "0"}}
                            onClick={_ => reset()}
                        >
                            <TablerReact.IconTrash size=20 stroke=2.0 />
                        </Button>
                    </ButtonGroup>
                </div>
            </div>
            <Box className={css##controls ++ " " ++ css##onlyDesktop}>
                <div>
                    <FormControlLabel
                        control={
                            <Switch
                                checked=has_word_divider
                                color=Color.primary
                                onChange={event => set_has_word_divider(_ => event -> React.Event.Form.target##checked)}
                            />
                        }
                        label={"Word delimiter" |> React.string}
                        labelPlacement=`end_
                    />
                    <FormControlLabel
                        control={
                            <Switch
                                checked=visual_aid
                                color=Color.primary
                                onChange={event => set_visual_aid(_ => event -> React.Event.Form.target##checked)}
                            />
                        }
                        label={"Visual aid" |> React.string}
                        labelPlacement=`end_
                    />
                </div>
                <div>
                    <span>
                        {
                            switch cuneiform_display {
                                | Some(display) => {
                                    if (Array.length(display) === 1) {
                                        (Array.length(display) |> Js.Int.toString) ++ " sign"
                                    } else {
                                        ((Array.length(display) |> Js.Int.toString) ++ " signs")
                                    }
                                }
                                | None => "0 signs"
                            } |> React.string
                        }
                    </span>
                </div>
            </Box> 
            
            // MOBILE VIEW
            <Paper 
                elevation=0
                sx={{
                    "display": {"xs": "flex", "sm": "none"},
                    "flexDirection": "row",
                    "justifyContent": "space-between",
                    "alignItems": "center",
                    "width": "100%",
                    "gap": "10px",
                }}
            >
                <Button
                    variant=`text
                    sx={{"fontFamily": "CuneiformComposite", "fontSize": "1rem", "display": "flex", "alignItems": "center", "justifyContent": "center", "gap": "5px"}}
                    onClick={event =>
                        set_determinatives_menu_anchor(_ =>
                            React.Event.Mouse.currentTarget(event)
                            |> dom_element_from_event_target
                            |> Js.Nullable.return
                        )
                    }
                >
                    <span className="cuneiform x-small">
                        {{js|𒀭|js} |> React.string}
                    </span>
                    <TablerReact.IconChevronDown size=20 stroke=2.0 />
                </Button>
                <Divider orientation=`vertical />
                <Autocomplete
                    autoHighlight=true
                    fullWidth=true
                    size=`small
                    options={switch cuneiform_selection {
                        | Some(selections) => selections
                        | None => [||]
                    }}
                    value={switch active_cuneiform_selection {
                        | Some(selection) => Js.Nullable.return(selection)
                        | None => Js.Nullable.null
                    }}
                    inputValue={switch input {
                        | Some(value) => value
                        | None => ""
                    }}
                    getOptionLabel={(selection: cuneiform_selection) =>
                        selection.word
                        |> Web_utils.Format.from_phonetic_to_standard
                    }
                    getOptionKey={(selection: cuneiform_selection) =>
                        Autocomplete.OptionKey.fromString(selection.id)
                    }
                    isOptionEqualToValue={(option, value) => option.id === value.id}
                    filterOptions={(options, _) => options}
                    loading=dictionary_search
                    loadingText={"Searching the dictionary..." |> React.string}
                    noOptionsText={switch input {
                        | Some(value) when value |> Js.String.trim |> Js.String.length > 0 =>
                            "No cuneiform signs found" |> React.string
                        | _ => "Type a syllable or word to search" |> React.string
                    }}
                    onInputChange={(_event, value, reason) => {
                        switch reason {
                        | `input => set_input(_ => Some(value))
                        | `clear => {
                            set_input(_ => None);
                            set_cuneiform_selection(_ => None);
                            set_active_cuneiform_selection(_ => None);
                        }
                        | `blur
                        | `removeOption
                        | `reset
                        | `selectOption => ()
                        }
                    }}
                    onChange={(_event, selection) =>
                        set_active_cuneiform_selection(_ => selection |> Js.Nullable.toOption)
                    }
                    onHighlightChange={(_event, selection, _reason) =>
                        switch (selection |> Js.Nullable.toOption) {
                        | Some(selection) =>
                            set_active_cuneiform_selection(_ => Some(selection))
                        | None => ()
                        }
                    }
                    onKeyDown={handleKeyDown}
                    renderInput={params =>
                        React.cloneElement(
                            <TextField
                                type_="text"
                                placeholder="Search a word..."
                                variant=`standard
                                size=`small
                            />,
                            params,
                        )
                    }
                    renderOption={(props, selection, _state, _ownerState) => {
                        let option_props = Js.Obj.merge(
                            props,
                            {
                                "onClick": (_event: React.Event.Mouse.t) =>
                                    validateCuneiformSelection(selection),
                            },
                        );
                        React.cloneElement(
                            <li key={selection.id}>
                                <div className=css##searchFieldResult>
                                    <div className=css##searchFieldResultLeft>
                                        <strong className="cuneiforms small">
                                            {selection.cuneiforms[0] |> React.string}
                                        </strong>
                                        <div style={ReactDOM.Style.make(~marginLeft="12px", ~display="flex", ~flexDirection="column", ~justifyContent="center", ~alignItems="flex-start", ())}>
                                            <span>
                                                <span>
                                                {
                                                    (selection.word
                                                    |> Web_utils.Format.from_phonetic_to_standard)
                                                    |> React.string
                                                }
                                                </span>
                                                <span style={ReactDOM.Style.make(~color="grey", ())}>
                                                    {
                                                        " ("
                                                        ++ (Dictionary.display_part_of_speech(selection.part_of_speech) |> Js.String.toLowerCase)
                                                        ++ ")"
                                                        |> React.string
                                                    }
                                                </span>
                                            </span>
                                            <span style={ReactDOM.Style.make(~fontSize="0.8rem", ())}>
                                                {selection.translation |> React.string}
                                            </span>
                                        </div>
                                    </div>
                                    <div>
                                        <span className=css##searchFieldResultOccurences>
                                            {(selection.icount |> Int.to_string) ++ {js|×|js} |> React.string}
                                        </span>
                                    </div>
                                </div>
                            </li>,
                            option_props,
                        )
                    }}
                    sx={{
                        "flex": 1,
                        "minWidth": 0,
                        "backgroundColor": "white",
                    }}
                />
                <Menu
                    anchorEl=determinatives_menu_anchor
                    _open={determinatives_menu_open}
                    onClose={_ =>
                        set_determinatives_menu_anchor(_ => Js.Nullable.null)
                    }
                >
                    <ListSubheader>{"Front" |> React.string}</ListSubheader>
                    {
                        determinative_groups[0].options
                        |> Array.map((option: Determinatives.select_option) =>
                            <MenuItem
                                key={option.symbol}
                                value={option.phonetic}
                                onClick={_ => select_determinative(option)}
                            >
                                {option.label |> React.string}
                            </MenuItem>
                        )
                        |> React.array
                    }
                    <ListSubheader>{"End" |> React.string}</ListSubheader>
                    {
                        determinative_groups[1].options
                        |> Array.map((option: Determinatives.select_option) =>
                            <MenuItem
                                key={option.symbol}
                                value={option.phonetic}
                                onClick={_ => select_determinative(option)}
                            >
                                {option.label |> React.string}
                            </MenuItem>
                        )
                        |> React.array
                    }
                </Menu>
            </Paper>           
            /* <Button 
                onClick={_ => toggleHowToDrawer()}
            >
                <Typography>{"How to use the Sumerian Keyboard" |> React.string}</Typography>
            </Button>
            <Drawer
                anchor=`bottom
                _open={how_to_drawer}
                onClose={_ => set_how_to_drawer(_ => false)}
            >
                <Box className=css##howToUse>
                    <ol>
                        <li>{"Type a word in the input field. Use hyphens instead of spaces for compound words." |> React.string}</li>
                        <li>{"The keyboard will search for cuneiforms that match the word and display them in the selection area." |> React.string}</li>
                        <li>{"Select a cuneiform from the selection area by clicking on it or using the arrow keys." |> React.string}</li>
                        <li>{"Press Enter (or the pencil plus icon on mobile) to add the selected cuneiform to the display area, or press Space to add a space." |> React.string}</li>
                        <li>{"You can also copy the cuneiform text to your clipboard using the copy button." |> React.string}</li>
                    </ol>
                </Box>
            </Drawer> */
        </Stack>
    </div>
}
