[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 
external dom_element_from_event_target: Js.t({..}) => Dom.element = "%identity";

module ScrollableElement = {
    type scroll_to_options;

    [@mel.obj]
    external make_scroll_to_options:
        (~top: float, ~behavior: string, unit) => scroll_to_options = "";

    [@mel.get]
    external scroll_top: Dom.element => float = "scrollTop";

    [@mel.get]
    external scroll_height: Dom.element => float = "scrollHeight";

    [@mel.get]
    external client_height: Dom.element => float = "clientHeight";

    [@mel.send]
    external scroll_to:
        (scroll_to_options, [@mel.this] Dom.element) => unit = "scrollTo";
};

type grammar_note = {
    slug: string,
    title: string,
    file: string,
};

let decode_string_field = (object_, field) =>
    switch (Js.Dict.get(object_, field)) {
    | Some(value) => Js.Json.decodeString(value)
    | None => None
    };

let decode_grammar_note = json =>
    switch (Js.Json.decodeObject(json)) {
    | Some(object_) =>
        switch (
            decode_string_field(object_, "slug"),
            decode_string_field(object_, "title"),
            decode_string_field(object_, "file"),
        ) {
        | (Some(slug), Some(title), Some(file)) =>
            Some({slug, title, file})
        | _ => None
        }
    | None => None
    };

let parse_grammar_notes_index = json =>
    switch (Js.Json.decodeArray(json)) {
    | Some(rows) =>
        let rec decode_rows = (index, notes) =>
            if (index >= Array.length(rows)) {
                Ok(notes |> Stdlib.List.rev |> Array.of_list);
            } else {
                switch (decode_grammar_note(rows[index])) {
                | Some(note) => decode_rows(index + 1, [note, ...notes])
                | None =>
                    Error(
                        "Invalid grammar note data at array index "
                        ++ Js.Int.toString(index),
                    )
                };
            };

        decode_rows(0, []);
    | None => Error("The grammar notes index JSON root must be an array")
    };

[@react.component]
let make = () => {
    open Bindings;
    open Mui; 

    let url = ReasonReactRouter.useUrl();
    let slug = switch url.path {
    | ["learn", "grammar_notes", slug] => Some(slug)
    | _ => None
    };
    let (grammar_notes, set_grammar_notes) =
        React.useState(() => [||]);
    let (index_error, set_index_error) =
        React.useState(() => (None: option(string)));
    let (markdown, set_markdown) =
        React.useState(() => (None: option(string)));
    let (markdown_error, set_markdown_error) =
        React.useState(() => (None: option(string)));
    let (scroll_percentage, set_scroll_percentage) =
        React.useState(() => 0);
    let grammar_note_ref: React.ref(Js.nullable(Dom.element)) =
        React.useRef(Js.Nullable.null);

    React.useEffect0(() => {
        Browser.Fetch.get("/grammar_notes/index.json")
        |> Js.Promise.then_(response => {
            if (Browser.Fetch.ok(response)) {
                response
                |> Browser.Fetch.json
                |> Js.Promise.then_(json => {
                    switch (parse_grammar_notes_index(json)) {
                    | Ok(notes) => set_grammar_notes(_ => notes)
                    | Error(message) => set_index_error(_ => Some(message))
                    };
                    Js.Promise.resolve();
                });
            } else {
                set_index_error(_ => Some("Unable to load the grammar notes index"));
                Js.Promise.resolve();
            };
        })
        |> Js.Promise.catch(_error => {
            set_index_error(_ => Some("Unable to load the grammar notes index"));
            Js.Promise.resolve();
        })
        |> ignore;

        None;
    });

    let selected_note = switch slug {
    | Some(slug) => Array.find_opt(note => note.slug == slug, grammar_notes)
    | None => None
    };
    let selected_note_file = switch selected_note {
    | Some(note) => note.file
    | None => ""
    };

    React.useEffect1(() => {
        let cancelled = ref(false);
        set_markdown(_ => None);
        set_markdown_error(_ => None);
        set_scroll_percentage(_ => 0);

        switch selected_note {
        | Some(note) =>
            Browser.Fetch.get(note.file)
            |> Js.Promise.then_(response => {
                let response_is_html =
                    switch (
                        response
                        |> Browser.Fetch.headers
                        |> Browser.Fetch.get_header("Content-Type")
                    ) {
                    | Some(content_type) =>
                        content_type
                        |> Js.String.toLowerCase
                        |> Js.String.includes(~search="text/html")
                    | None => false
                    };

                if (Browser.Fetch.ok(response) && !response_is_html) {
                    response
                    |> Browser.Fetch.text
                    |> Js.Promise.then_(content => {
                        if (!cancelled^) {
                            set_markdown(_ => Some(content));
                        };
                        Js.Promise.resolve();
                    });
                } else {
                    if (!cancelled^) {
                        set_markdown_error(_ => Some("Unable to load this grammar note"));
                    };
                    Js.Promise.resolve();
                };
            })
            |> Js.Promise.catch(_error => {
                if (!cancelled^) {
                    set_markdown_error(_ => Some("Unable to load this grammar note"));
                };
                Js.Promise.resolve();
            })
            |> ignore
        | None => ()
        };

        Some(() => cancelled := true);
    }, [|selected_note_file|]);

    let handle_scroll = (event: React.Event.UI.t) => {
        let element =
            event
            |> React.Event.UI.currentTarget
            |> dom_element_from_event_target;
        let maximum_scroll =
            ScrollableElement.scroll_height(element)
            -. ScrollableElement.client_height(element);
        let percentage =
            if (maximum_scroll <= 0.0) {
                100;
            } else {
                let calculated_percentage =
                    ScrollableElement.scroll_top(element)
                    /. maximum_scroll
                    *. 100.0
                    |> Js.Math.round
                    |> int_of_float;

                if (calculated_percentage < 0) {
                    0;
                } else if (calculated_percentage > 100) {
                    100;
                } else {
                    calculated_percentage;
                };
            };

        set_scroll_percentage(_ => percentage);
    };

    let scroll_to_top = () =>
        switch (Js.Nullable.toOption(grammar_note_ref.current)) {
        | Some(element) =>
            ScrollableElement.scroll_to(
                ScrollableElement.make_scroll_to_options(
                    ~top=0.0,
                    ~behavior="smooth",
                    (),
                ),
                element,
            )
        | None => ()
        };

    let share_grammar_note = () => {
        let _ =
            Browser.Window.location_href
            |> Browser.Clipboard.write_text
            |> Js.Promise.catch(error => {
                Js.log2("Could not copy the grammar note URL:", error);
                Js.Promise.resolve();
            });
        ();
    };

    <Grid className=css##grammarNotesContainer>
        {
            switch selected_note {
                | Some(note) =>
                    <>
                        <Box className=css##readProgress>
                            <CircularProgress
                                variant=`determinate
                                value={scroll_percentage |> Int.to_float}
                                enableTrackSlot=true
                            />
                            <Box
                                sx={{
                                    "top": 0,
                                    "left": 0,
                                    "bottom": 0,
                                    "right": 0,
                                    "position": "absolute",
                                    "display": "flex",
                                    "alignItems": "center",
                                    "justifyContent": "center",
                                }}
                            >
                                <Typography
                                    variant=Typography.Variant.caption
                                    component=RootComponent.htmlElement("div")
                                    sx={{ "color": "text.secondary" }}
                                >
                                    {(scroll_percentage |> Int.to_string) ++ "%" |> React.string}
                                </Typography>
                            </Box>
                        </Box>
                        <div
                            className=css##grammarNote
                            onScroll=handle_scroll
                            ref={ReactDOM.Ref.domRef(grammar_note_ref)}
                        >
                            <Typography 
                                variant=Typography.Variant.h3
                                align=`center
                                className=css##grammarNoteTitle
                                sx={{"padding": "20px", "backgroundColor": Config.colors##whiteSmoke}}
                            > 
                                {note.title |> React.string}
                            </Typography>
                            <Container
                                className=css##grammarNoteContent
                            >
                                {
                                switch (markdown, markdown_error) {
                                | (Some(content), _) =>
                                    <ReactMarkdown
                                        markdown=content
                                        remarkPlugins=[|ReactMarkdown.remarkGfm|]
                                        rehypePlugins=[|ReactMarkdown.rehypeCuneiform|]
                                    />
                                    | (None, Some(message)) =>
                                        <p> {message |> React.string} </p>
                                    | (None, None) =>
                                        <p> {"Loading grammar note..." |> React.string} </p>
                                    }
                                }
                            </Container>
                        </div>
                    </>
                | None =>
                    switch index_error {
                    | Some(message) => <p> {message |> React.string} </p>
                    | None =>
                        <Container
                            sx={{
                                "display": "flex",
                                "flexDirection": "column",
                                "alignItems": "center",
                                "padding": "40px 20px",
                            }}
                        >
                            <Typography variant=Typography.Variant.h4>
                                {"Available grammar notes" |> React.string}
                            </Typography>
                            <List sx={{"width": "100%", "maxWidth": "600px"}}>
                                {
                                    grammar_notes
                                    |> Array.map(note =>
                                        <ListItem key=note.slug disablePadding=true>
                                            <ListItemButton
                                                onClick={_ =>
                                                    ReasonReactRouter.push(
                                                        "/learn/grammar_notes/" ++ note.slug,
                                                    )
                                                }
                                            >
                                                <ListItemIcon>
                                                    <TablerReact.IconNote />
                                                </ListItemIcon>
                                                <ListItemText
                                                    primary={note.title |> React.string}
                                                />
                                            </ListItemButton>
                                        </ListItem>
                                    )
                                    |> React.array
                                }
                            </List>
                        </Container>
                    }
            }
        }
        <Stack 
            className=css##sideButtons
            useFlexGap=true
            spacing=`Number(1)
        >
            <Tooltip 
                title={"Share this grammar note" |> React.string}
                placement=Tooltip.Placement.left
                arrow=true
            >
                <IconButton
                    size=`small
                    ariaLabel="Share this grammar note"
                    onClick={_ => share_grammar_note()}
                >
                    <TablerReact.IconLink color=Config.colors##protonRed />
                </IconButton>
            </Tooltip>
            <Tooltip 
                title={"Scroll to the top" |> React.string}
                placement=Tooltip.Placement.left
                arrow=true
            >
                <IconButton
                    size=`small
                    ariaLabel="Scroll to the top of the grammar note"
                    onClick={_ => scroll_to_top()}
                >
                    <TablerReact.IconArrowBigUpLinesFilled color=Config.colors##protonRed />
                </IconButton>
            </Tooltip>
        </Stack>
    </Grid>
}
