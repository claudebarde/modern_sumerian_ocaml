[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

type view =
    | DailyVocabulary
    | Flashcards
    | Lessons
    | GrammarNotes;

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let url = ReasonReactRouter.useUrl();
    let is_grammar_notes_route = switch url.path {
    | ["learn", "grammar_notes"]
    | ["learn", "grammar_notes", _] => true
    | _ => false
    };
    let (is_drawer_open, set_drawer_open) =
        React.useState(() => is_grammar_notes_route);
    let (grammar_notes_nav_open, set_grammar_notes_nav_open) = React.useState(() => true);
    let (grammar_notes, set_grammar_notes) =
        React.useState(() => ([||]: array(Learn_grammar_notes.grammar_note)));
    let selected_grammar_note_slug = switch url.path {
    | ["learn", "grammar_notes", slug] => Some(slug)
    | _ => None
    };
    let current_view = switch url.path {
        | ["learn"] => None
        | ["learn", "daily_vocabulary"] => Some(DailyVocabulary)
        | ["learn", "flashcards"] => Some(Flashcards)
        | ["learn", "lessons"] => Some(Lessons)
        | ["learn", "grammar_notes"]
        | ["learn", "grammar_notes", _] => Some(GrammarNotes)
        | _ => None
    };

    React.useEffect1(() => {
        if (is_grammar_notes_route) {
            set_drawer_open(_ => true);
        };
        None;
    }, [|is_grammar_notes_route|]);

    React.useEffect0(() => {
        Browser.Fetch.get("/grammar_notes/index.json")
        |> Js.Promise.then_(response => {
            if (Browser.Fetch.ok(response)) {
                response
                |> Browser.Fetch.json
                |> Js.Promise.then_(json => {
                    switch (Learn_grammar_notes.parse_grammar_notes_index(json)) {
                    | Ok(notes) => set_grammar_notes(_ => notes)
                    | Error(message) =>
                        Js.log2("Unable to parse the grammar notes index:", message)
                    };
                    Js.Promise.resolve();
                });
            } else {
                Js.log("Unable to load the grammar notes index");
                Js.Promise.resolve();
            };
        })
        |> Js.Promise.catch(error => {
            Js.log2("Unable to load the grammar notes index:", error);
            Js.Promise.resolve();
        })
        |> ignore;

        None;
    });

    let drawer_width = is_drawer_open ? "280px" : "64px";
    let drawer_transition = "width 225ms cubic-bezier(0.4, 0, 0.6, 1)";
    let select_view = key => switch key {
        | "daily_vocabulary" => ReasonReactRouter.push("/learn/daily_vocabulary")
        | "flashcards" => ReasonReactRouter.push("/learn/flashcards")
        | "lessons" => ReasonReactRouter.push("/learn/lessons")
        | "grammar_notes" => ReasonReactRouter.push("/learn/grammar_notes")
        | _ => ReasonReactRouter.push("/learn")
    };

    let is_mobile = UseMediaQuery.use("(max-width:599px)");

    let navigation_item = (~key, ~label, ~icon) =>
        <ListItem key disablePadding=true sx={{"display": "block"}}>
            <Tooltip
                title={is_drawer_open ? React.null : label |> React.string}
                placement=Tooltip.Placement.right
                arrow=true
            >
                <ListItemButton
                    selected={switch current_view {
                        | Some(DailyVocabulary) => key == "daily_vocabulary"
                        | Some(Flashcards) => key == "flashcards"
                        | Some(Lessons) => key == "lessons"
                        | Some(GrammarNotes) => key == "grammar_notes"
                        | None => false
                    }}
                    sx={{
                        "minHeight": "48px",
                        "justifyContent": is_drawer_open ? "initial" : "center",
                        "padding": is_drawer_open ? "8px 16px" : "8px 0",
                    }}
                    onClick={_ => select_view(key)}
                >
                    <ListItemIcon
                        sx={{
                            "minWidth": is_drawer_open ? "40px" : "0",
                            "marginRight": is_drawer_open ? "8px" : "0",
                            "justifyContent": "center",
                        }}
                    >
                        {icon}
                    </ListItemIcon>
                    <ListItemText
                        primary={label |> React.string}
                        sx={{
                            "display": is_drawer_open ? "block" : "none",
                            "whiteSpace": "nowrap",
                        }}
                    />
                    {
                        switch key {
                        | "grammar_notes" =>
                            <IconButton
                                ariaLabel={grammar_notes_nav_open ? "Collapse grammar notes section" : "Expand grammar notes section"}
                                onClick={ev => {
                                    React.Event.Mouse.stopPropagation(ev);
                                    set_grammar_notes_nav_open(prev => !prev);
                                }}
                            >
                                {grammar_notes_nav_open ? <TablerReact.IconChevronUp /> : <TablerReact.IconChevronDown />}
                            </IconButton>
                        | _ => React.null
                        }
                    }
                </ListItemButton>
            </Tooltip>
        </ListItem>;

    <Container 
        className={css##learnContainer}
        maxWidth=MaxWidth.disabled
        disableGutters=true
    >
        {
            if (is_mobile) {
                React.null
            } else {
                <Drawer 
                    variant=`permanent 
                    _open=is_drawer_open
                    sx={{                
                        "width": drawer_width,
                        "height": "100%",
                        "flexShrink": 0,
                        "whiteSpace": "nowrap",
                        "transition": drawer_transition,
                        "& .MuiDrawer-paper": {
                            "position": "absolute",
                            "top": "0",
                            "bottom": "0",
                            "height": "100%",
                            "width": drawer_width,
                            "maxWidth": "100%",
                            "boxSizing": "border-box",
                            "overflowX": "hidden",
                            "transition": drawer_transition,
                            "backgroundColor": Config.colors##cerealFlake,
                        },
                    }}
                >
                    <Box
                        sx={{
                            "display": "flex",
                            "alignItems": "center",
                            "justifyContent": is_drawer_open ? "flex-end" : "center",
                            "minHeight": "56px",
                            "padding": is_drawer_open ? "0 8px" : "0",
                        }}
                    >
                        <IconButton
                            ariaLabel={is_drawer_open ? "Collapse navigation" : "Expand navigation"}
                            onClick={_ => set_drawer_open(open_ => !open_)}
                        >
                            {
                                is_drawer_open
                                    ? <TablerReact.IconChevronLeft color=Config.colors##darkRift />
                                    : <TablerReact.IconChevronRight color=Config.colors##darkRift />
                            }
                        </IconButton>
                    </Box>
                    <Divider />
                    <List 
                        disablePadding=true
                    >
                        {
                            navigation_item(
                                ~key="daily_vocabulary",
                                ~label="Daily Vocabulary",
                                ~icon=<TablerReact.IconListCheck color=Config.colors##darkRift />,
                            )
                        }
                        {
                            navigation_item(
                                ~key="lessons",
                                ~label="Lessons",
                                ~icon=<TablerReact.IconBook2 color=Config.colors##darkRift />,
                            )
                        }
                        {
                            navigation_item(
                                ~key="flashcards",
                                ~label="Flashcards",
                                ~icon=<TablerReact.IconPhoto color=Config.colors##darkRift />,
                            )
                        }
                        {
                            navigation_item(
                                ~key="grammar_notes",
                                ~label="Grammar Notes",
                                ~icon=<TablerReact.IconPencil color=Config.colors##darkRift />,
                            )
                        }
                        {
                            is_grammar_notes_route
                            ? <Collapse in_=grammar_notes_nav_open sx={{"marginLeft": "16px"}}>
                                <List
                                    sx={{"margin": "0px", "padding": "0px"}}
                                >
                                    {
                                        grammar_notes
                                        |> Array.map((note: Learn_grammar_notes.grammar_note) =>
                                            <ListItem key=note.slug disablePadding=true>
                                                <ListItemButton
                                                    selected={selected_grammar_note_slug == Some(note.slug)}
                                                    onClick={_ =>
                                                        ReasonReactRouter.push(
                                                            "/learn/grammar_notes/" ++ note.slug,
                                                        )
                                                    }
                                                >
                                                    <ListItemIcon>
                                                    <TablerReact.IconNote />
                                                    </ListItemIcon>
                                                    <ListItemText primary={note.title |> React.string} />
                                                </ListItemButton>
                                            </ListItem>
                                        )
                                        |> React.array
                                    }
                                </List>
                            </Collapse>
                            : React.null
                        }
                    </List>
                </Drawer>
            }
        }
        <Box sx={{"width": "100%"}}>
            {
                switch current_view {
                    | None => <Learn_welcome set_current_view=select_view />
                    | Some(view) =>
                        switch view {
                            | DailyVocabulary => <Learn_daily_vocabulary />
                            | Flashcards => <Learn_flashcards />
                            | Lessons => <Learn_lessons />
                            | GrammarNotes => <Learn_grammar_notes />
                        }
                }
            }
        </Box>
    </Container>
}
