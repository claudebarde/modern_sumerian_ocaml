[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

let remove_frontmatter: string => string = [%mel.raw {|
    markdown => markdown.replace(/^---\r?\n[\s\S]*?\r?\n---\r?\n?/, "")
|}];

type expanded_comic = 
  | Cuneiform
  | Transliteration;

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let url = ReasonReactRouter.useUrl();
    let requested_comic = switch url.path {
    | ["learn", "comics", strip] => Some(strip)
    | _ => None
    };
    let selected_comic = switch requested_comic {
    | Some(strip) => strip
    | None => "strip-1"
    };
    let selected_comic_menu_value = switch selected_comic {
    | "strip-1" => selected_comic
    | _ => ""
    };
    let (path_to_cuneiform_img, set_path_to_cuneiform_img) = React.useState(() => "/public/comics/strip-1/sux-cuneiform.png");
    let (path_to_transliteration_img, set_path_to_transliteration_img) = React.useState(() => "/public/comics/sux-transliteration.png");
    let (grammar_notes, set_grammar_notes) = React.useState(() => None);
    let (grammar_notes_error, set_grammar_notes_error) = React.useState(() => None);
    let (expanded_comic, set_expanded_comic) = React.useState(() => Cuneiform);
    let (link_copied_tooltip_open, set_link_copied_tooltip_open) =
        React.useState(() => false);
    let link_copied_tooltip_timeout =
        React.useRef((None: option(Js.Global.timeoutId)));

    React.useEffect0(() =>
        Some(() =>
            switch link_copied_tooltip_timeout.current {
            | Some(timeout_id) => Js.Global.clearTimeout(timeout_id)
            | None => ()
            }
        )
    );

    React.useEffect1(() => {
        // rebuilds the path to images based on the selected comic
        set_path_to_cuneiform_img(_ => "/public/comics/" ++ selected_comic ++ "/sux-cuneiform.png");
        set_path_to_transliteration_img(_ => "/public/comics/" ++ selected_comic ++ "/sux-transliteration.png");
        None
    }, [|selected_comic|]);

    React.useEffect1(() => {
        let cancelled = ref(false);
        let grammar_notes_path =
            "/comics/" ++ selected_comic ++ "/grammar-notes.md";

        set_grammar_notes(_ => None);
        set_grammar_notes_error(_ => None);

        Browser.Fetch.get(grammar_notes_path)
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
                |> Js.Promise.then_(text => {
                    if (!cancelled^) {
                        set_grammar_notes(_ => Some(remove_frontmatter(text)));
                    };
                    Js.Promise.resolve();
                });
            } else {
                if (!cancelled^) {
                    switch requested_comic {
                    | Some(_) => ReasonReactRouter.replace("/learn/comics")
                    | None =>
                        set_grammar_notes_error(_ => Some("Unable to load the comic grammar notes"))
                    };
                };
                Js.Promise.resolve();
            };
        })
        |> Js.Promise.catch(_error => {
            if (!cancelled^) {
                set_grammar_notes_error(_ => Some("Unable to load the comic grammar notes"));
            };
            Js.Promise.resolve();
        })
        |> ignore;

        Some(() => cancelled := true);
    }, [|selected_comic|]);

    let show_link_copied_tooltip = () => {
        switch link_copied_tooltip_timeout.current {
        | Some(timeout_id) => Js.Global.clearTimeout(timeout_id)
        | None => ()
        };
        set_link_copied_tooltip_open(_ => true);
        let timeout_id = Js.Global.setTimeout(
            ~f=() => {
                set_link_copied_tooltip_open(_ => false);
                link_copied_tooltip_timeout.current = None;
            },
            1500,
        );
        link_copied_tooltip_timeout.current = Some(timeout_id);
    };

    let copy_comic_url = () => {
        let comic_url =
            Browser.Window.location_origin
            ++ "/learn/comics/"
            ++ Js.Global.encodeURIComponent(selected_comic);

        comic_url
        |> Browser.Clipboard.write_text
        |> Js.Promise.then_(_ => {
            show_link_copied_tooltip();
            Js.Promise.resolve();
        })
        |> Js.Promise.catch(error => {
            Js.log2("Could not copy the comic URL:", error);
            Js.Promise.resolve();
        })
        |> ignore;
    };

    <Container className=css##comics>
        <Box className=css##comicsHeader>
            <Typography variant=Typography.Variant.h4>
                {"Comics" |> React.string}
            </Typography>
            <FormControl size=`small>
                <Select 
                    labelId="comic-select-label"
                    value={Select.Value.fromString(selected_comic_menu_value)}
                    onChange={(event, _) =>
                        ReasonReactRouter.push(
                            "/learn/comics/" ++ event##target##value,
                        )
                    }
                >
                    <MenuItem value="strip-1">{"#1 Kakug and his dog" |> React.string}</MenuItem>
                </Select>
            </FormControl>
            <Tooltip
                title={"Link copied!" |> React.string}
                disableFocusListener=true
                disableHoverListener=true
                disableTouchListener=true
                _open=link_copied_tooltip_open
                onClose={_ => set_link_copied_tooltip_open(_ => false)}
            >
                <IconButton
                    ariaLabel="Copy link to this comic"
                    onClick={_ => copy_comic_url()}
                >
                    <TablerReact.IconShare3 />
                </IconButton>
            </Tooltip>
        </Box>
        <Grid 
            container=true 
            spacing=`Number(2) 
            direction=`row
            className=css##comicsContent
        >
            <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=6, ())) className=css##comicViewer>
                <Accordion 
                    expanded={expanded_comic == Cuneiform} 
                    disableGutters=true
                    onChange={(_, isExpanded) => 
                        set_expanded_comic(_ => if (isExpanded) {Cuneiform} else {Transliteration})}
                >
                    <AccordionSummary
                        expandIcon={<TablerReact.IconChevronDown />}
                        sx={{
                            "backgroundColor": expanded_comic == Cuneiform ? "white" :Config.colors##silverSetting,
                            "minHeight": "32px",
                            "padding": "0 4px",
                            "&.Mui-expanded": {"minHeight": "32px"},
                            "& .MuiAccordionSummary-content": {"margin": "2px 10px"},
                            "& .MuiAccordionSummary-content.Mui-expanded": {"margin": "2px 10px"},
                        }}
                    >
                        {"Cuneiform" |> React.string}
                    </AccordionSummary>
                    <AccordionDetails>
                        <img src={path_to_cuneiform_img} alt="Comic 1" className=css##comicImage />
                    </AccordionDetails>
                </Accordion>
                <Accordion 
                    expanded={expanded_comic == Transliteration}  
                    disableGutters=true
                    onChange={(_, isExpanded) => 
                        set_expanded_comic(_ => if (isExpanded) {Transliteration} else {Cuneiform})}
                >
                    <AccordionSummary
                        expandIcon={<TablerReact.IconChevronDown />}
                        sx={{
                            "backgroundColor": expanded_comic == Transliteration ? "white" :Config.colors##silverSetting,
                            "minHeight": "32px",
                            "padding": "0 4px",
                            "&.Mui-expanded": {"minHeight": "32px"},
                            "& .MuiAccordionSummary-content": {"margin": "2px 10px"},
                            "& .MuiAccordionSummary-content.Mui-expanded": {"margin": "2px 10px"},
                        }}
                    >
                        {"Transliteration" |> React.string}
                    </AccordionSummary>
                    <AccordionDetails>
                        <img src={path_to_transliteration_img} alt="Comic 1" className=css##comicImage />
                    </AccordionDetails>
                </Accordion>            
            </Grid>
            <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=6, ()))>
                {
                    switch (grammar_notes, grammar_notes_error) {
                    | (Some(markdown), _) =>
                        <ReactMarkdown
                            markdown
                            remarkPlugins=[|ReactMarkdown.remarkGfmWithoutSingleTilde|]
                            rehypePlugins=[|ReactMarkdown.rehypeCuneiform|]
                        />
                    | (None, Some(message)) => <p>{message |> React.string}</p>
                    | (None, None) => React.null
                    }
                }
            </Grid>
        </Grid>
    </Container>
}
