[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

type view =
    | DailyVocabulary
    | Flashcards
    | Lessons;

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (is_drawer_open, set_drawer_open) = React.useState(() => false);
    let (current_view, set_current_view) = React.useState(() => None);

    let drawer_width = is_drawer_open ? "280px" : "64px";
    let drawer_transition = "width 225ms cubic-bezier(0.4, 0, 0.6, 1)";
    let select_view = key => switch key {
        | "daily_vocabulary" => set_current_view(_ => Some(DailyVocabulary))
        | "flashcards" => set_current_view(_ => Some(Flashcards))
        | "lessons" => set_current_view(_ => Some(Lessons))
        | _ => set_current_view(_ => None)
    };

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
                </ListItemButton>
            </Tooltip>
        </ListItem>;

    <Container 
        className={css##learnContainer}
        disableGutters=true
    >
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
            </List>
        </Drawer>
        <Box sx={{"width": "100%"}}>
            {
                switch current_view {
                    | None => <Learn_welcome set_current_view=select_view />
                    | Some(view) =>
                        switch view {
                            | DailyVocabulary => <Learn_daily_vocabulary />
                            | Flashcards => <Learn_flashcards />
                            | Lessons => <Learn_lessons />
                        }
                }
            }
        </Box>
    </Container>
}
