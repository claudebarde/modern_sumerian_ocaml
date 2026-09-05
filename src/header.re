[@mel.module "./Header.module.scss"] external css: Js.t({..}) = "default"; 
[@mel.scope ("process", "env")] external node_env: string = "NODE_ENV";
[@mel.module "./assets/logo.png"] external logoImage: string = "default";
external dom_element_from_event_target: Js.t({..}) => Dom.element = "%identity";
[@mel.send] external focus_element: Dom.element => unit = "focus";

[@react.component]
let make = () => {
    open Bindings;
    open Mui;
    open Components.Web_utils;
    open Components.Store;

    let (toolsAnchor, setToolsAnchor) =
        React.useState(() =>
            (Js.Nullable.null: Js.Nullable.t(Dom.element))
        );
    let (userAnchor, setUserAnchor) =
        React.useState(() =>
            (Js.Nullable.null: Js.Nullable.t(Dom.element))
        );
    let (mobileMenuOpen, setMobileMenuOpen) = React.useState(() => false);
    let (isSignupDialogOpen, setSignupDialogOpen) = React.useState(() => false);
    let (isSettingsDialogOpen, setSettingsDialogOpen) = React.useState(() => false);
    let settingsButtonRef: React.ref(Js.nullable(Dom.element)) =
        React.useRef(Js.Nullable.null);

    let openToolsMenu = !Js.Nullable.isNullable(toolsAnchor);
    let closeToolsMenu = () => setToolsAnchor(_ => Js.Nullable.null);
    let navigateFromToolsMenu = path => {
        closeToolsMenu();
        ReasonReactRouter.push(path);
    };
    let openUserMenu = !Js.Nullable.isNullable(userAnchor);
    let closeUserMenu = () => setUserAnchor(_ => Js.Nullable.null);
    let restoreSettingsButtonFocus = () => {
        // MUI calls onTransitionExited just before it removes aria-hidden from
        // the application root, so defer focus restoration by one task.
        let _ = Js.Global.setTimeout(~f=() => {
            switch (Js.Nullable.toOption(settingsButtonRef.current)) {
            | Some(button) => focus_element(button)
            | None => ()
            };
        }, 0);
        ();
    };

    let displayLanguage =
        app_store |> Zustand.use_store(store => store.display_language);

    <>
        <AppBar 
            position=`static
            color=Color.transparent
            sx={{"backgroundColor": Config.colors##cerealFlake}}
            className=css##appbar
        >
            <Toolbar variant=Toolbar.Variant.regular className=css##toolbar>
                <img 
                    src=logoImage
                    alt="logo"
                    className=css##logo
                    onClick={_ => ReasonReactRouter.push("/")}
                />
                <Box className={css##rotatingTitle}>
                    <Typography
                        className={css##titlePrimary}
                        variant=Typography.Variant.h6
                    >
                        {[|"eme", {js|ĝir15|js}, "u", "me", "e"|]
                        |> Components.Web_utils.display_cuneiforms
                        |> Array.mapi((i, (codePoint, word)) => {
                            <span
                                className="cuneiforms" 
                                key={codePoint ++ word ++ Int.to_string(i)} 
                            >
                                {React.string(codePoint)}
                            </span>
                        })
                        |> React.array}
                    </Typography>
                    <Typography
                        className={css##titleSecondary}
                        variant=Typography.Variant.h6
                    >
                        {"MODERN SUMERIAN"|>React.string}
                    </Typography>                
                </Box>
                // LARGER SCREEN MENU
                <Box className=css##navMenu>
                    <Button
                        variant=`text
                        color=Color.secondary
                        endIcon={<TablerReact.IconChevronDown />}
                        onClick={event =>
                            setToolsAnchor(_ =>
                                React.Event.Mouse.currentTarget(event)
                                |> dom_element_from_event_target
                                |> Js.Nullable.return
                            )
                        }
                    >
                        {"Tools" |> React.string}
                    </Button>
                    <Menu
                        _open=openToolsMenu
                        anchorEl=toolsAnchor
                        anchorOrigin={vertical: `bottom, horizontal: `right}
                        transformOrigin={vertical: `top, horizontal: `right}
                        onClose={_ => closeToolsMenu()}
                    >
                        <MenuItem
                            onClick={_ => navigateFromToolsMenu("/conjugator")}
                        >
                            <ListItemIcon>
                                <TablerReact.IconTable color=Config.colors##botanicalNight />
                            </ListItemIcon>
                            <ListItemText>
                                {"Conjugator" |> React.string}
                            </ListItemText>
                        </MenuItem>
                        <MenuItem
                            onClick={_ => navigateFromToolsMenu("/dictionary")}
                        >
                            <ListItemIcon>
                                <TablerReact.IconBook2 color=Config.colors##botanicalNight />
                            </ListItemIcon>
                            <ListItemText>
                                {"Dictionary" |> React.string}
                            </ListItemText>
                        </MenuItem>
                        <MenuItem
                            onClick={_ => navigateFromToolsMenu("/keyboard")}
                        >
                            <ListItemIcon>
                                <TablerReact.IconKeyboard color=Config.colors##botanicalNight />
                            </ListItemIcon>
                            <ListItemText>
                                {"Keyboard" |> React.string}
                            </ListItemText>
                        </MenuItem>
                        <MenuItem
                            onClick={_ => navigateFromToolsMenu("/wordslist")}
                        >
                            <ListItemIcon>
                                <TablerReact.IconListCheck color=Config.colors##botanicalNight />
                            </ListItemIcon>
                            <ListItemText>
                                {"My Words List" |> React.string}
                            </ListItemText>
                        </MenuItem>
                        <MenuItem
                            onClick={_ => navigateFromToolsMenu("/worldmap")}
                        >
                            <ListItemIcon>
                                <TablerReact.IconWorldMap color=Config.colors##botanicalNight />
                            </ListItemIcon>
                            <ListItemText>
                                {"World Map" |> React.string}
                            </ListItemText>
                        </MenuItem>
                    </Menu>
                    <Button
                        variant=`text
                        color=Color.secondary
                        onClick={_ => ReasonReactRouter.push("/games")}
                    >
                        {"Games" |> React.string}
                    </Button>
                    <Button
                        variant=`text
                        color=Color.secondary
                        onClick={_ => ReasonReactRouter.push("/learn")}
                    >
                        {"Learn" |> React.string}
                    </Button>
                    <Stack direction=`row spacing=`Number(0)>
                        <IconButton
                            color=Color.secondary
                            size=`small
                            onClick={_ => ReasonReactRouter.push("/links")}
                        >
                            <TablerReact.IconLinkFilled />
                        </IconButton>
                        <IconButton
                            color=Color.secondary
                            size=`small
                            onClick={event =>
                                setUserAnchor(_ =>
                                    React.Event.Mouse.currentTarget(event)
                                    |> dom_element_from_event_target
                                    |> Js.Nullable.return
                                )
                            }
                        >
                            <TablerReact.IconUserOff />
                        </IconButton>
                        <Menu
                            _open=openUserMenu
                            anchorEl=userAnchor
                            anchorOrigin={vertical: `bottom, horizontal: `right}
                            transformOrigin={vertical: `top, horizontal: `right}
                            onClose={_ => closeUserMenu()}
                        >
                            <MenuItem
                                onClick={_ => {
                                    closeUserMenu()
                                    setSignupDialogOpen(_ => true)
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconUserPlus color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {
                                        {
                                            Translation.display_to(
                                                ~sentence="sign_up", 
                                                ~language=displayLanguage, 
                                                ~size=Some(Translation.Small))
                                            }
                                    }
                                </ListItemText>
                            </MenuItem>
                            <MenuItem
                                onClick={_ => Js.log("Sign In clicked")}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconUserCheck color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {
                                        Translation.display_to(
                                            ~sentence="sign_in", 
                                            ~language=displayLanguage, 
                                            ~size=Some(Translation.Small))
                                    }
                                </ListItemText>
                            </MenuItem>
                        </Menu>
                        <IconButton
                            color=Color.secondary
                            size=`small
                            onClick={event => {
                                settingsButtonRef.current =
                                    React.Event.Mouse.currentTarget(event)
                                    |> dom_element_from_event_target
                                    |> Js.Nullable.return;
                                setSettingsDialogOpen(_ => true);
                            }}
                        >
                            <TablerReact.IconSettings />
                        </IconButton>
                    </Stack>
                </Box>
                // MOBILE VIEW MENU
                <Box className=css##hamburgerMenu>
                    <IconButton 
                        color=Color.secondary
                        onClick={_ => setMobileMenuOpen(state => !state)}
                    >
                        {
                            mobileMenuOpen 
                            ? <TablerReact.IconX color=Config.colors##botanicalNight /> 
                            : <TablerReact.IconMenu2 color=Config.colors##botanicalNight />
                        }
                    </IconButton>
                    <Drawer
                        anchor=`right
                        _open=mobileMenuOpen
                        onClose={_ => setMobileMenuOpen(_ => false)}
                        sx={{
                            "zIndex": 1202,
                            "& .MuiDrawer-paper": {
                                "width": "min(60vw, 360px)",
                                "boxSizing": "border-box",
                            },
                        }}
                    >
                        <List sx={{"height": "100%", "position": "relative"}}>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconHome color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Home" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListSubheader>
                                {"Tools" |> React.string}
                            </ListSubheader>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/conjugator");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>  
                                    <TablerReact.IconTable color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Conjugator" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/dictionary");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconBook2 color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Dictionary" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/keyboard");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconKeyboard color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Keyboard" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/wordslist");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconListCheck color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Words List" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListSubheader>
                                {"Learn" |> React.string}
                            </ListSubheader>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/learn/flashcards");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconPhoto color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Flashcards" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/learn/daily_vocabulary");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconListCheck color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Daily Vocabulary" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/learn/lessons");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconBook2 color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Lessons" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/learn/grammar_notes");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconPencil color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Grammar Notes" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <ListSubheader>
                                {"More" |> React.string}
                            </ListSubheader>
                            <ListItemButton
                                onClick={_ => {
                                    ReasonReactRouter.push("/links");
                                    setMobileMenuOpen(_ => false);
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconLink color=Config.colors##botanicalNight />
                                </ListItemIcon>
                                <ListItemText>
                                    {"Links" |> React.string}
                                </ListItemText>
                            </ListItemButton>
                            <Divider />
                            <ListItemText sx={{"position": "absolute", "bottom": "0", "left": "0", "width": "100%", "textAlign": "left", "padding": "1rem"}}>
                                <p>{{js|© 2025 Modern Sumerian.|js}|>React.string}</p>
                                <p>{{js|All rights reserved.|js}|>React.string}</p>
                            </ListItemText>
                        </List>
                    </Drawer>
                </Box>
            </Toolbar>
        </AppBar>
        <Components.User_signing 
            isSignupDialogOpen=isSignupDialogOpen
            setSignupDialogOpen=setSignupDialogOpen
            isSignUp=true
        />
        <Components.Settings_dialog
            isSettingsDialogOpen=isSettingsDialogOpen
            setSettingsDialogOpen=setSettingsDialogOpen
            restoreSettingsButtonFocus=restoreSettingsButtonFocus
        />
    </>
};
