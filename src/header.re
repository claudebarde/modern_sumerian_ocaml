[@mel.module "./Header.module.scss"] external css: Js.t({..}) = "default"; 
[@mel.scope ("process", "env")] external node_env: string = "NODE_ENV";
[@mel.module "./assets/logo.png"] external logoImage: string = "default";
external dom_element_from_event_target: Js.t({..}) => Dom.element = "%identity";

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (toolsAnchor, setToolsAnchor) =
        React.useState(() =>
            (Js.Nullable.null: Js.Nullable.t(Dom.element))
        );
    let (mobileMenuOpen, setMobileMenuOpen) = React.useState(() => false);

    let openToolsMenu = !Js.Nullable.isNullable(toolsAnchor);
    let closeToolsMenu = () => setToolsAnchor(_ => Js.Nullable.null);
    let navigateFromToolsMenu = path => {
        closeToolsMenu();
        ReasonReactRouter.push(path);
    };

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
                    color=Color.fromString(Config.colors##protonRed)
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
                </Menu>
                <Button
                    variant=`text
                    color=Color.fromString(Config.colors##protonRed)
                    onClick={_ => ReasonReactRouter.push("/games")}
                >
                    {"Games" |> React.string}
                </Button>
                <Button
                    variant=`text
                    color=Color.fromString(Config.colors##protonRed)
                    onClick={_ => ReasonReactRouter.push("/lessons")}
                >
                    {"Lessons" |> React.string}
                </Button>
                <Button
                    variant=`text
                    color=Color.fromString(Config.colors##protonRed)
                    onClick={_ => ReasonReactRouter.push("/links")}
                >
                    {"Links" |> React.string}
                </Button>
            </Box>
            // SMALLER SCREEN MENU
            <Box className=css##hamburgerMenu>
                <IconButton 
                    color=Color.secondary
                    onClick={_ => setMobileMenuOpen(_ => true)}
                >
                    <TablerReact.IconMenu2 />
                </IconButton>
                <Drawer
                    anchor=`right
                    _open=mobileMenuOpen
                    onClose={_ => setMobileMenuOpen(_ => false)}
                    sx={{
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
                                ReasonReactRouter.push("/lessons");
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
};
