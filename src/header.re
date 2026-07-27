[@mel.module "./Header.module.scss"] external css: Js.t({..}) = "default"; 
[@mel.scope ("process", "env")] external node_env: string = "NODE_ENV";
[@mel.module "./assets/logo.png"] external logoImage: string = "default";

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (toolsAnchor, setToolsAnchor) = React.useState(() => Js.Nullable.null);
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
            <Box sx={{"display": "flex", "flexDirection": "row", "alignItems": "center", "gap": "1rem"}}>
                <Button
                    variant=`text
                    color=Color.fromString(Config.colors##protonRed)
                    onClick={_ => ReasonReactRouter.push("/")}
                >
                    {"Home" |> React.string}
                </Button>
                <Button
                    variant=`text
                    color=Color.fromString(Config.colors##protonRed)
                    endIcon={<TablerReact.IconChevronDown />}
                    onClick={event =>
                        setToolsAnchor(_ =>
                            React.Event.Mouse.currentTarget(event)
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
                        onClick={_ => navigateFromToolsMenu("/dictionary")}
                    >
                        <ListItemIcon>
                            <TablerReact.IconBook2 color=Config.colors##botanicalNight />
                        </ListItemIcon>
                        <ListItemText>
                            {"Dictionary" |> React.string}
                        </ListItemText>
                    </MenuItem>
                </Menu>
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
        </Toolbar>
    </AppBar>

    // <header>
    //     <div>
    //         <h1>
    //             {[|"eme", {js|ĝir15|js}, "u", "me", "e"|]
    //             |> Components.Web_utils.display_cuneiforms
    //             |> Array.mapi((i, (codePoint, word)) => {
    //                 <span
    //                     className="cuneiforms" 
    //                     key={codePoint ++ word ++ Int.to_string(i)} 
    //                 >
    //                     {React.string(codePoint)}
    //                 </span>
    //             })
    //             |> React.array}
    //         </h1>
    //     </div>
    //     <div className={css##title}>
    //         <h1>{"Modern Sumerian"|>React.string}</h1>
    //         <img 
    //             src=betaButtonImage
    //             alt="beta"
    //         />
    //     </div>
    //     <div>
    //             <nav className={css##navColumn} role="navigation">
    //                 <ul>
    //                     <li>
    //                         <a
    //                             className={
    //                                 switch (List.nth_opt(url.path, 0)) {
    //                                 | Some(_) => ""
    //                                 | None => css##active
    //                                 }
    //                             }
    //                             onClick={_ => {
    //                                 ReasonReactRouter.push("/")
    //                             }}>
    //                             {"Home"|>React.string}
    //                         </a>
    //                     </li>
    //                         <>
    //                             <li>
    //                                 <a
    //                                     className={
    //                                         switch (List.nth_opt(url.path, 0)) {
    //                                         | Some(path) when path === "conjugator" => css##active
    //                                         | Some(path) when path === "keyboard" => css##active
    //                                         | Some(path) when path === "dictionary" => css##active
    //                                         | _ => ""
    //                                         }
    //                                     }>
    //                                     {"Tools"|>React.string}
    //                                 </a>
    //                                 <ul className={css##dropdown}>
    //                                     <li>
    //                                         <a 
    //                                             onClick={_ => { ReasonReactRouter.push("conjugator") }}
    //                                         >
    //                                             {"Conjugator"|>React.string}
    //                                         </a>
    //                                     </li>
    //                                     <li>
    //                                         <a 
    //                                             onClick={_ => { ReasonReactRouter.push("keyboard") }}
    //                                         >
    //                                             {"Keyboard"|>React.string}
    //                                         </a>
    //                                     </li>
    //                                     <li>
    //                                         <a 
    //                                             onClick={_ => { ReasonReactRouter.push("dictionary") }}
    //                                         >
    //                                             {"Dictionary"|>React.string}
    //                                         </a>
    //                                     </li>
    //                                 </ul>
    //                             </li>
    //                             <li>
    //                                 <a
    //                                     className={
    //                                         switch (List.nth_opt(url.path, 0)) {
    //                                         | Some(path) when path === "lessons" => css##active
    //                                         | _ => ""
    //                                         }
    //                                     }
    //                                     onClick={_ => {
    //                                         ReasonReactRouter.push("lessons")
    //                                     }}>
    //                                     {"Lessons"|>React.string}
    //                                 </a>
    //                             </li>
    //                         </>
    //                     <li>
    //                         <a
    //                             className={
    //                                 switch (List.nth_opt(url.path, 0)) {
    //                                 | Some(path) when path === "links" => css##active
    //                                 | _ => ""
    //                                 }
    //                             }
    //                             onClick={_ => {
    //                                 ReasonReactRouter.push("links")
    //                             }}>
    //                             {"Links"|>React.string}
    //                         </a>
    //                     </li>
    //                 </ul>
    //             </nav>
    //     </div>
    // </header>
};
