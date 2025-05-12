[@mel.module "./Header.module.scss"] external css: Js.t({..}) = "default"; 
[@mel.scope ("process", "env")] external node_env: string = "NODE_ENV";

[@react.component]
let make = () => {
    let url = ReasonReactRouter.useUrl();

    <header>
        <div>
            <h1>
                {[|"eme", "ĝir15", "u", "me", "e"|]
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
            </h1>
        </div>
        <div>
            <h1>{"Modern Sumerian"|>React.string}</h1>
        </div>
        <div>
            {node_env === "development" ? (               
                <nav className={css##navColumn} role="navigation">
                    <ul>
                        <li>
                            <a
                                className={
                                    switch (List.nth_opt(url.path, 0)) {
                                    | Some(_) => ""
                                    | None => css##active
                                    }
                                }
                                onClick={_ => {
                                    ReasonReactRouter.push("/")
                                }}>
                                {"Home"|>React.string}
                            </a>
                        </li>
                        <li>
                            <a
                                className={
                                    switch (List.nth_opt(url.path, 0)) {
                                    | Some(path) when path === "conjugator" => css##active
                                    | Some(path) when path === "cuneiforms" => css##active
                                    | Some(path) when path === "dictionary" => css##active
                                    | _ => ""
                                    }
                                }>
                                {"Tools"|>React.string}
                            </a>
                            <ul className={css##dropdown}>
                                <li>
                                    <a 
                                        onClick={_ => { ReasonReactRouter.push("conjugator") }}
                                    >
                                        {"Conjugator"|>React.string}
                                    </a>
                                </li>
                                <li>
                                    <a 
                                        onClick={_ => { ReasonReactRouter.push("cuneiforms") }}
                                    >
                                        {"Cuneiforms"|>React.string}
                                    </a>
                                </li>
                                <li>
                                    <a 
                                        onClick={_ => { ReasonReactRouter.push("dictionary") }}
                                    >
                                        {"Dictionary"|>React.string}
                                    </a>
                                </li>
                            </ul>
                        </li>
                        <li>
                            <a
                                className={
                                    switch (List.nth_opt(url.path, 0)) {
                                    | Some(path) when path === "lessons" => css##active
                                    | _ => ""
                                    }
                                }
                                onClick={_ => {
                                    ReasonReactRouter.push("lessons")
                                }}>
                                {"Lessons"|>React.string}
                            </a>
                        </li>
                        <li>
                            <a
                                className={
                                    switch (List.nth_opt(url.path, 0)) {
                                    | Some(path) when path === "links" => css##active
                                    | _ => ""
                                    }
                                }
                                onClick={_ => {
                                    ReasonReactRouter.push("links")
                                }}>
                                {"Links"|>React.string}
                            </a>
                        </li>
                    </ul>
                </nav>
            ) : React.null}
        </div>
    </header>
};