[@mel.module "../styles/Modal.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = (~children: React.element, ~is_open: bool, ~close: unit => unit) => {
    let (is_closing, set_is_closing) = React.useState(_ => false);

    if (is_open) {
        <div className={css##background ++ {is_closing ? " " ++ css##out : "" }}>
            <div className={css##modal ++ {is_closing ? " " ++ css##out : "" }}>
                <button className=css##close onClick={_ => {
                    set_is_closing(_ => true);
                    let _ = Js.Global.setTimeout(~f=() => {
                        set_is_closing(_ => false);
                        close();
                    }, 400);
                }}>
                    {"X" |> React.string}
                </button>
                children
            </div>
        </div>
    } else {
        React.null
    }
}