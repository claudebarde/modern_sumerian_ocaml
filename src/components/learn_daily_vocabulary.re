[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    <div className=css##dailyVocabulary>
        <div style={ReactDOM.Style.make(~textAlign="center", ())}>
            <p>{"Daily Vocabulary"|> React.string}</p>
            <p>{"Coming soon"|> React.string}</p>
            <p className="cuneiforms">{[| "ul", "la", " ", "im", {js|ĝen|js} |]
                |> Web_utils.display_cuneiforms
                |> Array.map(((cuneiform, _)) => cuneiform)
                |> Js.Array.join(~sep="")
                |> React.string}
            </p>
        </div>
    </div>
}