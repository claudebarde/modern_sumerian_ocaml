[@react.component]
let make = (~codePoint: string, ~pronunciation: string) => {
    let (hover, setHover) = React.useState(() => false)

    let styles = ReactDOM.Style.make(
        ~cursor="help",
        ~transform="scale(1.3)",
        (),
    )

    let element = 
        <span 
            className="cuneiforms" 
            onMouseEnter={_ => setHover(_ => true)} 
            onMouseLeave={_ => setHover(_ => false)} 
            style={hover ? styles : ReactDOM.Style.make()}
        >
            {React.string(codePoint)}
        </span>;

    React.cloneElement(element, {"data-tooltip": pronunciation})
}