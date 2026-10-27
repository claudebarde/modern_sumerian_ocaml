[@mel.module "../styles/Reveal.module.scss"] external css: Js.t({..}) = "default";

// Fades and lifts its children into view the first time they scroll into the
// viewport. `delay` (in ms) staggers siblings that appear together.
[@react.component]
let make = (~children: React.element, ~delay: int=0, ~className: string="") => {
    open Bindings;

    let element_ref: React.ref(Js.nullable(Dom.element)) =
        React.useRef(Js.Nullable.null);
    let (visible, set_visible) = React.useState(_ => false);

    React.useEffect0(() => {
        switch (Js.Nullable.toOption(element_ref.current)) {
        | Some(element) =>
            let observer =
                Browser.IntersectionObserver.make(
                    (entries, observer) =>
                        if (Js.Array.some(~f=Browser.IntersectionObserver.is_intersecting, entries)) {
                            set_visible(_ => true);
                            Browser.IntersectionObserver.disconnect(observer);
                        },
                    Browser.IntersectionObserver.make_options(~threshold=0.15, ()),
                );
            Browser.IntersectionObserver.observe(observer, element);
            Some(() => Browser.IntersectionObserver.disconnect(observer));
        | None => None
        }
    });

    <div
        ref={ReactDOM.Ref.domRef(element_ref)}
        className={css##reveal ++ (visible ? " " ++ css##visible : "") ++ " " ++ className}
        style={ReactDOM.Style.make(~transitionDelay=string_of_int(delay) ++ "ms", ())}
    >
        children
    </div>
}
