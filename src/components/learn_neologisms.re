[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (neologisms, set_neologisms) = React.useState(() => None);
    let (_index_error, set_index_error) = React.useState(() => None);

    React.useEffect0(() => {
        Browser.Fetch.get("/neologisms/modern-sumerian-glossary.md")
        |> Js.Promise.then_(response => {
            if (Browser.Fetch.ok(response)) {
                response
                |> Browser.Fetch.text
                |> Js.Promise.then_(text => {
                    set_neologisms(_ => Some(text));
                    Js.Promise.resolve();
                });
            } else {
                set_index_error(_ => Some("Unable to load the neologisms file"));
                Js.Promise.resolve();
            };
        })
        |> Js.Promise.catch(_error => {
            set_index_error(_ => Some("Unable to load the neologisms file"));
            Js.Promise.resolve();
        })
        |> ignore;

        None;
    });

    <Container className=css##neologismsContainer>
        {
            switch (neologisms) {
            | None => React.null
            | Some(text) => 
                <ReactMarkdown
                    markdown=text
                    remarkPlugins=[|ReactMarkdown.remarkGfmWithoutSingleTilde|]
                    rehypePlugins=[|ReactMarkdown.rehypeCuneiform|]
                />
            }
        }
    </Container>
}