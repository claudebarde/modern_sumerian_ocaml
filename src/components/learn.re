[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

type view =
    | DailyVocabulary
    | Flashcards
    | Lessons
    | GrammarNotes
    | Comics
    | Neologisms;

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let url = ReasonReactRouter.useUrl();
    let current_view = switch url.path {
        | ["learn"] => None
        | ["learn", "daily_vocabulary"] => Some(DailyVocabulary)
        | ["learn", "flashcards"] => Some(Flashcards)
        | ["learn", "lessons"] => Some(Lessons)
        | ["learn", "comics"]
        | ["learn", "comics", _] => Some(Comics)
        | ["learn", "neologisms"] => Some(Neologisms)
        | ["learn", "grammar_notes"]
        | ["learn", "grammar_notes", _] => Some(GrammarNotes)
        | _ => None
    };

    // React.useEffect0(() => {
    //     Browser.Fetch.get("/grammar_notes/index.json")
    //     |> Js.Promise.then_(response => {
    //         if (Browser.Fetch.ok(response)) {
    //             response
    //             |> Browser.Fetch.json
    //             |> Js.Promise.then_(json => {
    //                 switch (Learn_grammar_notes.parse_grammar_notes_index(json)) {
    //                 | Ok(notes) => set_grammar_notes(_ => notes)
    //                 | Error(message) =>
    //                     Js.log2("Unable to parse the grammar notes index:", message)
    //                 };
    //                 Js.Promise.resolve();
    //             });
    //         } else {
    //             Js.log("Unable to load the grammar notes index");
    //             Js.Promise.resolve();
    //         };
    //     })
    //     |> Js.Promise.catch(error => {
    //         Js.log2("Unable to load the grammar notes index:", error);
    //         Js.Promise.resolve();
    //     })
    //     |> ignore;

    //     None;
    // });

    let select_view = key => {
        switch key {
            | "daily_vocabulary" => ReasonReactRouter.push("/learn/daily_vocabulary")
            | "flashcards" => ReasonReactRouter.push("/learn/flashcards")
            | "lessons" => ReasonReactRouter.push("/learn/lessons")
            | "grammar_notes" => {
                ReasonReactRouter.push("/learn/grammar_notes")
            }
            | "neologisms" => {
                ReasonReactRouter.push("/learn/neologisms")
            }
            | "comics" => {
                ReasonReactRouter.push("/learn/comics")
            }
            | _ => ReasonReactRouter.push("/learn")
        }
    };

    <Container 
        className={css##learnContainer}
        maxWidth=MaxWidth.disabled
        disableGutters=true
    >
        <Box sx={{"width": "100%"}}>
            {
                switch current_view {
                    | None => <Learn_welcome set_current_view=select_view />
                    | Some(view) =>
                        switch view {
                            | DailyVocabulary => <Learn_daily_vocabulary />
                            | Flashcards => <Learn_flashcards />
                            | Lessons => <Learn_lessons />
                            | GrammarNotes => <Learn_grammar_notes />
                            | Neologisms => <Learn_neologisms />
                            | Comics => <Learn_comics />
                        }
                }
            }
        </Box>
    </Container>
}
