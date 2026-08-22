[@mel.module "./Body.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    open Components;

    let url = ReasonReactRouter.useUrl();
    
    <div className={css##body}>
        {
            switch (url.path) {
                | ["conjugator"] => <Conjugator_ui />
                | ["keyboard"] => <Keyboard />
                | ["links"] => <Links />
                | ["learn"]
                | ["learn", "daily_vocabulary"]
                | ["learn", "flashcards"]
                | ["learn", "lessons"] => <Learn />
                | ["dictionary"] => <Dictionary />
                | ["games"] => <Games />
                | ["wordslist"] => <Words_list />
                | ["worldmap"] => <World_map />
                | [] | ["home"] => <Home />
                | _ => <Page_not_found/>
            }
        }
    </div>
}
