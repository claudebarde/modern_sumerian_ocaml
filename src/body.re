[@mel.module "./Body.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    open Components;

    let url = ReasonReactRouter.useUrl();
    
    <div className={css##body}>
        {
            switch (url.path) {
                | ["conjugator"] => <Conjugator_ui />
                | ["cuneiforms"] => <Cuneiforms />
                | ["links"] => <Links />
                | ["lessons"] => <Lessons />
                | [] | ["home"] => <Home />
                | _ => <Page_not_found/>
            }
        }
    </div>
}