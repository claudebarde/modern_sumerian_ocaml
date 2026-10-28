[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default";

type topic = {
    key: string,
    title: string,
    description: string,
    // Topics that are not ready yet show "Coming soon" instead of a button
    available: bool,
    featured: bool,
};

let topics = [|
    {
        key: "grammar_notes",
        title: "Grammar Notes",
        description: "Read Grammar Notes to understand Sumerian grammar, with detailed explanations and examples.",
        available: true,
        featured: true,
    },
    {
        key: "daily_vocabulary",
        title: "Daily Vocabulary",
        description: "Build your Modern Sumerian skills with a 10-day challenge to learn 100 words.",
        available: true,
        featured: false,
    },
    {
        key: "comics",
        title: "Comics",
        description: "Read Comics to enjoy Modern Sumerian stories. They provide engaging narratives to enhance your learning.",
        available: true,
        featured: false,
    },
    {
        key: "lessons",
        title: "Lessons",
        description: "Follow guided Lessons to learn the language step by step.",
        available: false,
        featured: false,
    },
    {
        key: "flashcards",
        title: "Flashcards",
        description: "Use Flashcards to practise and review Sumerian vocabulary and reinforce your learning.",
        available: false,
        featured: false,
    },
|];

[@react.component]
let make = (~set_current_view) => {
    open Bindings;
    open Mui;

    <div className=css##hub>
        <header className=css##hubHero>
            <p className=css##hubLabel>{"Learn" |> React.string}</p>
            <h1>{"Learn Sumerian, one step at a time" |> React.string}</h1>
            <p className=css##hubIntro>
                {"Welcome to the Learning page! Pick a path below and start building your Modern Sumerian."
                |> React.string}
            </p>
        </header>
        <div className=css##topics>
            {topics
            |> Array.mapi((index, topic) =>
                <article
                    key={topic.key}
                    className={css##topic ++ (topic.featured ? " " ++ css##topicFeatured : "")}
                    style={ReactDOM.Style.make(
                        ~animationDelay=string_of_int(250 + index * 120) ++ "ms",
                        (),
                    )}
                >
                    <h2>{topic.title |> React.string}</h2>
                    <p>{topic.description |> React.string}</p>
                    {topic.available
                        ? <div className=css##topicFooter>
                            <Button
                                className={css##pill ++ " " ++ css##pillPrimary}
                                variant=`contained
                                ariaLabel={"Open " ++ topic.title}
                                onClick={_ => set_current_view(topic.key)}
                            >
                                {"Open" |> React.string}
                            </Button>
                            {topic.featured
                                ? <span className=css##badge>{"Start here" |> React.string}</span>
                                : React.null}
                        </div>
                        : <span className=css##comingSoon>{"Coming soon" |> React.string}</span>}
                </article>
            )
            |> React.array}
        </div>
    </div>
}
