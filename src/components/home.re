[@mel.module "../styles/Home.module.scss"] external css: Js.t({..}) = "default"; 
[@mel.module "../assets/gumroad-thumbnail.png"] external gumroad_thumbnail: string = "default";

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (openPromo, setOpenPromo) = React.useState(_ => true);

    <div className={css##home}>
        <section className={css##hero}>
            <p className={css##heroEyebrow}>{"The language of Sumer, alive again" |> React.string}</p>
            <h1 className={css##heroTitle}>{"Speak the first " |> React.string}
                <span className={css##noBreak}>{"written language." |> React.string}</span></h1>
            <p className={css##heroText}>
                {"Modern Sumerian brings the language of ancient Sumer back into everyday life, with approachable lessons, comics and interactive tools for learning it today."
                |> React.string}
            </p>
            <div className={css##heroActions}>
                <Button
                    className={css##pill ++ " " ++ css##pillPrimary}
                    variant=`contained
                    onClick={_ => ReasonReactRouter.push("/learn")}
                >
                    {"Start learning" |> React.string}
                </Button>
                <Button
                    className={css##pill ++ " " ++ css##pillSecondary}
                    variant=`outlined
                    onClick={_ => ReasonReactRouter.push("/learn/comics")}
                >
                    {"Read the comics" |> React.string}
                </Button>
            </div>
        </section>

        <Box className={css##section} component=RootComponent.htmlElement("section")>
            <div className={css##intro}>
                <h2>{"Discover the language of ancient Sumer" |> React.string}</h2>
                <p>
                    {"Sumerian was spoken in ancient Mesopotamia and preserved in cuneiform on thousands of clay tablets. As one of the earliest written languages known to us, it offers a remarkable view into the literature, beliefs, and daily life of the people of Sumer."
                    |> React.string}
                </p>
                <p>
                    {"Modern Sumerian makes this rich linguistic heritage easier to explore. The project brings together approachable explanations and interactive tools for studying Sumerian vocabulary, grammar, pronunciation, and writing."
                    |> React.string}
                </p>
                <p>
                    {"The goal of Modern Sumerian is to help revive the language as a living means of expression. By building on the surviving texts and introducing new vocabulary for the modern world, the project aims to make it possible to use Sumerian not only to understand the past, but also to communicate ideas about life today."
                    |> React.string}
                </p>
            </div>
        </Box>

        <Box className={css##section} component=RootComponent.htmlElement("section")>
            <div className={css##contentPanel}>
                <p className={css##label}>{"Tools" |> React.string}</p>
                <h2>{"Use our interactive tools" |> React.string}</h2>
                <div className={css##tools}>
                    <article className={css##card}>
                        <h3>{"The Conjugator" |> React.string}</h3>
                        <p>
                            {"Build Sumerian verb forms by choosing their grammatical features, then see how each element combines within the finished word. The conjugator makes complex verbal structures easier to examine and understand."
                            |> React.string}
                        </p>
                        <Button className={css##pill ++ " " ++ css##pillPrimary ++ " " ++ css##cardAction} href="/conjugator" variant=`contained>
                            {"Try it now" |> React.string}
                        </Button>
                    </article>
                    <article className={css##card}>
                        <h3>{"The Dictionary" |> React.string}</h3>
                        <p>
                            {"Search in Sumerian or English to discover words, translations, and their cuneiform forms. The dictionary offers a quick way to expand your vocabulary and compare different entries."
                            |> React.string}
                        </p>
                        <Button
                            className={css##pill ++ " " ++ css##pillPrimary ++ " " ++ css##cardAction}
                            variant=`contained
                            onClick={_ => ReasonReactRouter.push("/dictionary")}
                        >
                            {"Try it now" |> React.string}
                        </Button>
                    </article>
                    <article className={css##card}>
                        <h3>{"The Keyboard" |> React.string}</h3>
                        <p>
                            {"Turn transliterated words into cuneiform, browse the available signs, and assemble the text you want to write. Once it is ready, copy your inscription for use anywhere."
                            |> React.string}
                        </p>
                        <Button className={css##pill ++ " " ++ css##pillPrimary ++ " " ++ css##cardAction} href="/keyboard" variant=`contained>
                            {"Try it now" |> React.string}
                        </Button>
                    </article>
                    <article className={css##card}>
                        <h3>{"The Lessons" |> React.string}</h3>
                        <p>
                            {"Follow a guided path through Sumerian grammar, vocabulary, and writing. Each lesson will introduce new concepts gradually and provide opportunities to put them into practice."
                            |> React.string}
                        </p>
                        <span className={css##comingSoon}>{"Coming soon" |> React.string}</span>
                    </article>
                </div>
            </div>
        </Box>

        <Box className={css##section} component=RootComponent.htmlElement("section")>
            <div className={css##contentPanel}>
                <p className={css##label}>{"Community" |> React.string}</p>
                <h2>{"Explore the project" |> React.string}</h2>
                <div className={css##projectRow}>
                    <p className={css##projectText}>
                        {"Explore Modern Sumerian beyond this website: browse the project's source code on GitHub, follow the Facebook page for news and updates, or visit the Links page to discover more resources about the Sumerian language and ancient Mesopotamia."
                        |> React.string}
                    </p>
                    <div className={css##pillGroup}>
                        <Button
                            className={css##pill ++ " " ++ css##pillSecondary}
                            href="https://www.facebook.com/ModernSumerian"
                            target="_blank"
                            rel="noopener noreferrer"
                            variant=`outlined
                            startIcon={<TablerReact.IconBrandFacebook />}
                        >
                            {"Facebook" |> React.string}
                        </Button>
                        <Button
                            className={css##pill ++ " " ++ css##pillSecondary}
                            href="https://discord.gg/8WSw6nA5h"
                            target="_blank"
                            rel="noopener noreferrer"
                            variant=`outlined
                            startIcon={<TablerReact.IconBrandDiscord />}
                        >
                            {"Discord" |> React.string}
                        </Button>
                        <Button
                            className={css##pill ++ " " ++ css##pillSecondary}
                            variant=`outlined
                            startIcon={<TablerReact.IconBrandGithub />}
                            href="https://github.com/claudebarde/modern_sumerian_ocaml"
                            target="_blank"
                            rel="noopener noreferrer"
                        >
                            {"GitHub" |> React.string}
                        </Button>
                        <Button
                            className={css##pill ++ " " ++ css##pillSecondary}
                            variant=`outlined
                            startIcon={<TablerReact.IconLink />}
                            onClick={_ => ReasonReactRouter.push("/links")}
                        >
                            {"Links" |> React.string}
                        </Button>
                    </div>
                </div>
            </div>
        </Box>
        <Promo_card
            is_open=openPromo
            on_dismiss={() => setOpenPromo(_ => false)}
            title="Sumer: A Traveller's Phrasebook"
            description="201 phrases across 19 chapters, with readable Sumerian spellings and cuneiform. No previous knowledge required."
            image=gumroad_thumbnail
            image_alt="Cover of Sumer: A Traveller's Phrasebook"
            cta_label="Get the phrasebook"
            cta_href="https://modernsumerian.gumroad.com/l/sumer-travel-phrasebook"
        />
    </div>
}
