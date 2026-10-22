[@mel.module "../styles/Home.module.scss"] external css: Js.t({..}) = "default"; 
[@mel.module "../assets/gumroad-thumbnail.png"] external gumroad_thumbnail: string = "default";

[@mel.obj]
external make_transition_overrides: (
    ~direction: string,
    ~ref: Js.Nullable.t(React.ref(Dom.element)),
    unit,
) => Bindings.Mui.Slide.props = "";

[@mel.obj]
external make_empty_props: unit => Bindings.Mui.Slide.props = "";

external merge_props: (
    Bindings.Mui.Slide.props,
    Bindings.Mui.Slide.props,
    Bindings.Mui.Slide.props,
) => Bindings.Mui.Slide.props = "Object.assign";

[@mel.obj]
external make_dialog_slots: (
    ~transition: React.component(Bindings.Mui.Slide.props),
    unit,
) => Js.t({..}) = "";

let slide_transition: React.component(Bindings.Mui.Slide.props) =
    React.forwardRef((props, ref) =>
        React.createElement(
            Bindings.Mui.Slide.component,
            merge_props(
                make_empty_props(),
                props,
                make_transition_overrides(~direction="up", ~ref, ()),
            ),
        )
    );

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (openPromo, setOpenPromo) = React.useState(_ => true);

    <div className={css##home}>
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
                <h2>{"Use our interactive tools" |> React.string}</h2>
                <Grid 
                    container=true 
                    spacing=`Number(2)
                >
                    <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~md=6, ~xl=3, ()))>
                        <Card className={css##toolCard}>
                            <CardHeader title={"The Conjugator" |> React.string} />
                            <CardContent className={css##toolCardContent}>
                                <Typography variant=Typography.Variant.body1>
                                    {"Build Sumerian verb forms by choosing their grammatical features, then see how each element combines within the finished word. The conjugator makes complex verbal structures easier to examine and understand."
                                    |> React.string}
                                </Typography>
                            </CardContent>
                            <CardActions>
                                <Button className="button" href="/conjugator" variant=`contained>
                                    {"Try it now" |> React.string}
                                </Button>
                            </CardActions>
                        </Card>
                    </Grid>
                    <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~md=6, ~xl=3, ()))>
                        <Card className={css##toolCard}>
                            <CardHeader title={"The Dictionary" |> React.string} />
                            <CardContent className={css##toolCardContent}>
                                <Typography variant=Typography.Variant.body1>
                                    {"Search in Sumerian or English to discover words, translations, and their cuneiform forms. The dictionary offers a quick way to expand your vocabulary and compare different entries."
                                    |> React.string}
                                </Typography>
                            </CardContent>
                            <CardActions>
                                <Button 
                                    className="button" 
                                    variant=`contained
                                    onClick={_ => ReasonReactRouter.push("/dictionary")}
                                >
                                    {"Try it now" |> React.string}
                                </Button>
                            </CardActions>
                        </Card>
                    </Grid>
                    <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~md=6, ~xl=3, ()))>
                        <Card className={css##toolCard}>
                            <CardHeader title={"The Keyboard" |> React.string} />
                            <CardContent className={css##toolCardContent}>
                                <Typography variant=Typography.Variant.body1>
                                    {"Turn transliterated words into cuneiform, browse the available signs, and assemble the text you want to write. Once it is ready, copy your inscription for use anywhere."
                                    |> React.string}
                                </Typography>
                            </CardContent>
                            <CardActions>
                                <Button className="button" href="/keyboard" variant=`contained>
                                    {"Try it now" |> React.string}
                                </Button>
                            </CardActions>
                        </Card>
                    </Grid>
                    <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~md=6, ~xl=3, ()))>
                        <Card className={css##toolCard}>
                            <CardHeader title={"The Lessons" |> React.string} />
                            <CardContent className={css##toolCardContent}>
                                <Typography variant=Typography.Variant.body1>
                                    {"Follow a guided path through Sumerian grammar, vocabulary, and writing. Each lesson will introduce new concepts gradually and provide opportunities to put them into practice."
                                    |> React.string}
                                </Typography>
                            </CardContent>
                            <CardActions>
                                <Button className="button" href="/lessons" variant=`contained>
                                    {"Coming soon" |> React.string}
                                </Button>
                            </CardActions>
                        </Card>
                    </Grid>
                </Grid>
            </div>
        </Box>

        <Box className={css##section} component=RootComponent.htmlElement("section")>
            <div className={css##contentPanel}>
                <h2>{"Explore the project" |> React.string}</h2>
                <p>
                    {"Explore Modern Sumerian beyond this website: browse the project's source code on GitHub, follow the Facebook page for news and updates, or visit the Links page to discover more resources about the Sumerian language and ancient Mesopotamia."
                    |> React.string}
                </p>
                // LARGE SCREEN BUTTONS
                <Box className=css##buttonGroup>
                    <Button
                        href="https://www.facebook.com/ModernSumerian"
                        target="_blank"
                        rel="noopener noreferrer"
                        variant=`contained
                        startIcon={<TablerReact.IconBrandFacebook />}
                    >
                        {"Facebook" |> React.string}
                    </Button>
                    <Button
                        href="https://discord.gg/8WSw6nA5h"
                        target="_blank"
                        rel="noopener noreferrer"
                        variant=`contained
                        startIcon={<TablerReact.IconBrandDiscord />}
                    >
                        {"Discord" |> React.string}
                    </Button>
                    <Button 
                        variant=`contained
                        startIcon={<TablerReact.IconLink />}
                        onClick={_ => ReasonReactRouter.push("/links")}
                    >
                        {"Links" |> React.string}
                    </Button>
                    <Button 
                        href="https://github.com/claudebarde/modern_sumerian_ocaml" 
                        variant=`contained
                        target="_blank"
                        rel="noopener noreferrer"
                        startIcon={<TablerReact.IconBrandGithub />}
                    >
                        {"GitHub" |> React.string}
                    </Button>
                </Box>
                // SMALL SCREEN BUTTONS
                <Box className=css##buttonGroupMobile>
                    <Fab
                        color=Color.primary
                        href="https://www.facebook.com/ModernSumerian"
                        target="_blank"
                        rel="noopener noreferrer"
                        variant=`circular
                    >
                        <TablerReact.IconBrandFacebook />
                    </Fab>
                    <Fab
                        color=Color.primary
                        href="https://discord.gg/8WSw6nA5h"
                        target="_blank"
                        rel="noopener noreferrer"
                        variant=`circular
                    >
                        <TablerReact.IconBrandDiscord />
                    </Fab>
                    <Fab
                        color=Color.primary
                        variant=`circular
                        onClick={_ => ReasonReactRouter.push("/links")}
                    >
                        <TablerReact.IconLink />
                    </Fab>
                    <Fab
                        color=Color.primary
                        href="https://github.com/claudebarde/modern_sumerian_ocaml" 
                        variant=`circular
                        target="_blank"
                        rel="noopener noreferrer"
                    >
                        <TablerReact.IconBrandGithub />
                    </Fab>
                </Box>
            </div>
        </Box>
        <Dialog
            _open=openPromo
            onClose={(_, _) => setOpenPromo(_ => false)}
            slots={make_dialog_slots(~transition=slide_transition, ())}
            fullWidth=true
            maxWidth=MaxWidth.md
        >
            <DialogTitle className={css##promoTitle}>
                {"Take a journey through ancient Sumer" |> React.string}
            </DialogTitle>
            <DialogContent>
                <div className={css##promoColumns}>
                    <div className={css##promoImageColumn}>
                        <img
                            className={css##promoImage}
                            src=gumroad_thumbnail
                            alt="Cover of Sumer: A Traveller's Phrasebook"
                        />
                    </div>
                    <div className={css##promoDescription}>
                        <Typography variant=Typography.Variant.h5 component=RootComponent.htmlElement("h2")>
                            {"Sumer: A Traveller's Phrasebook" |> React.string}
                        </Typography>
                        <Typography variant=Typography.Variant.body1>
                            {"Explore the Sumerian language as if you were preparing for a trip to ancient Mesopotamia. Learn how to buy and sell, share a meal, find your way through the city, and navigate everyday life in Sumer."
                            |> React.string}
                        </Typography>
                        <Typography variant=Typography.Variant.body1>
                            {"The illustrated ebook brings together 201 phrases across 19 themed chapters, with English translations, readable Sumerian spellings, and cuneiform. It also includes a detailed language and sources companion."
                            |> React.string}
                        </Typography>
                        <Typography variant=Typography.Variant.body1>
                            {"No previous knowledge of Sumerian is required. Both volumes are included in PDF and EPUB formats."
                            |> React.string}
                        </Typography>
                    </div>
                </div>
            </DialogContent>
            <DialogActions className={css##promoActions}>
                <Button
                    onClick={_ => setOpenPromo(_ => false)}
                    variant=`outlined
                    size=`large
                >
                    {"Dismiss" |> React.string}
                </Button>
                <Button
                    href="https://modernsumerian.gumroad.com/l/sumer-travel-phrasebook"
                    target="_blank"
                    rel="noopener noreferrer"
                    variant=`contained
                    size=`large
                >
                    {"Get the phrasebook" |> React.string}
                </Button>
            </DialogActions>
        </Dialog>
    </div>
}
