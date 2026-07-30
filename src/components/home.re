[@mel.module "../styles/Home.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

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
                    spacing={`Number(2)} 
                >
                    <Grid size={`Object({"xs": 12, "md": 6, "xl": 3})}>
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
                    <Grid size={`Object({"xs": 12, "md": 6, "xl": 3})}>
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
                    <Grid size={`Object({"xs": 12, "md": 6, "xl": 3})}>
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
                    <Grid size={`Object({"xs": 12, "md": 6, "xl": 3})}>
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
    </div>
}
