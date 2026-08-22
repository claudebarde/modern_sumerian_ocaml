[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

type category = Words | Cuneiform;

[@react.component]
let make = (~entries, ~category) => {
    open Bindings;
    open Mui;
    
    let shuffle_entries = entries => {
        let shuffled_entries = Array.copy(entries);
        Array.shuffle(
            ~rand=bound => Js.Math.random_int(0, bound),
            shuffled_entries,
        );
        shuffled_entries;
    };

    let (test_data, _set_test_data) = React.useState(_ => shuffle_entries(entries));
    let (turn, set_turn) = React.useState(_ => 0);
    let (answer, set_answer) = React.useState(_ => "");
    let (right_answers, set_right_answers) = React.useState(_ => [||]);
    let (wrong_answers, set_wrong_answers) = React.useState(_ => [||]);
    let (end_game, set_end_game) = React.useState(_ => false);

    let check_answer = (correct_answer: string) => {
        let entry = Array.get(test_data, turn);
        let formatted_answer = answer |> String.trim;

        if (formatted_answer |> String.length === 0) {
            ();
        } else if (formatted_answer === correct_answer) {
            set_right_answers(answers => Array.append(answers, [|entry|]));
            set_turn(turn => turn + 1);
            set_answer(_ => "");
            if (turn + 1 >= Array.length(test_data)) {
                set_end_game(_ => true);
            };
        } else {
            set_wrong_answers(answers => Array.append(answers, [|entry|]));
            set_turn(turn => turn + 1);
            set_answer(_ => "");
            if (turn + 1 >= Array.length(test_data)) {
                set_end_game(_ => true);
            };
        };
    };

    <Typography 
        variant=Typography.Variant.subtitle1
    >
        {
            if (end_game === false) {
                let (english, sumerian, cuneiform) = Array.get(test_data, turn);

                <>
                    <Typography gutterBottom=true>
                    {
                        switch category {
                        | Words => {
                            {"Translate the word below in English" |> React.string}
                        }
                        | Cuneiform => {
                            {"Translate the cuneiform below in English" |> React.string}
                        }
                        }
                    }
                    </Typography>
                    <Stack 
                        direction=`row 
                        useFlexGap=true
                        spacing=`Number(2)
                        sx={{"alignItems": "center", "margin": "5px"}}
                    > 
                        <CircularProgress 
                            enableTrackSlot=true
                            max=100.
                            min=0.
                            size=`Number(40)
                            thickness=4.
                            value={(turn * 10) |> float_of_int}
                            variant=`determinate
                        />
                        <Chip 
                            icon={<TablerReact.IconMoodHappy />} 
                            label={(right_answers |> Array.length |> string_of_int) ++ " correct"} 
                            color={turn === 0 ? Color.transparent : Color.success}
                        />
                        <Chip 
                            icon={<TablerReact.IconMoodSadDizzy />} 
                            label={(wrong_answers |> Array.length |> string_of_int) ++ " wrong"} 
                            color={turn === 0 ? Color.transparent : Color.error}
                        />
                    </Stack>
                    <Card>
                        <CardContent sx={{"textAlign": "center"}}>
                            <Typography 
                                variant=Typography.Variant.h6
                                component=RootComponent.htmlElement("div")
                                gutterBottom=true
                            >
                                <span className="cuneiforms small">
                                    {cuneiform |> React.string}
                                </span>
                                {
                                    switch category {
                                        | Words => {
                                            <span className="cuneiforms small">
                                                {sumerian |> React.string}
                                            </span>
                                        }
                                        | Cuneiform => React.null
                                    }
                                }
                            </Typography>
                            <FormControl
                                fullWidth=true
                                size=`small
                                variant=`outlined
                                sx={{"marginTop": "16px"}}
                            >
                                <InputLabel
                                    htmlFor="daily-vocabulary-answer"
                                    sx={{
                                        "color": Config.colors##darkRift,
                                        "&.Mui-focused": {
                                            "color": Config.colors##protonRed,
                                        },
                                    }}
                                >
                                    {"Your answer" |> React.string}
                                </InputLabel>
                                <OutlinedInput
                                    id="daily-vocabulary-answer"
                                    label={"Your answer" |> React.string}
                                    value=answer
                                    sx={{"color": Config.colors##darkRift}}
                                    endAdornment={
                                        <InputAdornment position=`end_>
                                            <Button 
                                                ariaLabel="Check answer" 
                                                variant=`contained
                                                size=`small
                                                disabled={answer |> String.trim |> String.length === 0}
                                                onClick={_ => check_answer(english)}
                                            >
                                                {"Check" |> React.string}
                                            </Button>
                                        </InputAdornment>
                                    }
                                    onChange={event =>
                                        set_answer(_ => React.Event.Form.target(event)##value)
                                    }
                                    onKeyDown={event => {
                                        if (React.Event.Keyboard.key(event) === "Enter") {
                                            React.Event.Keyboard.preventDefault(event);
                                            check_answer(english);
                                        };
                                    }}
                                />
                            </FormControl>
                        </CardContent>
                    </Card>
                </>                
            } else {
                <>
                    <Typography gutterBottom=true sx={{"textAlign": "center"}}>
                        {(Array.length(right_answers) > Array.length(wrong_answers) ? "Congratulations!" : "Try again") |> React.string}
                    </Typography>
                    <Typography gutterBottom=true sx={{"textAlign": "center"}}>
                        {"You got " ++ (Array.length(right_answers) |> string_of_int) ++ " right and " ++ (Array.length(wrong_answers) |> string_of_int) ++ " wrong." |> React.string}
                    </Typography>
                    {
                        if (Array.length(wrong_answers) > 0) {
                            <>
                                <Typography gutterBottom=true sx={{"textAlign": "center"}}>
                                    {"Review your wrong answers" |> React.string}
                                </Typography>
                                <Grid container=true spacing=`Number(2)>
                                {
                                    wrong_answers
                                    |> Array.mapi((index, (english, sumerian, cuneiform)) => {
                                        <Grid
                                            key={english ++ Int.to_string(index)}
                                            size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=6, ()))
                                        >
                                            <Card sx={{"height": "100%"}}>
                                                <CardContent sx={{
                                                    "display": "flex",
                                                    "justifyContent": "center",
                                                    "alignItems": "center",
                                                    "gap": "1rem",
                                                    "padding": "12px 8px",
                                                    "&:last-child": {
                                                        "paddingBottom": "12px",
                                                    },
                                                }}>
                                                    <Typography component=RootComponent.htmlElement("span")>
                                                        {english |> React.string}
                                                    </Typography>
                                                    <Typography component=RootComponent.htmlElement("span")>
                                                        {" => "|> React.string}
                                                    </Typography>
                                                    <Typography component=RootComponent.htmlElement("span")>
                                                        {sumerian |> React.string}
                                                    </Typography>
                                                    <Typography 
                                                        component=RootComponent.htmlElement("span")
                                                        className="cuneiforms small"
                                                    >
                                                        {cuneiform |> React.string}
                                                    </Typography>
                                                </CardContent>
                                            </Card>
                                        </Grid>
                                    })
                                    |> React.array
                                }
                                </Grid>
                            </>
                        } else {
                            React.null
                        }
                    }
                </>
            }            
        }
    </Typography>
}
