[@mel.module "../styles/Games.module.scss"] external css: Js.t({..}) = "default"; 

type game_choice =
  | Memory
  | Wordle;

module Timer = {
  [@react.component]
  let make = (
    ~running: bool,
    ~onMaximumReached: unit => unit,
  ) => {
    let maximum_time = 59 * 60 + 59;
    let (time, set_time) = React.useState(() => 0);

    React.useEffect1(() => {
        if (running) {
            let interval_id =
                Js.Global.setInterval(
                    ~f=() =>
                        set_time(prev_time =>
                            prev_time >= maximum_time
                                ? prev_time
                                : prev_time + 1
                        ),
                    1000,
                );
            Some(() => Js.Global.clearInterval(interval_id));
        } else {
            None;
        };
    }, [|running|]);

    React.useEffect1(() => {
        if (time >= maximum_time) {
            set_time(_ => 0);
            onMaximumReached();
        };
        None;
    }, [|time|]);

    let two_digits = value =>
        value < 10
            ? "0" ++ Js.Int.toString(value)
            : Js.Int.toString(value);
    let formatted_time =
        two_digits(time / 60)
        ++ ":"
        ++ two_digits(time mod 60);

    <div>
        {formatted_time |> React.string}
    </div>
  }
};

type card_data = {
  symbol: string,
  translation: string,
  word: string,
};

type game_card = {
  data: card_data,
  id: int,
  matched: bool,
  revealed: bool,
};

module MemoryGame = {
  [@react.component]
  let make = () => {
    open Bindings;
    open Mui;

    let cards_data: array(card_data) = [|
        {symbol: {js|𒀀|js}, translation: "water", word: "a"},
        {symbol: {js|𒀭|js}, translation: "god", word: {js|diĝir|js}},
        {symbol: {js|𒂍|js}, translation: "house", word: "e"},
        {symbol: {js|𒆠|js}, translation: "place", word: "ki"},
        {symbol: {js|𒈗|js}, translation: "king", word: "lugal"},
        {symbol: {js|𒊩|js}, translation: "woman", word: "munus"},
        {symbol: {js|𒌓|js}, translation: "sun", word: "ud"},
        {symbol: {js|𒌉|js}, translation: "child", word: "dumu"},
    |];

    let create_cards = () => {
        let duplicated_cards =
            Array.init(
                Array.length(cards_data) * 2,
                index => {
                    data: Array.get(cards_data, index / 2),
                    id: index,
                    matched: false,
                    revealed: false,
                },
            );
        Array.shuffle(
            ~rand=bound => Js.Math.random_int(0, bound),
            duplicated_cards,
        );
        duplicated_cards;
    };

    let (cards, set_cards) = React.useState(() => create_cards());
    let (flipped_cards, set_flipped_cards) = React.useState(() => (None, None));
    let (timer_running, set_timer_running) = React.useState(() => false);
    let (timer_generation, set_timer_generation) = React.useState(() => 0);
    let celebration_launched = React.useRef(false);
    let game_generation = React.useRef(0);

    let reset_game = () => {
        game_generation.current = game_generation.current + 1;
        set_cards(_ => create_cards());
        set_flipped_cards(_ => (None, None));
        set_timer_running(_ => false);
        set_timer_generation(generation => generation + 1);
        celebration_launched.current = false;
    };

    React.useEffect1(() => {
        let game_completed =
            Array.length(cards) > 0
            && Array.for_all((card: game_card) => card.matched, cards);

        if (game_completed && !celebration_launched.current) {
            celebration_launched.current = true;
            set_timer_running(_ => false);
            Confetti.launch(
                Confetti.make_options(
                    ~particleCount=180,
                    ~spread=120,
                    ~startVelocity=45,
                    ~origin=Confetti.make_origin(~x=0.5, ~y=0.6, ()),
                    ~colors=[|
                        Config.colors##protonRed,
                        Config.colors##nycTaxi,
                        Config.colors##cerealFlake,
                        Config.colors##botanicalNight,
                    |],
                    ~disableForReducedMotion=true,
                    (),
                ),
            );
        } else if (!game_completed) {
            celebration_launched.current = false;
        };

        None;
    }, [|cards|]);

    let reveal_card = clicked_id =>
        set_cards(previous_cards =>
            previous_cards
            |> Array.map((card: game_card) =>
                card.id === clicked_id
                    ? {...card, revealed: true}
                    : card
            )
        );

    let hide_cards = (first_id, second_id) =>
        set_cards(previous_cards =>
            previous_cards
            |> Array.map((card: game_card) =>
                card.id === first_id || card.id === second_id
                    ? {...card, revealed: false}
                    : card
            )
        );

    let mark_cards_as_matched = (first_id, second_id) =>
        set_cards(previous_cards =>
            previous_cards
            |> Array.map((card: game_card) =>
                card.id === first_id || card.id === second_id
                    ? {...card, matched: true, revealed: true}
                    : card
            )
        );

    let flip_card = (clicked_card: game_card) =>
        // flips the card and checks for matches
        switch flipped_cards {
        | (None, None) when !clicked_card.revealed => {
            set_timer_running(_ => true);
            reveal_card(clicked_card.id);
            set_flipped_cards(_ => (Some(clicked_card), None));
        }
        | (Some(first_card), None)
            when !clicked_card.revealed && first_card.id !== clicked_card.id => {
            reveal_card(clicked_card.id);
            set_flipped_cards(_ => (Some(first_card), Some(clicked_card)));

            let cards_match =
                first_card.data.symbol === clicked_card.data.symbol;
            let current_generation = game_generation.current;
            let _timeout =
                Js.Global.setTimeout(
                    ~f=() => {
                        if (current_generation === game_generation.current) {
                            if (cards_match) {
                                mark_cards_as_matched(first_card.id, clicked_card.id);
                            } else {
                                hide_cards(first_card.id, clicked_card.id);
                            };
                            set_flipped_cards(_ => (None, None));
                        };
                    },
                    1000,
                );
            ();
        }
        // Ignore clicks while a pair is being compared, or on cards that
        // have already been revealed successfully.
        | _ => ()
        };


    <Container className=css##memoryGameContainer>
        <Typography 
            variant=Typography.Variant.h4 
            className=css##gameTitle
        >
            {"Memory Game" |> React.string}
        </Typography>
        <Typography 
            variant=Typography.Variant.h6 
            className=css##gameDescription
        >
            <span></span>
            <span>{"Match pairs of cards to learn new Sumerian words!" |> React.string}</span>
            <Timer
                key={timer_generation |> Js.Int.toString}
                running={timer_running}
                onMaximumReached={reset_game}
            />
            {
                celebration_launched.current == true
                ? <Button
                    onClick={_ => {
                        reset_game();
                    }}
                >
                        {"Play Again" |> React.string}
                    </Button>
                : React.null
            }
        </Typography>
        <Grid
            container=true
            spacing=`Number(2)
            columns=`Number(12)
            className=css##cardsGrid
        >
            {
                cards
                |> Array.map(card =>
                    <Grid key={card.id |> Js.Int.toString} size=`Object(Grid.ResponsiveSize.make(~xs=6, ~sm=3, ()))>
                        <div
                            className=css##cardScene
                            onClick={_ => flip_card(card)}
                        >
                            <div
                                className={
                                    css##cardInner
                                    ++ (card.revealed ? " " ++ css##revealed : "")
                                }
                            >
                                <Card
                                    className={
                                        css##memoryCard
                                        ++ " "
                                        ++ css##cardFace
                                        ++ " "
                                        ++ css##cardCover
                                    }
                                >
                                    <span className="cuneiforms small">{{js|𒅴𒄀|js} |> React.string}</span>
                                </Card>
                                <Card
                                    className={
                                        css##memoryCard
                                        ++ " "
                                        ++ css##cardFace
                                        ++ " "
                                        ++ css##cardValues
                                        ++ (card.matched ? " " ++ css##matched : "")
                                    }
                                >
                                    <CardHeader
                                        avatar={
                                            <strong className="cuneiforms">
                                                {card.data.symbol |> React.string}
                                            </strong>
                                        }
                                        title={
                                            <Typography variant=Typography.Variant.h6>
                                                {
                                                    card.data.word
                                                    |> Web_utils.Format.from_phonetic_to_standard
                                                    |> React.string
                                                }
                                            </Typography>
                                        }
                                        sx={{"padding": "10px"}}
                                    />
                                    <CardContent
                                        sx={{
                                            "display": "flex",
                                            "justifyContent": "space-between",
                                            "padding": "10px",
                                        }}
                                    >
                                        <Typography
                                            variant=Typography.Variant.body1
                                            sx={{
                                                "width": "100%",
                                                "textAlign": "center",
                                                "textTransform": "uppercase",
                                            }}
                                        >
                                            {card.data.translation |> React.string}
                                        </Typography>
                                    </CardContent>
                                </Card>
                            </div>
                        </div>
                    </Grid>
                )
                |> React.array
            }
        </Grid>
    </Container>
  }
};

module Wordle = {
  [@react.component]
  let make = () => {
    open Bindings;
    open Mui;

    let words_to_guess = [|"dumu", "lugal", "urim", "anshe", "mushen", "guza", "dungu"|];
    let (wordle_word, set_wordle_word) = React.useState(() => None);
    let (current_row, set_current_row) = React.useState(() => 0);
    let (current_guess, set_current_guess) = React.useState(() => None);
    let (previous_guesses, set_previous_guesses) = React.useState(() => [||]);
    let (success, set_success) = React.useState(() => false);
    let celebration_launched = React.useRef(false);

    let launch_confetti = () =>
        Confetti.launch(
            Confetti.make_options(
                ~particleCount=180,
                ~spread=120,
                ~startVelocity=45,
                ~origin=Confetti.make_origin(~x=0.5, ~y=0.6, ()),
                ~colors=[|
                    Config.colors##protonRed,
                    Config.colors##nycTaxi,
                    Config.colors##cerealFlake,
                    Config.colors##botanicalNight,
                |],
                ~disableForReducedMotion=true,
                (),
            ),
        );

    let reset = () => {
        set_current_row(_ => 0);
        set_current_guess(_ => None);
        set_previous_guesses(_ => [||]);
        set_success(_ => false);
        celebration_launched.current = false;
        set_wordle_word(_ => None);
        // selects a new word to guess
        let random_index = Js.Math.random_int(0, Array.length(words_to_guess) - 1);
        let word = Array.get(words_to_guess, random_index);
        Js.log("Wordle word selected: " ++ word);
        set_wordle_word(_ => Some(word));
    }

    React.useEffect0(() => {
        // selects the word to guess randomly from the list of words
        let random_index = Js.Math.random_int(0, Array.length(words_to_guess) - 1);
        let word = Array.get(words_to_guess, random_index);
        Js.log("Wordle word selected: " ++ word);
        set_wordle_word(_ => Some(word));
        None;
    });

    React.useEffect1(() => {
        // listens to keyboard events for user input
        let handle_keydown = event => {
            let key = Browser.Window.key(event);

            if (Js.String.length(key) === 1) {
                set_current_guess(current_guess => {
                    switch current_guess {
                    | None => Some(key)
                    | Some(guess) => Some(guess ++ key)
                    }
                });
            } else if (key === "Enter") {
                switch (current_guess, wordle_word) {
                | (Some(guess), Some(word)) => {
                    // updates the UI
                    // and checks if the user guessed the word correctly
                    set_previous_guesses(previous_guesses =>
                        Array.append(previous_guesses, [|guess|])
                    );
                    set_current_row(current_row =>
                        current_row < 6
                            ? current_row + 1
                            : current_row
                    );
                    set_current_guess(_ => None);
                    
                    if (
                        guess === word
                        && !celebration_launched.current
                    ) {
                        celebration_launched.current = true;
                        set_success(_ => true);
                        launch_confetti();
                    }
                }
                | _ => ()
                };
            } else if(key === "Backspace") {
                set_current_guess(current_guess => {
                    switch current_guess {
                    | Some(guess) =>
                        if (Js.String.length(guess) > 0) {
                            Some(Js.String.slice(~start=0, ~end_=Js.String.length(guess) - 1, guess))
                        } else {
                            None
                        }
                    | None => None
                    }
                });
            };
        };

        Browser.Window.add_keydown_listener("keydown", handle_keydown);

        Some(() =>
            Browser.Window.remove_keydown_listener("keydown", handle_keydown)
        );
    }, [|current_guess|]);

    <Container className=css##memoryGameContainer>
        <Typography 
            variant=Typography.Variant.h4 
            className=css##gameTitle
        >
            {"Wordle" |> React.string}
        </Typography>
        <Typography 
            variant=Typography.Variant.h6 
            className=css##gameDescription
        >
            <span></span>
            <span>{"Guess the Sumerian word based on the given clues!" |> React.string}</span>
            <span></span>
        </Typography>
        <div 
            className=css##wordleGrid
        >
            {
                switch wordle_word {
                | None => <div>{"Loading..." |> React.string}</div>
                | Some(word) => {
                    let letters = word |> Js.String.split(~sep="");

                    Array.init(6, row_index => {
                        <div
                            key={"row-" ++ Js.Int.toString(row_index)}
                            className=css##wordleRow
                        >
                            {
                                // The number of blocks in each row is equal to
                                // the length of the word.
                                letters
                                |> Array.mapi((letter_index, letter) =>
                                    <Paper
                                        key={
                                            Js.Int.toString(row_index)
                                            ++ "-"
                                            ++ Js.Int.toString(letter_index)
                                        }
                                        className={
                                            css##wordleBlock
                                            ++ " "
                                            ++ (
                                                if (row_index < current_row) {
                                                    let previous_guess = Array.get(previous_guesses, row_index);
                                                    let guessed_letter = Js.String.get(previous_guess, letter_index);
                                                    if (guessed_letter === letter) {
                                                        css##success
                                                    } else if (Js.String.includes(~search=guessed_letter, word)) {
                                                        css##partial
                                                    } else {
                                                        css##incorrect
                                                    }
                                                } else {
                                                    ""
                                                }
                                            )
                                        }
                                        ariaLabel={letter}
                                    >
                                        {
                                            if (current_row === row_index) {
                                                switch current_guess {
                                                | None => React.null
                                                | Some(guess) =>
                                                    if (Js.String.length(guess) > letter_index) {
                                                        Js.String.get(
                                                            guess,
                                                            letter_index,
                                                        )
                                                        |> React.string
                                                    } else {
                                                        React.null
                                                    }
                                                }
                                            } else if (row_index < current_row) {
                                                // Display the letter from the previous guesses.
                                                let previous_guess = Array.get(previous_guesses, row_index);
                                                Js.String.get(previous_guess, letter_index)
                                                |> React.string
                                            } else {
                                                React.null
                                            }
                                        }
                                    </Paper>
                                )
                                |> React.array
                            }
                        </div>
                    })
                    |> React.array
                }}
            }
        </div>
        {
            if (success) {
                <Typography 
                    variant=Typography.Variant.h6 
                    className=css##successMessage
                    sx={{"display": "flex", "flexDirection": "column", "alignItems": "center"}}
                >
                    {"Congratulations! You've guessed the word!" |> React.string}
                    <Button 
                        variant=`contained
                        onClick={_ => reset()}
                    >
                        {"Play Again" |> React.string}
                    </Button>
                </Typography>
            } else {
                <Typography 
                    variant=Typography.Variant.body2
                    sx={{"marginTop": "10px", "display": "flex", "flexDirection": "column"}}
                >
                    <span>{"Dark green = Incorrect letter" |> React.string}</span>
                    <span>{"Yellow = Correct letter in the wrong position" |> React.string}</span>
                    <span>{"Dark red = Correct letter in the correct position" |> React.string}</span>
                </Typography>
            }
        }
    </Container>
  }
};

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    // let (game_choice, _set_game_choice) = React.useState(() => (None: option(game_choice)));
    let (game_choice, set_game_choice) = React.useState(() => Some(Wordle));
    let (memory_game_open, set_memory_game_open) = React.useState(() => false);
    let (memory_game_generation, set_memory_game_generation) =
        React.useState(() => 0);

    <Grid 
        container=true 
        spacing=`Number(2)
        className=css##gamesContainer
    >
        <Grid 
            size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=3, ()))
            className=css##gamesMenuContainer
        >
            <List
                subheader={
                    <ListSubheader component=RootComponent.htmlElement("div")>
                        {"Select a game" |> React.string}
                    </ListSubheader>
                }
                component=RootComponent.htmlElement("nav")
                sx={{"backgroundColor": "white", "borderRadius": "8px"}}
            >
                <ListItemButton
                    selected={game_choice === Some(Memory)}
                    onClick={_ => {
                        set_memory_game_open(prev => !prev);                        
                    }}
                >
                    <ListItemIcon>
                        <TablerReact.IconBrain />
                    </ListItemIcon>
                    <ListItemText primary={"Memory" |> React.string} />
                    {memory_game_open ? <TablerReact.IconChevronUp /> : <TablerReact.IconChevronDown />}
                </ListItemButton>
                <Collapse in_={memory_game_open} timeout=`auto sx={{"backgroundColor": "white"}}>
                    {
                        let paddingLeft = "32px";

                        <List component=RootComponent.htmlElement("div")>
                            <ListItemButton
                                sx={{"paddingLeft": paddingLeft}}
                                onClick={_ => {
                                    set_game_choice(_ => Some(Memory));
                                    set_memory_game_generation(generation =>
                                        generation + 1
                                    )
                                }}
                            >
                                <ListItemIcon>
                                    <TablerReact.IconCircleNumber1 />
                                </ListItemIcon>
                                <ListItemText primary={"Easy" |> React.string} />
                            </ListItemButton>
                            // <ListItemButton sx={{"paddingLeft": paddingLeft}}>
                            //     <ListItemIcon>
                            //         <TablerReact.IconCircleNumber2 />
                            //     </ListItemIcon>
                            //     <ListItemText primary={"Medium" |> React.string} />
                            // </ListItemButton>
                            // <ListItemButton sx={{"paddingLeft": paddingLeft}}>
                            //     <ListItemIcon>
                            //         <TablerReact.IconCircleNumber3 />
                            //     </ListItemIcon>
                            //     <ListItemText primary={"Hard" |> React.string} />
                            // </ListItemButton>
                            <Divider />
                            <ListItemButton
                                sx={{"paddingLeft": paddingLeft}}
                                onClick={_ =>
                                    set_memory_game_generation(generation =>
                                        generation + 1
                                    )
                                }
                            >
                                <ListItemIcon>
                                    <TablerReact.IconArrowBackUpDouble />
                                </ListItemIcon>
                                <ListItemText primary={"Reset" |> React.string} />
                            </ListItemButton>
                        </List>
                    }
                </Collapse>
                <ListItemButton
                    selected={game_choice === Some(Wordle)}
                    onClick={_ => {
                        set_game_choice(_ => Some(Wordle));
                        set_memory_game_open(_ => false);
                    }}
                >
                    <ListItemIcon>
                        <TablerReact.IconBorderAll />
                    </ListItemIcon>
                    <ListItemText primary={"Wordle" |> React.string} />
                </ListItemButton>
            </List>
        </Grid>
        <Grid 
            size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=9, ()))
            className=css##gameplayContainer
        >
            {
                switch (game_choice) {
                | None => <div>{"Please select a game" |> React.string}</div>
                | Some(Memory) =>
                    <MemoryGame
                        key={memory_game_generation |> Js.Int.toString}
                    />
                | Some(Wordle) => <Wordle />
                }
            }
        </Grid>
    </Grid>
}
