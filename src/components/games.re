[@mel.module "../styles/Games.module.scss"] external css: Js.t({..}) = "default"; 

type game_choice =
  | Memory
  | Other;

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

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    // let (game_choice, _set_game_choice) = React.useState(() => (None: option(game_choice)));
    let (game_choice, _set_game_choice) = React.useState(() => Some(Memory));
    let (memory_game_open, set_memory_game_open) = React.useState(() => true);
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
                    onClick={_ => set_memory_game_open(prev => !prev)}
                >
                    <ListItemText primary={"Memory Game" |> React.string} />
                    {memory_game_open ? <TablerReact.IconChevronUp /> : <TablerReact.IconChevronDown />}
                </ListItemButton>
                <Collapse in_={memory_game_open} timeout=`auto sx={{"backgroundColor": "white"}}>
                    {
                        let paddingLeft = "32px";

                        <List component=RootComponent.htmlElement("div")>
                            <ListItemButton
                                sx={{"paddingLeft": paddingLeft}}
                                onClick={_ =>
                                    set_memory_game_generation(generation =>
                                        generation + 1
                                    )
                                }
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
                                <ListItemText primary={"Reset" |> React.string} />
                            </ListItemButton>
                        </List>
                    }
                </Collapse>
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
                | Some(Other) => <div>{"Other Game" |> React.string}</div>
                }
            }
        </Grid>
    </Grid>
}
