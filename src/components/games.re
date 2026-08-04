[@mel.module "../styles/Games.module.scss"] external css: Js.t({..}) = "default"; 

type game_choice =
  | Memory
  | Other;

type card_data = {
  symbol: string,
  translation: string,
  word: string,
};

type game_card = {
  data: card_data,
  id: int,
  revealed: bool,
};

module MemoryGame = {
  [@react.component]
  let make = () => {
    open Bindings.Mui;

    let cards_data: array(card_data) = [|
        {symbol: {js|𒀀|js}, translation: "water", word: "a"},
        {symbol: {js|𒀭|js}, translation: "god", word: {js|diĝir|js}},
        {symbol: {js|𒂍|js}, translation: "house", word: "e"},
        {symbol: {js|𒆠|js}, translation: "earth", word: "ki"},
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

    let flip_card = clicked_id =>
        set_cards(previous_cards =>
            previous_cards
            |> Array.map((card: game_card) =>
                card.id === clicked_id
                    ? {...card, revealed: !card.revealed}
                    : card
            )
        );

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
            {"Match pairs of cards to learn new Sumerian words!" |> React.string}
        </Typography>
        <Grid
            container=true
            spacing=`Number(2)
            columns=`Number(12)
            className=css##cardsGrid
        >
            {
                cards
                |> Array.mapi((index, card) =>
                    <Grid key={index |> Js.Int.toString} size=`Number(3)>
                        <div
                            className=css##cardScene
                            onClick={_ => flip_card(card.id)}
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
                                                "text-align": "center",
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
    open Bindings.Mui;

    // let (game_choice, _set_game_choice) = React.useState(() => (None: option(game_choice)));
    let (game_choice, _set_game_choice) = React.useState(() => Some(Memory));

    <Grid 
        container=true 
        spacing=`Number(2) 
        className=css##gamesContainer
    >
        <Grid 
            size=`Number(2) 
            className=css##gamesMenuContainer
        >
            <div>{"Games Menu" |> React.string}</div>
        </Grid>
        <Grid 
            size=`Number(10) 
            className=css##gameplayContainer
        >
            {
                switch (game_choice) {
                | None => <div>{"Please select a game" |> React.string}</div>
                | Some(Memory) => <MemoryGame />
                | Some(Other) => <div>{"Other Game" |> React.string}</div>
                }
            }
        </Grid>
    </Grid>
}
