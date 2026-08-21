[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

type test_yourself_category = Words | Cuneiform;

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (current_day, set_current_day) = React.useState(() => None);
    let (is_test_yourself_open, set_test_yourself_open) = React.useState(() => false);
    let (test_yourself_category, set_test_yourself_category) = React.useState(() => None);

    let words_list: array(array((string, string, string))) = [|
        /* Day 1 — Everyday essentials */
        [|
            ("water", "a", {js|𒀀|js}),
            ("food", {js|niĝgu|js}, {js|𒃻𒅥|js}),
            ("bread", "ninda", {js|𒃻|js}),
            ("house", "e", {js|𒂍|js}),
            ("to eat", "gu", {js|𒅥|js}),
            ("to drink", {js|naĝ|js}, {js|𒅘|js}),
            ("to sleep", "uku", {js|𒌑𒆪|js}),
            ("good", "dug", {js|𒄭|js}),
            ("bad", "hul", {js|𒅆𒌨|js}),
            ("now", "inesh", {js|𒉌𒉈𒌍|js}),
        |],

        /* Day 2 — People and communication */
        [|
            ("person", "lu", {js|𒇽|js}),
            ("woman", "munus", {js|𒊩|js}),
            ("man", "nita", {js|𒍑|js}),
            ("child", "dumu", {js|𒌉|js}),
            ("to speak", "dug", {js|𒅗|js}),
            ("to listen", {js|ĝeshtug ĝar|js}, {js|𒉿𒃻|js}),
            ("to love", {js|ki aĝ|js}, {js|𒆠𒉘|js}),
            ("young", "tur", {js|𒌉|js}),
            ("old", "sumun", {js|𒁁|js}),
            ("together", "teshbi", {js|𒀸𒁉|js}),
        |],

        /* Day 3 — Movement and orientation */
        [|
            ("hand", "shu", {js|𒋗|js}),
            ("foot", {js|ĝiri|js}, {js|𒄊|js}),
            ("door", "ig", {js|𒅅|js}),
            ("road", "kaskal", {js|𒆜|js}),
            ("to go", {js|ĝen|js}, {js|𒁺|js}),
            ("to enter", "kur", {js|𒆭|js}),
            ("to stand", "gub", {js|𒁺|js}),
            ("long", "gid", {js|𒁍|js}),
            ("short", "lugud", {js|𒆸|js}),
            ("here", "kiba", {js|𒆠𒁀|js}),
        |],

        /* Day 4 — Home and possessions */
        [|
            ("room", "enun", {js|𒂍𒉣|js}),
            ("chair", "guza", {js|𒄑𒄖𒍝|js}),
            ("bed", {js|ĝeshnud|js}, {js|𒄑𒈿|js}),
            ("clothing", "tug", {js|𒌆|js}),
            ("to sit", "tush", {js|𒆪|js}),
            ("to open", "duh", {js|𒂃|js}),
            ("to have", "tuku", {js|𒌇|js}),
            ("clean", "sikil", {js|𒂖|js}),
            ("new", "gibil", {js|𒉋|js}),
            ("inside", "shaga", {js|𒊮𒂵|js}),
        |],

        /* Day 5 — Food and cooking */
        [|
            ("beer", "kash", {js|𒃼|js}),
            ("milk", "ga", {js|𒂵|js}),
            ("meat", "uzu", {js|𒍜|js}),
            ("fish", "ku", {js|𒄩|js}),
            ("to cook", {js|sheĝ|js}, {js|𒉈|js}),
            ("to cut", "kud", {js|𒋻|js}),
            ("to give", "shum", {js|𒋧|js}),
            ("hot", "kum", {js|𒉈|js}),
            ("cold", "sed", {js|𒆗|js}),
            ("well", "dugesh", {js|𒄭𒄀𒌍|js}),
        |],

        /* Day 6 — Nature and weather */
        [|
            ("sun", "ud", {js|𒌓|js}),
            ("sky", "an", {js|𒀭|js}),
            ("rain", {js|sheĝ|js}, {js|𒀀𒀭|js}),
            ("tree", {js|ĝesh|js}, {js|𒄑|js}),
            ("to rise", "zig", {js|𒍣|js}),
            ("to fall", "shub", {js|𒊒|js}),
            ("to grow", "mu", {js|𒈬|js}),
            ("bright", "zalag", {js|𒌓|js}),
            ("dark", "kukkug", {js|𒈪𒈪|js}),
            ("outside", "bara", {js|𒁇𒊏|js}),
        |],

        /* Day 7 — Animals and the countryside */
        [|
            ("dog", "ur", {js|𒌨|js}),
            ("bird", "mushen", {js|𒄷|js}),
            ("sheep", "udu", {js|𒇻|js}),
            ("cow", "ab", {js|𒀖|js}),
            ("to fly", "dal", {js|𒊑|js}),
            ("to run", "kash", {js|𒁽|js}),
            ("to see", "igi duh", {js|𒅆𒂃|js}),
            ("big", "gal", {js|𒃲|js}),
            ("small", "tur", {js|𒌉|js}),
            ("quickly", "ullabe", {js|𒄉𒆷𒁉|js}),
        |],

        /* Day 8 — Work and learning */
        [|
            ("work", {js|kiĝ|js}, {js|𒆥|js}),
            ("tablet", "dub", {js|𒁾|js}),
            ("word", "inim", {js|𒅗|js}),
            ("name", "mu", {js|𒈬|js}),
            ("to do", "ak", {js|𒀝|js}),
            ("to write", "sar", {js|𒊬|js}),
            ("to know", "zu", {js|𒍪|js}),
            ("clear", "sig", {js|𒋛|js}),
            ("difficult", "gilim", {js|𒄃|js}),
            ("again", "hur", {js|𒄯|js}),
        |],

        /* Day 9 — Feelings and social life */
        [|
            ("friend", "dusa", {js|𒄭𒊓|js}),
            ("heart", "shag", {js|𒊮|js}),
            ("joy", "kirizal", {js|𒅗𒉌|js}),
            ("fear", "ni", {js|𒉎|js}),
            ("to laugh", "zubir", {js|𒅗𒉈|js}),
            ("to cry", "ershesh", {js|𒀀𒅆𒋁|js}),
            ("to help", {js|aĝal|js}, {js|𒀉𒅅|js}),
            ("happy", "shaghul", {js|𒊮𒄾|js}),
            ("sad, afflicted", "shagsag", {js|𒊮𒉺|js}),
            ("always", "udmeda", {js|𒌓𒈨𒁕|js}),
        |],

        /* Day 10 — Time, place, and description */
        [|
            ("month", "iti", {js|𒌗|js}),
            ("night", {js|ĝi|js}, {js|𒈪|js}),
            ("city", "iri", {js|𒌷|js}),
            ("country", "mada", {js|𒈠𒁕|js}),
            ("to live", "til", {js|𒋾|js}),
            ("to arrive", "sa", {js|𒁲|js}),
            ("to return", "gi", {js|𒄄|js}),
            ("near", {js|teĝ|js}, {js|𒋼|js}),
            ("far", "sud", {js|𒋤|js}),
            ("soon", "kitukumshe", {js|𒆠𒋗𒃻𒌉𒆷𒂠|js}),
        |],
    |];

    <>
        <Stack className=css##dailyVocabulary>
            <Typography 
                variant=Typography.Variant.h4 
                align=`center 
                gutterBottom=true
            >
            {"Daily Vocabulary" |> React.string}       
            </Typography>
            <Typography 
                variant=Typography.Variant.h6 
                align=`center
                gutterBottom=true
            >
            {"Learn 10 Sumerian words every day for 10 days" |> React.string}       
            </Typography>
            <Box className=css##accordionContainer>
            {
                words_list
                |> Array.mapi((day_index, day) => {
                    <Accordion 
                        key={"day-" ++ Int.to_string(day_index + 1)}
                        sx={{"width": "70%"}}
                        onChange={(_, is_expanded) => {
                            if (is_expanded) {
                                set_current_day(_ => Some(day_index))
                            } else {
                                set_current_day(_ => None)
                            }
                        }}
                    >
                        <AccordionSummary expandIcon={<TablerReact.IconChevronDown />}>
                            {
                                switch day_index {
                                    | 0 => ("Day 1" ++ " - " ++ "Ud dishkamma" ++ " - " ++ {js|𒌓𒁹𒄰𒈠|js}) |> React.string
                                    | 1 => ("Day 2" ++ " - " ++ "Ud minkamma" ++ " - " ++ {js|𒌓𒈫𒄰𒈠|js}) |> React.string
                                    | 2 => ("Day 3" ++ " - " ++ "Ud eshkamma" ++ " - " ++ {js|𒌓𒁹𒁹𒁹𒄰𒈠|js}) |> React.string
                                    | 3 => ("Day 4" ++ " - " ++ "Ud limmukamma" ++ " - " ++ {js|𒌓𒇹𒄰𒈠|js}) |> React.string
                                    | 4 => ("Day 5" ++ " - " ++ "Ud iakamma" ++ " - " ++ {js|𒌓𒐊𒄰𒈠|js}) |> React.string
                                    | 5 => ("Day 6" ++ " - " ++ "Ud ashkamma" ++ " - " ++ {js|𒌓𒐋𒄰𒈠|js}) |> React.string
                                    | 6 => ("Day 7" ++ " - " ++ "Ud iminkamma" ++ " - " ++ {js|𒌓𒅓𒄰𒈠|js}) |> React.string
                                    | 7 => ("Day 8" ++ " - " ++ "Ud ussukamma" ++ " - " ++ {js|𒌓𒐍𒄰𒈠|js}) |> React.string
                                    | 8 => ("Day 9" ++ " - " ++ "Ud ilimmukamma" ++ " - " ++ {js|𒌓𒑆𒄰𒈠|js}) |> React.string
                                    | 9 => ("Day 10" ++ " - " ++ "Ud ukamma" ++ " - " ++ {js|𒌓𒌋𒄰𒈠|js}) |> React.string
                                    | _ => "N/A" |> React.string
                                }
                            }
                        </AccordionSummary>
                        <AccordionDetails>
                            <Grid
                                container=true
                                columns=`Number(10)
                                spacing=`Number(2)
                            >
                                {
                                    day
                                    |> Array.mapi((index, (english, sumerian, cuneiform)) => {
                                        <Grid 
                                            key={english ++ Int.to_string(index)}
                                            size=`Object(Grid.ResponsiveSize.make(~xs=5, ~sm=2, ()))
                                        >
                                            <Card>
                                                <CardContent sx={{"textAlign": "center"}}>
                                                    {
                                                        if (is_test_yourself_open) {
                                                            <>
                                                                <Typography gutterBottom=true>
                                                                    {"---" |> React.string}
                                                                </Typography>
                                                                <Typography 
                                                                    variant=Typography.Variant.h5
                                                                    component=RootComponent.htmlElement("div")
                                                                >
                                                                    <span className="cuneiforms small">
                                                                        {{js|𒄿𒀄|js} |> React.string}
                                                                    </span>
                                                                </Typography>
                                                                <Typography>
                                                                    {"---" |> React.string}
                                                                </Typography>
                                                            </>
                                                        } else {
                                                            <>
                                                                <Typography gutterBottom=true>
                                                                    {english |> React.string}
                                                                </Typography>
                                                                <Typography 
                                                                    variant=Typography.Variant.h5
                                                                    component=RootComponent.htmlElement("div")
                                                                >
                                                                    <span className="cuneiforms small">
                                                                        {cuneiform |> React.string}
                                                                    </span>
                                                                </Typography>
                                                                <Typography>
                                                                    {sumerian |> React.string}
                                                                </Typography>
                                                            </>
                                                        }
                                                    }
                                                </CardContent>
                                            </Card>
                                        </Grid>
                                    })
                                    |> React.array
                                }
                            </Grid>
                        </AccordionDetails>
                        <AccordionActions>
                            <Button 
                                variant=`outlined
                                onClick={_ => {
                                    set_test_yourself_open(_ => true)
                                    set_current_day(_ => Some(day_index))
                                }}
                            >
                                {"Test yourself" |> React.string}
                            </Button>
                            <Button variant=`outlined>{"Mark as learned" |> React.string}</Button>
                        </AccordionActions>
                    </Accordion>
                })
                |> React.array
            }
            </Box>
        </Stack>
        <Dialog 
            _open=is_test_yourself_open 
            onClose={(_, _) => set_test_yourself_open(_ => false)}
        >
            <DialogTitle>
                {
                    switch current_day {
                        | Some(day) => ("Test yourself " ++ "(day " ++ string_of_int(day + 1) ++ ")")
                        | None => "Test yourself" 
                    } |> React.string
                }
            </DialogTitle>
            <DialogContent>
                <DialogContentText>
                    {
                        switch (test_yourself_category) {
                        | Some(Words) => "Test your knowledge of words"
                        | Some(Cuneiform) => "Test your knowledge of cuneiform"
                        | None => "Choose a category below to test your knowledge"
                        } |> React.string
                    }
                </DialogContentText>
            </DialogContent>
            <DialogActions>
                <Button onClick={_ => set_test_yourself_category(_ => Some(Words))}>{"Words" |> React.string}</Button>
                <Button onClick={_ => set_test_yourself_category(_ => Some(Cuneiform))}>{"Cuneiform" |> React.string}</Button>
            </DialogActions>
        </Dialog>
    </>
}
