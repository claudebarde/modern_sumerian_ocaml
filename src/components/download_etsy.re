[@mel.module "../styles/Downloads.module.scss"] external css: Js.t({..}) = "default"; 

type listing_data = {
    link: string,
    description: string,
    num_items: int,
    total_size: float,
    other: string
};

let decode_string_field = (obj, field) =>
    switch (Js.Dict.get(obj, field)) {
    | Some(value) =>
        switch (Js.Json.decodeString(value)) {
        | Some(value) => value
        | None => ""
        }
    | None => ""
    };

let decode_number_field = (obj, field) =>
    switch (Js.Dict.get(obj, field)) {
    | Some(value) =>
        switch (Js.Json.decodeNumber(value)) {
        | Some(value) => value
        | None => 0.0
        }
    | None => 0.0
    };

let decode_listing_response = response =>
    switch (Js.Json.decodeObject(response)) {
    | Some(response_object) =>
        switch (Js.Dict.get(response_object, "data")) {
        | Some(data) =>
            switch (Js.Json.decodeArray(data)) {
            | Some(rows) when Array.length(rows) > 0 =>
                switch (Js.Json.decodeObject(rows[0])) {
                | Some(row) =>
                    Some({
                        link: decode_string_field(row, "link"),
                        description: decode_string_field(row, "description"),
                        num_items:
                            decode_number_field(row, "num_items")
                            |> int_of_float,
                        total_size: decode_number_field(row, "total_size"),
                        other: decode_string_field(row, "other"),
                    })
                | None => None
                }
            | _ => None
            }
        | None => None
        }
    | None => None
    };

[@react.component]
let make = (~listing: string) => {
    open Bindings;
    open Mui;

    let (listing_url, set_listing_url) =
        React.useState(() => (None: option(listing_data)));
    let (listing_loaded, set_listing_loaded) = React.useState(() => false);
    let (listing_id, set_listing_id) = React.useState(() => "");
    let (listing_search_error, set_listing_search_error) = React.useState(() => false);

    let fetch_listing = (~show_search_error, listing_id) => {
        set_listing_loaded(_ => false);

        let _ =
            Supabase.client
            |> Supabase.Query.rpc_etsy_listing(
                "get_etsy_listing",
                Supabase.Query.etsy_listing_params(
                    ~p_listing_id=listing_id,
                    (),
                ),
            )
            |> Js.Promise.then_(response => {
                switch (decode_listing_response(response)) {
                | Some(listing_data) => {
                    set_listing_url(_ => Some(listing_data));
                    set_listing_search_error(_ => false);
                }
                | None => {
                    set_listing_url(_ => None);
                    set_listing_search_error(_ => show_search_error);
                }
                };
                set_listing_loaded(_ => true);
                Js.Promise.resolve();
            })
            |> Js.Promise.catch(error => {
                Js.log2("Unable to fetch Etsy listing:", error);
                set_listing_url(_ => None);
                set_listing_search_error(_ => show_search_error);
                set_listing_loaded(_ => true);
                Js.Promise.resolve();
            });
        ();
    };

    React.useEffect1(() => {
        fetch_listing(~show_search_error=false, listing);
        None;
    }, [|listing|]);

    <Container className=css##etsyDownloadContainer>
        {
            if (!listing_loaded) {
                React.null;
            } else switch (listing_url) {
            | Some(listing_data) => 
                <Stack className=css##etsyDownload spacing=`Number(2) useFlexGap=true>
                    <Typography
                        variant=Typography.Variant.h5
                        sx={{"display": "flex", "alignItems": "center", "gap": "8px"}}
                    >
                        <TablerReact.IconCircleCheck color=Config.colors##pacificTeal />
                        {"Your listing is ready" |> React.string}
                    </Typography>
                    <Typography
                        variant=Typography.Variant.body1
                    >
                        {"Thanks for your order. This page stays live, bookmark it and come back any time." |> React.string}
                    </Typography>
                    <Paper sx={{"padding": "16px"}}>
                        <Stack direction=`row spacing=`Number(2) useFlexGap=true>
                            <Typography
                                variant=Typography.Variant.h6
                                className="cuneiforms"                            
                            >
                                {{js|𒅴𒄀|js} |> React.string}
                            </Typography>
                            <Stack direction=`column>
                                <Typography
                                    variant=Typography.Variant.body1
                                >
                                    <strong>
                                        {listing_data.description |> React.string}
                                    </strong>
                                </Typography>
                                <Typography
                                    variant=Typography.Variant.body1
                                    sx={{"color": "grey"}}
                                >
                                    {(listing_data.num_items |> Js.Int.toString) ++ " files / " ++ (listing_data.total_size |> Js.Float.toString) ++ " MB total" |> React.string}
                                </Typography>
                                {
                                    if (listing_data.other |> String.length > 0) {
                                        <Typography
                                            variant=Typography.Variant.body1
                                        >
                                            {listing_data.other |> React.string}
                                        </Typography>
                                    } else {
                                        React.null
                                    }
                                }
                            </Stack>
                        </Stack>
                        <Button
                            variant=`contained
                            endIcon={<TablerReact.IconExternalLink />}
                            fullWidth=true
                            sx={{"marginTop": "16px"}}
                            component=RootComponent.htmlElement("a")
                            href=listing_data.link
                            target="_blank"
                        >
                            {"Open your bundle in Google Drive" |> React.string}
                        </Button>
                    </Paper>
                    <Paper sx={{"padding": "16px"}}>
                        <Typography
                            variant=Typography.Variant.body1
                        >
                            <strong>
                                {"What happens next?" |> React.string}
                            </strong>
                        </Typography>
                        <ol>
                            <li>
                                {"A Drive folder opens with all the files inside." |> React.string}
                            </li>
                            <li>
                                {"Use Download all at the top right for the whole set, or right-click a single file to save just that one." |> React.string}
                            </li>
                            <li>
                                {"The files land in your Downloads. Unzip them to access the images inside." |> React.string}
                            </li>
                        </ol>
                    </Paper>
                    <Alert severity=`warning>
                        <AlertTitle>
                            {"Seeing a \"request access\" screen?" |> React.string}
                        </AlertTitle>
                        {"That means Drive is using a different Google account than expected. Open the link in a private window, or sign out and try again. If it persists, message us and we'll email the files straight to you." |> React.string}
                    </Alert>
                    <Box>
                        <Button>
                            {"License and usage" |> React.string}
                        </Button>
                        <Button
                            component=RootComponent.htmlElement("a")
                            href="mailto:emegir.umee@gmail.com"
                            target="_blank"
                        >
                            {"Get help" |> React.string}
                        </Button>
                    </Box>
                </Stack>
            | None => 
                <Stack className=css##etsyDownload spacing=`Number(2) useFlexGap=true>
                    <Typography
                        variant=Typography.Variant.h5
                        sx={{"display": "flex", "alignItems": "center", "gap": "8px"}}
                    >
                        <TablerReact.IconSearch />
                        {"We couldn't find that listing" |> React.string}
                    </Typography>
                    <Typography
                        variant=Typography.Variant.body1
                    >
                        {"The link may have been cut short when it was copied. Enter your listing number and we'll take you straight there." |> React.string}
                    </Typography>
                    <Paper sx={{"padding": "16px"}}>
                        <FormControl 
                            fullWidth=true
                            error=listing_search_error
                        >
                            <InputLabel
                                htmlFor="listing-id"
                                sx={{"backgroundColor": "background.paper", "padding": "0 8px"}}
                            >
                                {"Listing ID" |> React.string}
                            </InputLabel>
                            <OutlinedInput
                                id="listing-id"
                                value=listing_id                                
                                endAdornment={
                                    <IconButton
                                        onClick={_ =>
                                            fetch_listing(
                                                ~show_search_error=true,
                                                listing_id,
                                            )
                                        }
                                    >
                                        <TablerReact.IconSearch />
                                    </IconButton>
                                }
                                onChange={event => set_listing_id(React.Event.Form.target(event)##value)}
                            />
                            {
                                if (listing_search_error == false) {
                                    <FormHelperText>
                                        {"It's a long string of letters and numbers that starts with \"listing-\". You will find it in the PDF you received on Etsy." |> React.string}
                                    </FormHelperText>
                                } else {
                                    <FormHelperText>
                                        {"This listing number doesn't exist." |> React.string}
                                    </FormHelperText>
                                }
                            }
                        </FormControl>
                    </Paper>
                    <Paper sx={{"padding": "16px"}}>
                        <Typography
                            variant=Typography.Variant.body1
                            sx={{"display": "flex", "alignItems": "center", "justifyContent": "space-between", "gap": "8px"}}
                        >
                            <Box sx={{"display": "flex", "alignItems": "center", "gap": "8px"}}>
                                <TablerReact.IconMail />
                                {"Still stuck? Send us an email and we'll send you the correct link." |> React.string}
                            </Box>
                            <a href="mailto:emegir.umee@gmail.com" target="_blank">{"Get help" |> React.string}</a>
                        </Typography>
                    </Paper>
                </Stack>
            }
        }
    </Container>
}
