[@mel.module "../styles/Downloads.module.scss"] external css: Js.t({..}) = "default"; 

type listing_data = {
    link: string,
    description: string,
    num_items: int,
    total_size: float,
    other: string
};

[@react.component]
let make = (~listing: string) => {
    open Bindings;
    open Mui;

    let listing_urls: Js.Dict.t(listing_data) = Js.Dict.fromList(
        [
            ("listing-1fhXYfB5bvufN_uTyUP0dt2fNfsXBoSCX", 
            {
                link: "https://drive.google.com/drive/folders/1fhXYfB5bvufN_uTyUP0dt2fNfsXBoSCX?usp=drive_link",
                description: "Ancient Sumer: Everyday Life",
                num_items: 3,
                total_size: 365.7,
                other: "Color + black and white PNG files",
            })
        ]
    );

    let (listing_url, set_listing_url) = React.useState(() => Js.Dict.get(listing_urls, listing));
    let (listing_id, set_listing_id) = React.useState(() => "");
    let (listing_search_error, set_listing_search_error) = React.useState(() => false);

    <Container className=css##etsyDownloadContainer>
        {
            switch (listing_url) {
            | Some(listing_data) => 
                <Stack className=css##etsyDownload spacing=`Number(2) useFlexGap=true>
                    <Typography
                        variant=Typography.Variant.h5
                        sx={{"display": "flex", "alignItems": "center", "gap": "8px"}}
                    >
                        <TablerReact.IconCircleCheck />
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
                                        onClick={_ => {
                                            switch (Js.Dict.get(listing_urls, listing_id)) {
                                                | Some(url) => {
                                                    set_listing_url(_ => Some(url));
                                                    set_listing_search_error(_ => false);
                                                }
                                                | None => {
                                                    set_listing_url(_ => None);
                                                    set_listing_search_error(_ => true);
                                                }
                                            }                                            
                                        }}
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
