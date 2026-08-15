[@mel.module "../styles/WorldMap.module.scss"] external css: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (map_width, set_map_width) = React.useState(_ => 0.0);
    let map_viewport_ref: React.ref(Js.nullable(Dom.element)) =
        React.useRef(Js.Nullable.null);
    let (country_details, set_country_details) = React.useState(_ => None);
    let (open_add_name_dialog, set_open_add_name_dialog) = React.useState(_ => false);
    let (new_country_name, set_new_country_name) = React.useState(_ => "");
    let (new_country_cuneiform, set_new_country_cuneiform) = React.useState(_ => "");
    let (new_country_email, set_new_country_email) = React.useState(_ => "");
    let (new_country_user_name, set_new_country_user_name) = React.useState(_ => "");

    let countryData: Js.Dict.t((string, string)) = Js.Dict.fromList([
        ("FR", ("Paransa", {js|𒉺𒁺𒀭𒊓𒆠|js})),
        ("CA", ("Kanada", {js|𒅗𒈾𒁕𒆠|js})),
    ]);

    let mapData = Js.Dict.entries(countryData)
        |> Array.map(((country, (country_name, _cuneiforms))) =>
            ReactSvgWorldmap.dataItem(
                ~country,
                ~value=ReactSvgWorldmap.Value.fromString(country_name),
            )
        );

    let handleClick = React.useCallback0(
        context => {
            set_country_details(_ => Some(context##countryCode));
        }
    );

    let handleTooltip = (context: ReactSvgWorldmap.countryContext) => {
        let country_value =
            switch (Js.Undefined.toOption(context##countryValue)) {
            | Some(value) => ReactSvgWorldmap.Value.toString(value)
            | None => ""
            };

        country_value === ""
            ? context##countryName
            : country_value ++ " (" ++ context##countryName ++ ")";
    };

    let get_country_name = country_code => {
        let normalized_code = Js.String.toUpperCase(country_code);

        ReactSvgWorldmap.regions
        |> Array.find_opt(region => region##code === normalized_code)
        |> Option.map(region => region##name);
    };

    React.useEffect1(() => {
        switch (Js.Nullable.toOption(map_viewport_ref.current)) {
        | Some(map_viewport) => {
            let observer = Browser.ResizeObserver.make((entries, _observer) => {
                if (Array.length(entries) > 0) {
                    let content_rect =
                        Browser.ResizeObserver.content_rect(
                            Array.get(entries, 0),
                        );
                    let available_width =
                        Browser.ResizeObserver.width(content_rect);
                    let available_height =
                        Browser.ResizeObserver.height(content_rect);
                    let width_from_height = available_height /. 0.75;
                    let next_map_width =
                        available_width < width_from_height
                            ? available_width
                            : width_from_height;

                    set_map_width(_ => next_map_width);
                };
            });

            Browser.ResizeObserver.observe(observer, map_viewport);
            Some(() => Browser.ResizeObserver.disconnect(observer));
        }
        | None => None
        };
    }, [||]);

    <div className={css##mainContainer}>
        <Container
            className={css##mapContainer}
            maxWidth=MaxWidth.disabled
            disableGutters=true
        >
            <h1>
                {"World Map in Modern Sumerian" |> React.string}
            </h1>
            <div
                className={css##mapViewport}
                ref={ReactDOM.Ref.domRef(map_viewport_ref)}
            >
                {
                    map_width > 0.0
                        ? <ReactSvgWorldmap.WorldMap
                            data=mapData
                            size={ReactSvgWorldmap.Size.fromFloat(map_width)}
                            containerClassName={css##worldMapWrapper}
                            backgroundColor=Config.colors##whiteSmoke
                            color=Config.colors##protonRed
                            borderColor=Config.colors##botanicalNight
                            onClickFunction=handleClick
                            tooltipTextFunction=handleTooltip
                        />
                        : React.null
                }
            </div>
            <Typography variant=Typography.Variant.body1 className={css##countryDetails}>
                {
                    switch (country_details) {
                    | Some(details) => {
                        switch (Js.Dict.get(countryData, details)) {
                        | Some((country_name, cuneiforms)) =>
                            <>
                                <span className="cuneiforms small">{cuneiforms |> React.string}</span>
                                <span>{country_name |> React.string}</span>
                                <span>
                                    {
                                        switch (get_country_name(details)) {
                                        | Some(name) => {"(" ++ name ++ ")" |> React.string}
                                        | None => React.null
                                        }
                                    }
                                </span>
                            </>
                        | None => {
                            switch (get_country_name(details)) {
                            | Some(name) => <>
                                <span>{name |> React.string}</span>
                                <Tooltip 
                                    title={"Mu emegira nubtuku" |> React.string}
                                    placement=Tooltip.Placement.top
                                    arrow=true
                                >
                                    <span className="cuneiforms x-small">
                                        {"(" ++ {js|𒈬·𒅴𒄀𒀀·𒉡𒌒𒌇|js} ++ ")" |> React.string}
                                    </span>
                                </Tooltip>
                                <Button
                                    variant=`outlined
                                    color=Color.primary
                                    size=`small
                                    onClick={_ => set_open_add_name_dialog(_ => true)}
                                >
                                    {"Add a Name" |> React.string}
                                </Button>
                            </>
                            | None => <span>{"Unknown Country" |> React.string}</span>
                            };
                        }
                        }
                    }
                    | None => React.string("Click on a country to see more details.")
                    }
                }
            </Typography>
        </Container>
        <Dialog 
            _open=open_add_name_dialog 
            onClose={(_, _) => set_open_add_name_dialog(_ => false)}
        >
            <DialogTitle>
                {
                    switch country_details {
                    | Some(details) => {
                        switch (Js.Dict.get(countryData, details)) {
                        | Some((country_name, _cuneiforms)) =>
                            {"Add a Name for " ++ country_name |> React.string}
                        | None => {
                            switch (get_country_name(details)) {
                            | Some(name) => {"Add a Name for " ++ name |> React.string}
                            | None => {"Add a Name for Unknown Country" |> React.string}
                            };
                        }
                        };
                    }
                    | None => React.string("Add a Name for Unknown Country")
                    };  
                }              
            </DialogTitle>
            {
                React.cloneElement(
                    <form
                        className={css##countryNameForm}
                        name="country-name-suggestion"
                        method="POST"
                    >
                        <input
                            type_="hidden"
                            name="form-name"
                            value="country-name-suggestion"
                        />
                        <p className={css##honeypot}>
                            <label>
                                {"Do not fill this field" |> React.string}
                                <input name="bot-field" type_="text" tabIndex={-1} />
                            </label>
                        </p>
                        <DialogContent>
                            <Grid container=true spacing=`Number(2)>
                                <Grid container=true size=`Number(12) spacing=`Number(2)>
                                    <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=6, ()))>
                                        <TextField
                                            label={"Country name in Sumerian" |> React.string}
                                            fullWidth=true
                                            size=`small
                                            name="country-name-sumerian"
                                            required=true
                                            value=new_country_name
                                            onChange={event =>
                                                set_new_country_name(_ => React.Event.Form.target(event)##value)
                                            }
                                        />
                                    </Grid>

                                    <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=6, ()))>
                                        <TextField
                                            label={"Cuneiform spelling" |> React.string}
                                            fullWidth=true
                                            size=`small
                                            name="country-name-cuneiform"
                                            required=true
                                            value=new_country_cuneiform
                                            onChange={event =>
                                                set_new_country_cuneiform(_ => React.Event.Form.target(event)##value)
                                            }
                                        />
                                    </Grid>
                                </Grid>

                                <Grid container=true size=`Number(12) spacing=`Number(2)>
                                    <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=6, ()))>
                                        <TextField
                                            label={"Your name" |> React.string}
                                            fullWidth=true
                                            size=`small
                                            name="your-name"
                                            required=true
                                            value=new_country_user_name
                                            onChange={event =>
                                                set_new_country_user_name(_ => React.Event.Form.target(event)##value)
                                            }
                                        />
                                    </Grid>

                                    <Grid size=`Object(Grid.ResponsiveSize.make(~xs=12, ~sm=6, ()))>
                                        <TextField
                                            label={"Your email address" |> React.string}
                                            fullWidth=true
                                            size=`small
                                            name="your-email"
                                            required=true
                                            value=new_country_email
                                            onChange={event =>
                                                set_new_country_email(_ => React.Event.Form.target(event)##value)
                                            }
                                        />
                                    </Grid>
                                </Grid>
                            </Grid>
                            </DialogContent>
                        <DialogActions>
                            <Button
                                type_=`button
                                variant=`contained
                                color=Color.secondary
                                onClick={_ => {
                                    set_new_country_user_name(_ => "")
                                    set_new_country_email(_ => "")
                                    set_new_country_name(_ => "")
                                    set_new_country_cuneiform(_ => "")
                                    set_open_add_name_dialog(_ => false)
                                }}
                            >
                                {"Close" |> React.string}
                            </Button>
                            <Button
                                type_=`submit
                                variant=`contained
                                color=Color.primary
                                disabled={
                                    new_country_user_name == "" ||
                                    new_country_email == "" ||
                                    new_country_name == "" ||
                                    new_country_cuneiform == ""
                                }
                            >
                                {"Submit" |> React.string}
                            </Button>
                        </DialogActions>
                    </form>,
                    {
                        "data-netlify": "true",
                        "data-netlify-honeypot": "bot-field",
                    },
                )
            }
        </Dialog>
    </div>
};
