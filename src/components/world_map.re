[@mel.module "../styles/WorldMap.module.scss"] external css: Js.t({..}) = "default"; 

type continent =
  | Africa
  | Asia
  | Europe
  | NorthAmerica
  | SouthAmerica
  | Oceania;

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
    let (expanded_continent, set_expanded_continent) = React.useState(_ => None);
    let (country_name_input, set_country_name_input) = React.useState(_ => "");

    let is_mobile = UseMediaQuery.use("(max-width:599px)");

    let countryData: Js.Dict.t((string, string)) = Js.Dict.fromList([
        ("FR", ("Paransa", {js|𒉺𒁺𒀭𒊓𒆠|js})),
        ("CA", ("Kanada", {js|𒅗𒈾𒁕𒆠|js})),
    ]);

    let continent_by_code: Js.Dict.t(continent) =
        Js.Dict.fromList([
            ("CA", NorthAmerica),
            ("FR", Europe),
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

    let find_country = input => {
        let normalized_input =
            input
            |> Js.String.trim
            |> Js.String.toLowerCase;

        ReactSvgWorldmap.regions
        |> Array.find_opt(region => {
            let normalized_name =
                region##name
                |> Js.String.trim
                |> Js.String.toLowerCase;
            let normalized_code =
                region##code
                |> Js.String.toLowerCase;

            normalized_name === normalized_input
            || normalized_code === normalized_input;
        });
    };

    let matching_country = find_country(country_name_input);
    let country_name_has_error =
        Js.String.length(Js.String.trim(country_name_input)) > 0
        && Option.is_none(matching_country);
    let selected_country_english_name =
        switch country_details {
        | Some(country_code) =>
            switch (get_country_name(country_code)) {
            | Some(country_name) => country_name
            | None => ""
            }
        | None => ""
        };

    let country_code_to_flag = country_code => {
        let normalized_code = Js.String.toUpperCase(country_code);

        if (Js.String.length(normalized_code) !== 2) {
            "";
        } else {
            normalized_code
            |> Js.String.split(~sep="")
            |> Array.map(letter =>
                switch (letter |> Js.String.codePointAt(~index=0)) {
                | Some(code_point) =>
                    Js.String.fromCodePoint(code_point + 0x1F1A5)
                | None => ""
                }
            )
            |> Js.Array.join(~sep="");
        };
    };

    let countries_by_continent = (continent_to_search: continent) =>
        Js.Dict.entries(continent_by_code)
        |> Array.fold_left(
            (acc, (code, continent)) =>
                if (continent === continent_to_search) {
                    Array.append(acc, [|code|]);
                } else {
                    acc;
                },
            [||],
        );

    let show_countries_in_continent = (continent_to_search: continent) => {
        let countries =
            countries_by_continent(continent_to_search);
        if (Array.length(countries) == 0) {
            <Stack spacing=`Number(2)>
                <span className="cuneiforms small">{{js|𒈠𒁕·𒉡𒅅𒅅|js} |> React.string}</span>
                <span>{{js|Mada nuĝal-ĝal|js} |> React.string}</span>
                <span>{"No countries found." |> React.string}</span>
                <TextField 
                        label={"Add a Country" |> React.string}
                        variant=`outlined
                        size=`small
                    />
            </Stack>
        } else {
            <List dense=true sx={{"width": "100%"}}>
                {countries
                |> Array.map(code => 
                    <ListItem key={code}>
                        {
                            switch (Js.Dict.get(countryData, code)) {
                            | Some((country_name, cuneiforms)) =>
                                <>
                                    <ListItemIcon sx={{"fontSize": "1.7rem", "minWidth": "40px", "color": "rgba(0, 0, 0, 1)", "opacity": 1,}}>
                                        {country_code_to_flag(code) |> React.string}
                                    </ListItemIcon>
                                    <ListItemText
                                        key={code}
                                        primary={
                                            switch (get_country_name(code)) {
                                            | Some(name) => {country_name ++ " (" ++ name ++ ")" |> React.string}
                                            | None => React.null
                                            }
                                        }
                                        secondary={<span className="cuneiforms small">{cuneiforms |> React.string}</span>}
                                    />
                                </>
                            | None => React.null
                            }
                        }
                    </ListItem>)
                |> React.array}
            </List>
        }
    }

    let continent_is_expanded = continent_to_check =>
        switch expanded_continent {
        | Some(continent) => continent === continent_to_check
        | None => false
        };

    let handle_continent_change = continent =>
        (_event: React.Event.Synthetic.t, is_expanded: bool) =>
            set_expanded_continent(_ =>
                is_expanded ? Some(continent) : None
            );

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
        {is_mobile 
            ? <Stack>
                <h1> {"World Countries" |> React.string} </h1>
                <Box sx={{"margin": "20px 0px"}}>
                    <Accordion>
                        <AccordionSummary expandIcon={<TablerReact.IconChevronDown />} >
                            {"Add a new country" |> React.string}
                        </AccordionSummary>
                        <AccordionDetails>
                            <FormControl
                                fullWidth=true
                                size=`small
                                variant=`outlined
                                >
                                <InputLabel htmlFor="country-name-input">
                                    {"Add a country" |> React.string}
                                </InputLabel>
                                <OutlinedInput
                                    id="country-name-input"
                                    label={"Add a country" |> React.string}
                                    value=country_name_input
                                    error=country_name_has_error
                                    onChange={event =>
                                        set_country_name_input(_ =>
                                            React.Event.Form.target(event)##value
                                        )
                                    }
                                    endAdornment={
                                        <InputAdornment position=`end_>
                                            <IconButton
                                                edge=`end_
                                                ariaLabel="Open the add-country dialog"
                                                disabled={Option.is_none(matching_country)}
                                                variant=`contained
                                                color=Color.primary
                                                onClick={_ =>
                                                    switch matching_country {
                                                    | Some(country) => {
                                                        set_country_details(_ => Some(country##code));
                                                        set_open_add_name_dialog(_ => true);
                                                    }
                                                    | None => ()
                                                    }
                                                }
                                            >
                                                <TablerReact.IconCirclePlusFilled />
                                            </IconButton>
                                        </InputAdornment>
                                    }
                                />
                                <FormHelperText error=country_name_has_error>
                                    {
                                        country_name_has_error
                                            ? React.string("Enter a valid English country name or ISO code.")
                                            : switch matching_country {
                                            | Some(country) =>
                                                React.string(country##name ++ " - " ++ country##code)
                                            | None =>
                                                React.string("Enter a country name, for example Russia.")
                                            }
                                    }
                                </FormHelperText>
                            </FormControl>
                        </AccordionDetails>                    
                    </Accordion>
                    <Accordion 
                        expanded={continent_is_expanded(Africa)} 
                        onChange={handle_continent_change(Africa)}>
                        <AccordionSummary expandIcon={<TablerReact.IconChevronDown />} >
                            {"Africa" ++ " (" ++ (Array.length(countries_by_continent(Africa)) |> Int.to_string) ++ ")" |> React.string}
                        </AccordionSummary>
                        <AccordionDetails>
                            {show_countries_in_continent(Africa)}
                        </AccordionDetails>                    
                    </Accordion>
                    <Accordion 
                        expanded={continent_is_expanded(Asia)} 
                        onChange={handle_continent_change(Asia)}>
                        <AccordionSummary expandIcon={<TablerReact.IconChevronDown />} >
                            {"Asia" ++ " (" ++ (Array.length(countries_by_continent(Asia)) |> Int.to_string) ++ ")" |> React.string}
                        </AccordionSummary>
                        <AccordionDetails>
                            {show_countries_in_continent(Asia)}
                        </AccordionDetails>                    
                    </Accordion>
                    <Accordion 
                        expanded={continent_is_expanded(Europe)} 
                        onChange={handle_continent_change(Europe)}>
                        <AccordionSummary expandIcon={<TablerReact.IconChevronDown />} >
                            {"Europe" ++ " (" ++ (Array.length(countries_by_continent(Europe)) |> Int.to_string) ++ ")" |> React.string}
                        </AccordionSummary>
                        <AccordionDetails>
                            { show_countries_in_continent(Europe) }
                        </AccordionDetails>                    
                    </Accordion>
                    <Accordion 
                        expanded={continent_is_expanded(NorthAmerica)} 
                        onChange={handle_continent_change(NorthAmerica)}>
                        <AccordionSummary expandIcon={<TablerReact.IconChevronDown />} >
                            {"North America" ++ " (" ++ (Array.length(countries_by_continent(NorthAmerica)) |> Int.to_string) ++ ")" |> React.string}
                        </AccordionSummary>
                        <AccordionDetails>
                            {show_countries_in_continent(NorthAmerica)}
                        </AccordionDetails>                    
                    </Accordion>
                    <Accordion 
                        expanded={continent_is_expanded(SouthAmerica)} 
                        onChange={handle_continent_change(SouthAmerica)}>
                        <AccordionSummary expandIcon={<TablerReact.IconChevronDown />} >
                            {"South America" ++ " (" ++ (Array.length(countries_by_continent(SouthAmerica)) |> Int.to_string) ++ ")" |> React.string}
                        </AccordionSummary>
                        <AccordionDetails>
                            {show_countries_in_continent(SouthAmerica)}
                        </AccordionDetails>                    
                    </Accordion>
                    <Accordion 
                        expanded={continent_is_expanded(Oceania)} 
                        onChange={handle_continent_change(Oceania)}>
                        <AccordionSummary expandIcon={<TablerReact.IconChevronDown />} >
                            {"Oceania" ++ " (" ++ (Array.length(countries_by_continent(Oceania)) |> Int.to_string) ++ ")" |> React.string}
                        </AccordionSummary>
                        <AccordionDetails>
                            {show_countries_in_continent(Oceania)}
                        </AccordionDetails>                    
                    </Accordion>                    
                </Box>
            </Stack>
            : <Container
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
                                richInteraction=true
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
                        | None => React.string("Click on a country to see more details. Double-click to zoom in and out.")
                        }
                    }
                </Typography>
            </Container>
        }
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
                        action="/country-name-success"
                        onSubmit={event =>
                            if (Config.isDevelopment) {
                                React.Event.Form.preventDefault(event);
                                ReasonReactRouter.push("/country-name-success");
                            }
                        }
                    >
                        <input
                            type_="hidden"
                            name="form-name"
                            value="country-name-suggestion"
                        />
                        <input
                            type_="hidden"
                            name="country-name-english"
                            value=selected_country_english_name
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
                                            name="user-name"
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
                                            name="user-email"
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
