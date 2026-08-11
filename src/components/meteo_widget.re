// ICONS: https://icons8.com/icons/set/weather--style-color
[@mel.module "../assets/weather/icons8-summer.gif"] external sunGif: string = "default";
[@mel.module "../assets/weather/icons8-partly-cloudy-day.gif"] external partlyCloudyGif: string = "default";
[@mel.module "../assets/weather/icons8-clouds-96.png"] external cloudyPng: string = "default";
[@mel.module "../assets/weather/icons8-haze.gif"] external fogGif: string = "default";
[@mel.module "../assets/weather/icons8-light-rain.gif"] external drizzleGif: string = "default";
[@mel.module "../assets/weather/icons8-rain.gif"] external rainGif: string = "default";
[@mel.module "../assets/weather/icons8-snow.gif"] external snowGif: string = "default";
[@mel.module "../assets/weather/icons8-cloud-lightning.gif"] external thunderstormGif: string = "default";
[@mel.module "../assets/weather/icons8-rainbow-96.png"] external rainbowPng: string = "default";
[@mel.module "../assets/weather/icons8-thermometer-96.png"] external thermometerPng: string = "default";

[@mel.module "../styles/MeteoWidget.module.scss"] external css: Js.t({..}) = "default"; 
external dom_element_from_event_target: Js.t({..}) => Dom.element = "%identity";

/** An opaque response returned by the Open-Meteo FlatBuffers SDK. */
type weather_api_response;
type variables_with_time;
type variable_with_values;

module WeatherApiResponse = {
    [@mel.send]
    external current: (
      [@mel.this] weather_api_response,
    ) => Js.Nullable.t(variables_with_time) = "current";

    [@mel.send]
    external latitude: ([@mel.this] weather_api_response) => float = "latitude";

    [@mel.send]
    external longitude: ([@mel.this] weather_api_response) => float = "longitude";

    [@mel.send]
    external utc_offset_seconds: (
      [@mel.this] weather_api_response,
    ) => int = "utcOffsetSeconds";
};

module VariablesWithTime = {
    [@mel.send]
    external variables: (
      int,
      [@mel.this] variables_with_time,
    ) => Js.Nullable.t(variable_with_values) = "variables";

    [@mel.send]
    external interval: ([@mel.this] variables_with_time) => int = "interval";

    [@mel.send]
    external variables_length: (
      [@mel.this] variables_with_time,
    ) => int = "variablesLength";
};

module VariableWithValues = {
    [@mel.send]
    external value: ([@mel.this] variable_with_values) => float = "value";
};

type weather_api_params;

[@mel.obj]
external make_weather_api_params: (
  ~latitude: float,
  ~longitude: float,
  ~current: string,
  ~daily: string=?,
  ~hourly: string=?,
  unit,
) => weather_api_params = "";

type weather_condition =
  | Clear
  | PartlyCloudy
  | Cloudy
  | Fog
  | Drizzle
  | Rain
  | Snow
  | Thunderstorm
  | Unknown;

let weather_condition_from_code = code =>
  switch code {
  | 0 => Clear
  | 1 | 2 => PartlyCloudy
  | 3 => Cloudy
  | 45 | 48 => Fog
  | 51 | 53 | 55 | 56 | 57 => Drizzle
  | 61 | 63 | 65 | 66 | 67 | 80 | 81 | 82 => Rain
  | 71 | 73 | 75 | 77 | 85 | 86 => Snow
  | 95 | 96 | 99 => Thunderstorm
  | _ => Unknown
  };

/**
 * Binding for:
 * import { fetchWeatherApi } from "openmeteo";
 *
 * The parameter object remains open because Open-Meteo supports a large,
 * endpoint-dependent set of query parameters.
 */
[@mel.module "openmeteo"]
external fetchWeatherApi: (
  string,
  weather_api_params,
) => Js.Promise.t(array(weather_api_response)) = "fetchWeatherApi";

// Central coordinates for the archaeological cities.
type city_lat_long = Js.dict((float, float));
type city_cuneiform = Js.dict(string);
let default_cities: city_lat_long = Js.Dict.fromList([
    ("ur", (30.963056, 46.103056)),
    ("nippur", (32.126944, 45.230832)),
    ("lagash", (31.4025, 46.4025)),
]);
let cities_cuneiform: city_cuneiform = Js.Dict.fromList([
    ("ur", {js|𒋀𒀊𒆠|js}),
    ("nippur", {js|𒂗𒆤𒆠|js}),
    ("lagash", {js|𒉢𒁓𒆷𒆠|js}),
]);

let capitalize = value =>
    if (Js.String.length(value) === 0) {
        value;
    } else {
        let first_character =
            value
            |> Js.String.charAt(~index=0)
            |> Js.String.toUpperCase;
        let remaining_characters = value |> Js.String.slice(~start=1);
        first_character ++ remaining_characters;
    };

type language = English | Sumerian;

[@react.component]
let make = () => {
    open Bindings;
    open Mui;


    let (current_city, set_current_city) = React.useState(() => None);
    let (lat_long, set_lat_long) =
        React.useState(() => None);
    let (weather_code, set_weather_code) = React.useState(() => None);
    let (temperature, set_temperature) = React.useState(() => None);
    let (anchor_el, set_anchor_el) =
        React.useState(() =>
            (Js.Nullable.null: Js.Nullable.t(Dom.element))
        );
    let (open_popover, set_open_popover) = React.useState(() => false);
    let (language, set_language) = React.useState(() => Sumerian);
    let (cities_menu_open, set_cities_menu_open) = React.useState(() => false);
    let (city_menu_anchor_el, set_city_menu_anchor_el) =
        React.useState(() =>
            (Js.Nullable.null: Js.Nullable.t(Dom.element))
        );

    let show_popover = (event: React.Event.Mouse.t) => {
        set_anchor_el(_ =>
            React.Event.Mouse.currentTarget(event)
            |> dom_element_from_event_target
            |> Js.Nullable.return
        );
        set_open_popover(_ => true);
    };

    let get_current_location = () => {
        Browser.Geolocation.get_current_position(
            ~success=(position => {
                let coordinates =
                Browser.Geolocation.coordinates(position);

                let latitude =
                Browser.Geolocation.latitude(coordinates);

                let longitude =
                Browser.Geolocation.longitude(coordinates);

                set_lat_long(_ => Some((latitude, longitude)));
                set_current_city(_ => Some("your city"));

                // saves the user's current location in local storage
                LocalStorage.set_location({
                    city: "your city",
                    cuneiforms: "𒌷𒍝",
                    lat_long: (latitude, longitude),
                });
            }),
            ~error=(error => {
                Js.log(Browser.Geolocation.error_message(error));
            }),
            ~options=Browser.Geolocation.make_options(
                ~enableHighAccuracy=true,
                ~timeout=10000,
                ~maximumAge=0,
                (),
            ),
            (),
        );
    };

    React.useEffect1(() => {
        switch (current_city, lat_long) {
            | (None, None) => {
                // checks if there is no location stored in local storage
                // if not, it initializes the city and lat_long with Ur's coordinates
                switch (LocalStorage.get_location()) {
                | Some(location) => {
                    set_current_city(_ => Some(location.city));
                    set_lat_long(_ => Some(location.lat_long));
                }
                | None => {
                    set_current_city(_ => Some("ur"));
                    set_lat_long(_ => Js.Dict.get(default_cities, "ur"));
                }
                };
            }
            | (Some(_city), Some((lat, long))) => {
                let params = make_weather_api_params(
                    ~latitude=lat,
                    ~longitude=long,
                    ~current="temperature_2m,weather_code",
                    (),
                );
                fetchWeatherApi("https://api.open-meteo.com/v1/forecast", params)
                |> Js.Promise.then_(response => {
                       // response should be an array of weather_api_response objects, we need the first entry
                       if (Array.length(response) > 0) {
                            let first_entry = Array.get(response, 0);
                            switch (
                                first_entry
                                |> WeatherApiResponse.current
                                |> Js.Nullable.toOption
                            ) {
                            | Some(current) =>
                                let temperature = 
                                    switch (
                                        current
                                        |> VariablesWithTime.variables(0)
                                        |> Js.Nullable.toOption
                                    ) {
                                    | Some(temperature_variable) =>
                                        temperature_variable
                                        |> VariableWithValues.value
                                        |> int_of_float
                                        |> Option.some;
                                    | None => None
                                    };
                                    
                                let weather_code = 
                                    switch (
                                        current
                                        |> VariablesWithTime.variables(1)
                                        |> Js.Nullable.toOption
                                    ) {
                                    | Some(weather_code_variable) =>
                                        weather_code_variable
                                        |> VariableWithValues.value
                                        |> int_of_float
                                        |> weather_condition_from_code
                                        |> Option.some;
                                    | None => None
                                    };

                                set_temperature(_ => temperature);
                                set_weather_code(_ => weather_code);
                            | None => Js.log("Current weather is unavailable")
                            };
                       };
                       Js.Promise.resolve();
                   })
                |> ignore;
            }
            | _ => ();
        };

        None
    }, [|lat_long|]);

    {
        switch (current_city, lat_long) {
        | (Some(city), Some(_)) =>
            <>
                <Chip 
                    color=Color.primary
                    variant=`outlined
                    size=`medium
                    clickable=true
                    onClick={show_popover}
                    sx={{
                        "& .MuiChip-avatar": {
                            "backgroundColor": "transparent",
                        },
                        "fontFamily": "CuneiformComposite, sans-serif",
                    }}
                    avatar={
                        <Avatar 
                            src={
                                switch weather_code {
                                | Some(Clear) => sunGif
                                | Some(PartlyCloudy) => partlyCloudyGif
                                | Some(Cloudy) => cloudyPng
                                | Some(Fog) => fogGif
                                | Some(Drizzle) => drizzleGif
                                | Some(Rain) => rainGif
                                | Some(Snow) => snowGif
                                | Some(Thunderstorm) => thunderstormGif
                                | _ => rainbowPng
                            }}
                            alt={
                                switch weather_code {
                                | Some(Clear) => "Sun"
                                | Some(PartlyCloudy) => "Partly Cloudy"
                                | Some(Cloudy) => "Cloudy"
                                | Some(Fog) => "Fog"
                                | Some(Drizzle) => "Drizzle"
                                | Some(Rain) => "Rain"
                                | Some(Snow) => "Snow"
                                | Some(Thunderstorm) => "Thunderstorm"
                                | _ => "Rainbow"
                                }
                            } 
                            sx={{"backgroundColor": "white"}}
                            className=css##meteoWidgetIcon 
                        />
                    }
                    label={
                        switch weather_code {
                        | Some(Clear) => {js|𒌓𒁕·|js}
                        | Some(Cloudy) => {js|𒅎𒋛𒀀𒁕·|js}
                        | Some(Fog) => {js|𒁇𒀀𒀭𒁕·|js}
                        | Some(Rain) => {js|𒀀𒀭𒁕·|js}
                        | _ => ""
                        } ++ " " ++
                        switch (Js.Dict.get(cities_cuneiform, city)) {
                        | Some(cuneiform) => cuneiform ++ {js|𒀀|js}
                        | None when city === "your city" => 
                            switch language {
                            | English => "your city"
                            | Sumerian => {js|𒌷𒍝|js}
                            }
                        | _ => city |> capitalize
                    }}
                    className=css##meteoWidget
                />
                <Popover
                    _open=open_popover
                    anchorEl=anchor_el
                    anchorOrigin={{
                        vertical: `top,
                        horizontal: `center,
                    }}
                    transformOrigin={{
                        vertical: `bottom,
                        horizontal: `center,
                    }}
                    onClose={_ => {
                        set_open_popover(_ => false);
                        set_anchor_el(_ => Js.Nullable.null);
                    }}
                >
                    <Paper className=css##meteoWidgetPopover>
                        <Stack>
                            {
                                let city_in_cuneiform =
                                    switch (Js.Dict.get(cities_cuneiform, city)) {
                                    | Some(cuneiform) => cuneiform ++ {js|𒀀|js}
                                    | None when city === "your city" => 
                                        switch language {
                                        | English => "your city"
                                        | Sumerian => {js|𒌷𒍝|js}
                                        }
                                    | _ => city |> capitalize
                                    };

                                <Typography 
                                    variant=Typography.Variant.h6 
                                    sx={{"marginBottom": "0.5rem"}}
                                    align=`center                                    
                                >
                                    {
                                        switch language {
                                        | English => (("Weather in " ++ (city |> capitalize)) |> React.string)
                                        | Sumerian => 
                                            <span className="cuneiforms small">
                                                ({js|𒀭·|js} ++ city_in_cuneiform |> React.string)
                                            </span>
                                        }}
                                </Typography>
                            }
                            <Divider sx={{"marginBottom": "0.5rem"}} />
                            <Typography 
                                variant=Typography.Variant.body1 
                                sx={{"display": "flex", "alignItems": "center", "padding": "0.5rem 0"}}
                            >
                                <img src={
                                    switch weather_code {
                                    | Some(Clear) => sunGif
                                    | Some(PartlyCloudy) => partlyCloudyGif
                                    | Some(Cloudy) => cloudyPng
                                    | Some(Fog) => fogGif
                                    | Some(Drizzle) => drizzleGif
                                    | Some(Rain) => rainGif
                                    | Some(Snow) => snowGif
                                    | Some(Thunderstorm) => thunderstormGif
                                    | _ => rainbowPng
                                }}
                                alt={
                                    switch weather_code {
                                    | Some(Clear) => "Sun"
                                    | Some(PartlyCloudy) => "Partly Cloudy"
                                    | Some(Cloudy) => "Cloudy"
                                    | Some(Fog) => "Fog"
                                    | Some(Drizzle) => "Drizzle"
                                    | Some(Rain) => "Rain"
                                    | Some(Snow) => "Snow"
                                    | Some(Thunderstorm) => "Thunderstorm"
                                    | _ => "Full Moon"
                                    } 
                                }
                                />
                                <span className="cuneiforms x-small">
                                    {
                                        switch weather_code {
                                        | Some(Clear) => {
                                            switch language {
                                            | English => "Sunny"
                                            | Sumerian => {js|𒌓𒁕|js}
                                            }
                                        }
                                        | Some(Cloudy) => {
                                            switch language {
                                            | English => "Cloudy"
                                            | Sumerian => {js|𒅎𒋛𒀀𒁕|js}
                                            }
                                        }
                                        | Some(Fog) => {
                                            switch language {
                                            | English => "Foggy"
                                            | Sumerian => {js|𒁇𒀀𒀭𒁕|js}
                                            }
                                        }
                                        | Some(Rain) => {
                                            switch language {
                                            | English => "Rainy"
                                            | Sumerian => {js|𒀀𒀭𒁕|js}
                                            }
                                        }
                                        | Some(PartlyCloudy) => {
                                            switch language {
                                            | English => "Partly Cloudy"
                                            | Sumerian => {js|𒌓·𒅎𒋛𒀀𒁕|js}
                                            }
                                        }
                                        | Some(Drizzle) => {
                                            switch language {
                                            | English => "Drizzle"
                                            | Sumerian => {js|𒅎𒂂𒁕|js}
                                            }
                                        }
                                        | Some(Snow) => {
                                            switch language {
                                            | English => "Snowy"
                                            | Sumerian => {js|𒊾𒁕|js}
                                            }
                                        }
                                        | Some(Thunderstorm) => {
                                            switch language {
                                            | English => "Thunderstorm"
                                            | Sumerian => {js|𒅗𒀭𒉌𒋛𒁕|js}
                                            }
                                        }
                                        | _ => {
                                            switch language {
                                            | English => "No data"
                                            | Sumerian => {js|𒃻𒈾𒈨·𒉡𒌋𒅅|js}
                                            }
                                        }
                                        } |> React.string
                                    }
                                </span>
                            </Typography>
                            <Typography 
                                variant=Typography.Variant.body1 
                                sx={{"display": "flex", "alignItems": "center", "padding": "0.5rem 0"}}
                            >
                                <img src=thermometerPng alt="Thermometer" />
                                {
                                    switch temperature {
                                    | Some(temp) => string_of_int(temp) ++ {js|°C|js}
                                    | None => "N/A"
                                    } |> React.string
                                }
                            </Typography>
                            <div className="buttons-group">
                                <Button 
                                    variant=`outlined
                                    color=Color.primary
                                    sx={{"marginTop": "0.5rem", "marginRight": "0.5rem", "fontSize": "0.75rem"}}
                                    onClick={_ => {
                                        set_language(prev_language =>
                                            switch prev_language {
                                            | English => Sumerian
                                            | Sumerian => English
                                            }
                                        );
                                    }}
                                >
                                    {
                                        switch language {
                                        | English => 
                                            <span className="cuneiforms" style={ReactDOM.Style.make(~fontSize="0.75rem", ())}>
                                                ({js|𒅴𒄀|js} |> React.string)
                                            </span>
                                        | Sumerian => "English" |> React.string
                                        }
                                    }
                                </Button>
                                <Button 
                                    variant=`contained
                                    color=Color.primary
                                    endIcon={<TablerReact.IconChevronUp />}
                                    sx={{"marginTop": "0.5rem"}}
                                    onClick={event => {
                                        set_city_menu_anchor_el(_ =>
                                            React.Event.Mouse.currentTarget(event)
                                            |> dom_element_from_event_target
                                            |> Js.Nullable.return
                                        );
                                        set_cities_menu_open(_ => true);
                                    }}
                                >
                                    {
                                        switch language {
                                        | English => 
                                            <span>
                                                {"Other cities" |> React.string}
                                            </span>
                                        | Sumerian => 
                                            <span className="cuneiforms" style={ReactDOM.Style.make(~fontSize="0.75rem", ())}>
                                                ({js|𒌷·𒉽𒊏|js} |> React.string)                                                
                                            </span>
                                        }
                                    }
                                </Button>
                                <Menu
                                    anchorEl=city_menu_anchor_el
                                    _open=cities_menu_open
                                    anchorOrigin={{
                                        vertical: `top,
                                        horizontal: `center,
                                    }}
                                    transformOrigin={{
                                        vertical: `bottom,
                                        horizontal: `center,
                                    }}
                                    onClose={_ => set_cities_menu_open(_ => false)}
                                >
                                    {
                                        Js.Dict.entries(cities_cuneiform)
                                        |> Array.map(((city_key, city_cuneiform)) => {
                                            <MenuItem
                                                key=city_key
                                                onClick={_ => {
                                                    set_current_city(_ => Some(city_key));
                                                    set_lat_long(_ => 
                                                        switch (Js.Dict.get(default_cities, city_key)) {
                                                        | Some(lat_long) => Some(lat_long)
                                                        | None => None
                                                        }
                                                    );
                                                    set_cities_menu_open(_ => false);
                                                }}
                                            >
                                                {
                                                    switch language {
                                                    | English => city_key |> capitalize |> React.string
                                                    | Sumerian => 
                                                        <span className="cuneiforms x-small">
                                                            ({city_cuneiform} |> React.string)
                                                        </span>
                                                    }
                                                }
                                            </MenuItem>
                                        })
                                        |> React.array
                                    }
                                    <Divider />
                                    <MenuItem
                                        onClick={_ => {
                                            get_current_location()
                                            set_cities_menu_open(_ => false);
                                        }}
                                    >
                                        {
                                            switch language {
                                            | English => "Use my location" |> React.string
                                            | Sumerian => 
                                                <span className="cuneiforms x-small">
                                                    ({js|𒀭·𒌷𒂷|js} |> React.string)
                                                </span>
                                            }
                                        }
                                    </MenuItem>
                                </Menu>
                            </div>
                        </Stack>
                    </Paper>
                </Popover>
            </>
        | _ => <Chip icon={<TablerReact.IconQuestionMark />} label="No data" className=css##meteoWidget />
        }
    }
}
