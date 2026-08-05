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

// Central point of Ur Archaeological City, Dhi Qar, Iraq.
type city_lat_long = { ur: (float, float) };
type city_cuneiform = Js.dict(string);
let default_cities: city_lat_long = {
    ur: (30.963056, 46.103056),
};
let cities_cuneiform: city_cuneiform = Js.Dict.fromList([("ur", {js|𒋀𒀊𒆠|js})]);

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

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (current_city, _set_current_city) = React.useState(() => Some("ur"));
    let (lat_long, _set_lat_long) = React.useState(() => Some(default_cities.ur));
    let (weather_code, set_weather_code) = React.useState(() => None);
    let (_temperature, set_temperature) = React.useState(() => None);
    let (anchor_el, set_anchor_el) =
        React.useState(() =>
            (Js.Nullable.null: Js.Nullable.t(Dom.element))
        );
    let (open_popover, set_open_popover) = React.useState(() => false);

    let show_popover = (event: React.Event.Mouse.t) => {
        set_anchor_el(_ =>
            React.Event.Mouse.currentTarget(event)
            |> dom_element_from_event_target
            |> Js.Nullable.return
        );
        set_open_popover(_ => true);
    };

    React.useEffect0(() => {
        switch lat_long {
            | Some((lat, long)) => {
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
                                
                                Js.log2(temperature, weather_code);
                            | None => Js.log("Current weather is unavailable")
                            };
                       };
                       Js.Promise.resolve();
                   })
                |> ignore;
            }
            | None => ();
        };

        None
    });

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
                                | _ => "Full Moon"
                                }
                            } 
                            className=css##meteoWidgetIcon 
                        />
                    }
                    label={
                        switch weather_code {
                        | Some(Clear) => {js|𒌓|js}
                        | Some(Cloudy) => {js|𒅎𒋛𒀀|js}
                        | Some(Fog) => {js|𒁇𒀀𒀭|js}
                        | Some(Rain) => {js|𒀀𒀭|js}
                        | _ => ""
                        } ++ " " ++
                        switch (Js.Dict.get(cities_cuneiform, city)) {
                        | Some(cuneiform) => cuneiform ++ {js|𒀀|js}
                        | None => city |> capitalize
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
                    <Paper>
                        <Typography sx={{"padding": "1rem"}} variant=Typography.Variant.body1>
                            {"Weather details" |> React.string}
                        </Typography>
                    </Paper>
                </Popover>
            </>
        | _ => <Chip icon={<TablerReact.IconQuestionMark />} label="No data" className=css##meteoWidget />
        }
    }
}
