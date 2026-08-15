/**
 * Typed Melange bindings for react-svg-worldmap 2.x.
 *
 * Package API: https://yanivam.github.io/react-svg-worldmap/api/
 */

module Value = {
  type t;

  external fromString: string => t = "%identity";
  external fromInt: int => t = "%identity";
  external fromFloat: float => t = "%identity";
  external toString: t => string = "String";
};

module Size = {
  type t;

  external fromString: string => t = "%identity";
  external fromInt: int => t = "%identity";
  external fromFloat: float => t = "%identity";

  let sm = fromString("sm");
  let md = fromString("md");
  let lg = fromString("lg");
  let xl = fromString("xl");
  let xxl = fromString("xxl");
  let responsive = fromString("responsive");
};

type dataItem;

[@mel.obj]
external dataItem: (~country: string, ~value: Value.t) => dataItem = "";

type countryContext = {
  . "countryCode": string,
  "countryName": string,
  "countryValue": Js.Undefined.t(Value.t),
  "color": string,
  "minValue": float,
  "maxValue": float,
  "prefix": string,
  "suffix": string,
};

type clickContext = {
  . "countryCode": string,
  "countryName": string,
  "countryValue": Js.Undefined.t(Value.t),
  "color": string,
  "minValue": float,
  "maxValue": float,
  "prefix": string,
  "suffix": string,
  "event": React.Event.Mouse.t,
};

module Href = {
  type t;

  external fromString: string => t = "%identity";
  external fromObject: Js.t({..}) => t = "%identity";
};

module Coordinate = {
  type t;

  external fromString: string => t = "%identity";
  external fromInt: int => t = "%identity";
  external fromFloat: float => t = "%identity";
};

module FontWeight = {
  type t;

  external fromString: string => t = "%identity";
  external fromInt: int => t = "%identity";
};

module TextAnchor = {
  type t;

  external fromString: string => t = "%identity";

  let start = fromString("start");
  let middle = fromString("middle");
  let end_ = fromString("end");
};

type textLabel;

[@mel.obj]
external textLabel: (
  ~label: string,
  ~x: Coordinate.t=?,
  ~y: Coordinate.t=?,
  ~dx: Coordinate.t=?,
  ~dy: Coordinate.t=?,
  ~fill: string=?,
  ~fontSize: Coordinate.t=?,
  ~fontWeight: FontWeight.t=?,
  ~textAnchor: TextAnchor.t=?,
  ~className: string=?,
  ~transform: string=?,
  unit,
) => textLabel = "";

type region = {
  . "name": string,
  "code": string,
};

[@mel.module "react-svg-worldmap"]
external regions: array(region) = "regions";

module WorldMap = {
  [@mel.module "react-svg-worldmap"] [@react.component]
  external make: (
    ~data: array(dataItem),
    ~title: string=?,
    ~valuePrefix: string=?,
    ~valueSuffix: string=?,
    ~color: string=?,
    ~strokeOpacity: float=?,
    ~backgroundColor: string=?,
    ~tooltipBgColor: string=?,
    ~tooltipTextColor: string=?,
    ~rtl: bool=?,
    ~size: Size.t=?,
    ~frame: bool=?,
    ~containerClassName: string=?,
    ~regionClassName: string=?,
    ~frameColor: string=?,
    ~borderColor: string=?,
    ~richInteraction: bool=?,
    ~styleFunction: (countryContext => Js.t({..}))=?,
    ~onClickFunction: (clickContext => unit)=?,
    ~tooltipTextFunction: (countryContext => string)=?,
    ~hrefFunction: (countryContext => Js.Undefined.t(Href.t))=?,
    ~textLabelFunction: (float => array(textLabel))=?,
    unit,
  ) => React.element = "WorldMap";
};
