/**
 * Dom is a collection of browser types provided by Melange.
 * See: https://melange.re/v2.2.0/api/ml/melange/Dom/index.html
 */
include Dom;

/**
 * The following is syntax for creating bindings to JavaScript. This feature is
 * likely the most foreign that you will encounter in Melange and ReasonML.
 * You can find extensive documentation on the subject here:
 * https://melange.re/v2.2.0/communicate-with-javascript/
 *
 * In the example below, we define two external bindings: `get_element_by_id`
 * and `set_inner_html`. These bindings allow us to interface directly with
 * JavaScript's DOM API in a type-safe manner using ReasonML.
 *
 * - `[@mel.scope "document"]` specifies the JavaScript object where the
 *   function is located, similar to specifying the object in JavaScript's dot
 *   notation.
 *
 * - `[@mel.return nullable]` indicates that the return type from the JavaScript
 *   function can be `null`, mapped to ReasonML's `option` type for safety.
 *
 * - The `external` keyword is used to declare a binding to a JavaScript
 *   function.
 *
 * - `get_element_by_id` is a binding to JavaScript's `getElementById` method.
 *
 * - `set_inner_html` is a binding to set the `innerHTML` property on a element.
 *
 * - In ReasonML, external bindings respect JavaScript's naming conventions,
 *   hence the camelCase in the external declaration.
 *
 * These bindings enable direct interaction with the DOM in a way that feels
 * natural in ReasonML while ensuring type safety and clarity.
 *
 * You can use the above bindings like so:
 *
 * let _ =
 *   "root"
 *   |> get_element_by_id
 *   |. set_inner_html "<p>hello world</p>"
 *
 * and it will generate the following JS:
 *
 * document.getElementById("root").innerHTML = "<p>hello world</p>";
 *
 * A quick note on `|>` and `|.`:
 *
 * - The `|>` operator is known as the 'pipe' operator. It is used to pass the
 *   result of the expression on the left side as the last argument to the
 *   function on the right side. This operator simplifies the code by allowing a
 *   more readable, left-to-right flow of data. For example, `x |> f` is
 *   equivalent to `f(x)`.
 *
 * - The `|.` operator, on the other hand, is a 'pipe first' operator. It is
 *   used to pass the value on the left as the first argument to the function on
 *   the right. This is particularly useful for chaining methods that expect the
 *   object they operate on to be the first argument, following the JavaScript
 *   method invocation pattern. For instance, `x |. g` effectively becomes
 *   `g(x)` in the JavaScript translation.
 *
 * These operators enhance the readability and functional style of ReasonML
 * code, making it easier to follow the flow of data transformations.
 */
[@mel.scope "document"] [@mel.return nullable]
external get_element_by_id: string => option(Dom.element) = "getElementById";

[@mel.set]
external set_inner_html: (Dom.element, string) => unit = "innerHTML";

module Clipboard = {
  [@mel.scope ("navigator", "clipboard")]
  external write_text: string => Js.Promise.t(unit) = "writeText";
};

module Geolocation = {
  type coordinates;
  type position;
  type error;
  type options;

  [@mel.get]
  external coordinates: position => coordinates = "coords";

  [@mel.get]
  external latitude: coordinates => float = "latitude";

  [@mel.get]
  external longitude: coordinates => float = "longitude";

  [@mel.get]
  external accuracy: coordinates => float = "accuracy";

  [@mel.get]
  external altitude: coordinates => Js.Nullable.t(float) = "altitude";

  [@mel.get]
  external altitude_accuracy:
    coordinates => Js.Nullable.t(float) = "altitudeAccuracy";

  [@mel.get]
  external heading: coordinates => Js.Nullable.t(float) = "heading";

  [@mel.get]
  external speed: coordinates => Js.Nullable.t(float) = "speed";

  [@mel.get]
  external timestamp: position => float = "timestamp";

  [@mel.get]
  external error_code: error => int = "code";

  [@mel.get]
  external error_message: error => string = "message";

  [@mel.obj]
  external make_options: (
    ~enableHighAccuracy: bool=?,
    ~timeout: int=?,
    ~maximumAge: int=?,
    unit,
  ) => options = "";

  [@mel.scope ("navigator", "geolocation")]
  external get_current_position: (
    ~success: position => unit,
    ~error: (error => unit)=?,
    ~options: options=?,
    unit,
  ) => unit = "getCurrentPosition";
};

module Window = {
  type keyboard_event;

  [@mel.get]
  external key: keyboard_event => string = "key";

  [@mel.scope "window"]
  external add_keydown_listener:
    (string, keyboard_event => unit) => unit = "addEventListener";

  [@mel.scope "window"]
  external remove_keydown_listener:
    (string, keyboard_event => unit) => unit = "removeEventListener";
};
