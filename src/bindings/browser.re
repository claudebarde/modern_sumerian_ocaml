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
  /**
   * Copy text with the modern Clipboard API when available, then fall back to
   * a temporary textarea for browsers or contexts that reject that API.
   */
  let write_text: string => Js.Promise.t(unit) = [%mel.raw {|
    text => {
      const copyWithTextarea = () => new Promise((resolve, reject) => {
        const textarea = document.createElement("textarea");
        textarea.value = text;
        textarea.setAttribute("readonly", "");
        textarea.style.position = "fixed";
        textarea.style.left = "-9999px";
        textarea.style.opacity = "0";
        document.body.appendChild(textarea);
        textarea.select();
        textarea.setSelectionRange(0, textarea.value.length);

        try {
          const copied = document.execCommand("copy");
          if (copied) {
            resolve();
          } else {
            reject(new Error("The browser rejected the copy command."));
          }
        } catch (error) {
          reject(error);
        } finally {
          textarea.remove();
        }
      });

      if (navigator.clipboard && typeof navigator.clipboard.writeText === "function") {
        return navigator.clipboard.writeText(text).catch(() => copyWithTextarea());
      }

      return copyWithTextarea();
    }
  |}];
};

module Fetch = {
  type response;
  type request_options;
  type headers;

  /** Perform a GET request using the browser's default fetch options. */
  [@mel.scope "window"]
  external get: string => Js.Promise.t(response) = "fetch";

  [@mel.obj]
  external make_request_options: (
    ~method_: [@mel.as "method"] string,
    ~headers: Js.Dict.t(string),
    ~body: string,
    unit,
  ) => request_options = "";

  [@mel.scope "window"]
  external request: (string, request_options) => Js.Promise.t(response) = "fetch";

  [@mel.get]
  external ok: response => bool = "ok";

  [@mel.get]
  external headers: response => headers = "headers";

  [@mel.send] [@mel.return nullable]
  external get_header: (string, [@mel.this] headers) => option(string) = "get";

  [@mel.send]
  external json: ([@mel.this] response) => Js.Promise.t(Js.Json.t) = "json";

  [@mel.send]
  external text: ([@mel.this] response) => Js.Promise.t(string) = "text";
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

module ResizeObserver = {
  type t;
  type entry;
  type contentRect;

  [@mel.new]
  external make: ((array(entry), t) => unit) => t = "ResizeObserver";

  [@mel.send]
  external observe: (t, Dom.element) => unit = "observe";

  [@mel.send]
  external disconnect: t => unit = "disconnect";

  [@mel.get]
  external content_rect: entry => contentRect = "contentRect";

  [@mel.get]
  external width: contentRect => float = "width";

  [@mel.get]
  external height: contentRect => float = "height";
};

/** Coordinates returned by Range.getBoundingClientRect/getClientRects.
    They are relative to the browser viewport. */
module DomRect = {
  type t = Dom.domRect;

  [@mel.get]
  external x: t => float = "x";

  [@mel.get]
  external y: t => float = "y";

  [@mel.get]
  external top: t => float = "top";

  [@mel.get]
  external right: t => float = "right";

  [@mel.get]
  external bottom: t => float = "bottom";

  [@mel.get]
  external left: t => float = "left";

  [@mel.get]
  external width: t => float = "width";

  [@mel.get]
  external height: t => float = "height";
};

module DomRectList = {
  type t;

  [@mel.get]
  external length: t => int = "length";

  [@mel.send] [@mel.return nullable]
  external item: (int, [@mel.this] t) => option(DomRect.t) = "item";
};

module Range = {
  type t;

  [@mel.get]
  external collapsed: t => bool = "collapsed";

  [@mel.get]
  external start_container: t => Dom.node = "startContainer";

  [@mel.get]
  external start_offset: t => int = "startOffset";

  [@mel.get]
  external end_container: t => Dom.node = "endContainer";

  [@mel.get]
  external end_offset: t => int = "endOffset";

  [@mel.get]
  external common_ancestor_container: t => Dom.node = "commonAncestorContainer";

  [@mel.send]
  external to_string: ([@mel.this] t) => string = "toString";

  /** One rectangle covering the complete selection. */
  [@mel.send]
  external get_bounding_client_rect: ([@mel.this] t) => DomRect.t = "getBoundingClientRect";

  /** One or more rectangles following the selected line boxes. */
  [@mel.send]
  external get_client_rects: ([@mel.this] t) => DomRectList.t = "getClientRects";

  [@mel.send]
  external clone_range: ([@mel.this] t) => t = "cloneRange";

  [@mel.send]
  external select_node_contents: (Dom.element, [@mel.this] t) => unit = "selectNodeContents";

  [@mel.send]
  external set_start: (Dom.node, int, [@mel.this] t) => unit = "setStart";

  [@mel.send]
  external set_end: (Dom.node, int, [@mel.this] t) => unit = "setEnd";
};

module Node = {
  [@mel.get] [@mel.return nullable]
  external value: Dom.node => option(string) = "nodeValue";
};

module TreeWalker = {
  type t;

  [@mel.send] [@mel.return nullable]
  external next_node: ([@mel.this] t) => option(Dom.node) = "nextNode";
};

module Highlight = {
  type t;

  [@mel.new] [@mel.variadic]
  external make: array(Range.t) => t = "Highlight";
};

module CssHighlights = {
  type registry;

  [@mel.scope "CSS"] [@mel.return nullable]
  external registry: option(registry) = "highlights";

  [@mel.send]
  external set: (string, Highlight.t, [@mel.this] registry) => unit = "set";

  [@mel.send]
  external delete: (string, [@mel.this] registry) => bool = "delete";
};

module Selection = {
  type t;

  [@mel.get]
  external is_collapsed: t => bool = "isCollapsed";

  [@mel.get]
  external range_count: t => int = "rangeCount";

  [@mel.get] [@mel.return nullable]
  external anchor_node: t => option(Dom.node) = "anchorNode";

  [@mel.get]
  external anchor_offset: t => int = "anchorOffset";

  [@mel.get] [@mel.return nullable]
  external focus_node: t => option(Dom.node) = "focusNode";

  [@mel.get]
  external focus_offset: t => int = "focusOffset";

  [@mel.send]
  external to_string: ([@mel.this] t) => string = "toString";

  [@mel.send]
  external get_range_at: (int, [@mel.this] t) => Range.t = "getRangeAt";

  [@mel.send]
  external contains_node: (Dom.node, bool, [@mel.this] t) => bool = "containsNode";

  [@mel.send]
  external remove_all_ranges: ([@mel.this] t) => unit = "removeAllRanges";
};

module Element = {
  /** Useful for ensuring that a selection belongs to the grammar-note container. */
  [@mel.send]
  external contains: (Dom.node, [@mel.this] Dom.element) => bool = "contains";

  [@mel.send]
  external blur: ([@mel.this] Dom.element) => unit = "blur";
};

module Document = {
  [@mel.scope "document"] [@mel.return nullable]
  external active_element: option(Dom.element) = "activeElement";

  /** The NodeFilter.SHOW_TEXT mask. */
  let show_text = 4;

  [@mel.scope "document"]
  external create_tree_walker: (Dom.element, int) => TreeWalker.t = "createTreeWalker";

  [@mel.scope "document"]
  external create_range: unit => Range.t = "createRange";
};

module Window = {
  type keyboard_event;

  [@mel.scope ("window", "location")]
  external location_origin: string = "origin";

  [@mel.scope ("window", "location")]
  external location_href: string = "href";

  /** Return the user's current document selection, when one exists. */
  [@mel.scope "window"] [@mel.return nullable]
  external get_selection: unit => option(Selection.t) = "getSelection";

  [@mel.scope "window"]
  external scroll_x: float = "scrollX";

  [@mel.scope "window"]
  external scroll_y: float = "scrollY";

  [@mel.get]
  external key: keyboard_event => string = "key";

  [@mel.scope "window"]
  external add_keydown_listener:
    (string, keyboard_event => unit) => unit = "addEventListener";

  [@mel.scope "window"]
  external remove_keydown_listener:
    (string, keyboard_event => unit) => unit = "removeEventListener";
};
