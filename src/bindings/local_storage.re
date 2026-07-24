/**
 * Typed bindings for the browser's localStorage API.
 *
 * localStorage stores string keys and values and persists them between browser
 * sessions for the same origin.
 */

/** Return Some(value) when the key exists, or None when it does not. */
[@mel.scope "localStorage"] [@mel.return nullable]
external get_item: string => option(string) = "getItem";

/** Store a string value under a key. */
[@mel.scope "localStorage"]
external set_item: (string, string) => unit = "setItem";

/** Remove every item stored by this application origin. */
[@mel.scope "localStorage"]
external clear: unit => unit = "clear";

/**
 * The value stored under the "keyboard" key:
 * {
 *   "word": ["𒀀", "𒁀"],
 *   "another-word": ["𒂊"]
 * }
 *
 * A Js.Dict is used instead of a Reason record because the keys are dynamic.
 */
type keyboard = Js.Dict.t(array(string));

/** Encode a keyboard dictionary as a JSON string suitable for localStorage. */
let encode_keyboard = (keyboard: keyboard): string =>
    switch (Js.Json.stringifyAny(keyboard)) {
    | Some(json) => json
    | None => "{}"
    };

let decode_string_array = (json: Js.Json.t): option(array(string)) =>
    switch (Js.Json.decodeArray(json)) {
    | None => None
    | Some(values) =>
        values
        |> Array.fold_left((decoded, value) =>
            switch (decoded, Js.Json.decodeString(value)) {
            | (Some(items), Some(item)) => Some([item, ...items])
            | _ => None
            }, Some([]))
        |> Option.map(items => items |> List.rev |> Array.of_list)
    };

/**
 * Decode and validate a keyboard JSON string.
 * Returns None for malformed JSON or for values that are not string arrays.
 */
let decode_keyboard = (value: string): option(keyboard) =>
    switch (value |> Js.Json.parseExn |> Js.Json.decodeObject) {
    | None => None
    | Some(object_) =>
        object_
        |> Js.Dict.entries
        |> Array.fold_left((decoded, (key, json)) =>
            switch (decoded, decode_string_array(json)) {
            | (Some(entries), Some(values)) =>
                Some([(key, values), ...entries])
            | _ => None
            }, Some([]))
        |> Option.map(entries =>
            entries
            |> List.rev
            |> Array.of_list
            |> Js.Dict.fromArray
        )
    };