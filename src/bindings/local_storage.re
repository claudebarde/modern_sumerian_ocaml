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

/**
 * The value stored under the "location" key.
 *
 * The coordinate tuple is encoded in JSON as a two-item array:
 * {
 *   "city": "ur",
 *   "cuneiforms": "𒋀𒀊𒆠",
 *   "lat_long": [30.963056, 46.103056]
 * }
 */
type location = {
    city: string,
    cuneiforms: string,
    lat_long: (float, float),
};

let encode_location = (location: location): string =>
    switch (Js.Json.stringifyAny(location)) {
    | Some(json) => json
    | None => "{}"
    };

let decode_lat_long = (json: Js.Json.t): option((float, float)) =>
    switch (Js.Json.decodeArray(json)) {
    | Some(values) when Array.length(values) === 2 =>
        switch (
            Js.Json.decodeNumber(Array.get(values, 0)),
            Js.Json.decodeNumber(Array.get(values, 1)),
        ) {
        | (Some(latitude), Some(longitude)) =>
            Some((latitude, longitude))
        | _ => None
        }
    | _ => None
    };

let decode_object_field = (object_, key, decode) =>
    switch (Js.Dict.get(object_, key)) {
    | Some(json) => decode(json)
    | None => None
    };

/** Decode and validate a value read from the "location" key. */
let decode_location = (value: string): option(location) =>
    try (
        switch (value |> Js.Json.parseExn |> Js.Json.decodeObject) {
        | Some(object_) =>
            switch (
                decode_object_field(
                    object_,
                    "city",
                    Js.Json.decodeString,
                ),
                decode_object_field(
                    object_,
                    "cuneiforms",
                    Js.Json.decodeString,
                ),
                decode_object_field(
                    object_,
                    "lat_long",
                    decode_lat_long,
                ),
            ) {
            | (Some(city), Some(cuneiforms), Some(lat_long)) =>
                Some({city, cuneiforms, lat_long})
            | _ => None
            }
        | None => None
        }
    ) {
    | _ => None
    };

/** Return the decoded location stored under "location", when valid. */
let get_location = (): option(location) =>
    switch (get_item("location")) {
    | Some(value) => decode_location(value)
    | None => None
    };

/** Encode and store a location under the fixed "location" key. */
let set_location = (location: location): unit =>
    set_item("location", encode_location(location));

/**
 * One entry in the value stored under the "words_list" key:
 *
 * (
 *   cuneiform signs,
 *   Sumerian transliteration,
 *   EPSD entry code,
 * )
 */
type word_entry = (string, string, string);

/**
 * The English words are dynamic object keys, so the complete list is a
 * JavaScript dictionary. Its JSON representation looks like this:
 * {
 *   "king": ["𒈗", "lugal", "o0026174"]
 * }
 */
type words_list = Js.Dict.t(word_entry);

let encode_words_list = (words: words_list): string =>
    switch (Js.Json.stringifyAny(words)) {
    | Some(json) => json
    | None => "{}"
    };

let decode_word_entry = (json: Js.Json.t): option(word_entry) =>
    switch (Js.Json.decodeArray(json)) {
    | Some(values) when Array.length(values) === 3 =>
        switch (
            Js.Json.decodeString(Array.get(values, 0)),
            Js.Json.decodeString(Array.get(values, 1)),
            Js.Json.decodeString(Array.get(values, 2)),
        ) {
        | (Some(cuneiforms), Some(sumerian), Some(epsd_code)) =>
            Some((cuneiforms, sumerian, epsd_code))
        | _ => None
        }
    | _ => None
    };

/** Decode and validate every entry stored in a words-list JSON object. */
let decode_words_list = (value: string): option(words_list) =>
    try (
        switch (value |> Js.Json.parseExn |> Js.Json.decodeObject) {
        | Some(object_) =>
            object_
            |> Js.Dict.entries
            |> Array.fold_left((decoded, (english_word, json)) =>
                switch (decoded, decode_word_entry(json)) {
                | (Some(entries), Some(entry)) =>
                    Some([(english_word, entry), ...entries])
                | _ => None
                }, Some([]))
            |> Option.map(entries =>
                entries
                |> List.rev
                |> Array.of_list
                |> Js.Dict.fromArray
            )
        | None => None
        }
    ) {
    | _ => None
    };

/** Return the decoded words list stored under "words_list", when valid. */
let get_words_list = (): option(words_list) =>
    switch (get_item("words_list")) {
    | Some(value) => decode_words_list(value)
    | None => None
    };

/** Encode and store the complete words list under the fixed "words_list" key. */
let set_words_list = (words: words_list): unit =>
    set_item("words_list", encode_words_list(words));

/** Add or replace one entry, creating the stored dictionary when necessary. */
let add_word = (
    ~english: string,
    ~cuneiforms: string,
    ~sumerian: string,
    ~epsd_code: string,
): unit => {
    let words =
        switch (get_words_list()) {
        | Some(words) => words
        | None => Js.Dict.empty()
        };
    Js.Dict.set(
        words,
        english,
        (cuneiforms, sumerian, epsd_code),
    );
    set_words_list(words);
};

/**
 * Remove an English-word entry and persist the updated dictionary.
 * Returns the updated dictionary for use as React state, or None when no valid
 * words list was stored.
 */
let remove_word = (~english: string): option(words_list) =>
    switch (get_words_list()) {
    | Some(words) => {
        let updated_words =
            words
            |> Js.Dict.entries
            |> Array.to_list
            |> List.filter(((word, _entry)) => word !== english)
            |> Array.of_list
            |> Js.Dict.fromArray;
        set_words_list(updated_words);
        Some(updated_words);
    }
    | None => None
    };

/** Ensure that the "words_list" key exists, without replacing valid data. */
let initialize_words_list = (): unit =>
    switch (get_words_list()) {
    | Some(_) => ()
    | None => set_words_list(Js.Dict.empty())
    };
