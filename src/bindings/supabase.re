/** An opaque Supabase client returned by createClient. */
type client;

/** 
 * A type to represent the values in the "marker" column of the dictionary table 
      A — directly attested Ancient Sumerian;
      E — modern semantic extension;
      N — native neologism;
      C — calque;
      L-Akk — Akkadian loan;
      L-Anc — loan from another ancient language;
      L-Mod — modern loan;
      X — experimental or uncertain.
*/
type word_marker = 
  | A
  | E
  | N
  | C
  | L_Akk
  | L_Anc
  | L_Mod
  | X;

/** A type to represent the values in a row of the dictionary table */
type dictionary_row = {
  id: string,
  marker: word_marker,
  headword: string,
  word: string,
  translation: string,
  part_of_speech: string,
  meanings: array(string),
  forms: array(string),
  cuneiforms: array(string),
  dc_title: string,
  icount: int,
};

/** Binding for the named createClient export from @supabase/supabase-js. */
[@mel.module "@supabase/supabase-js"]
external createClient: (~supabase_url: string, ~supabase_key: string) => client = "createClient";

module Query = {
    /** An opaque PostgREST query builder returned by client.from(table). */
    type query_builder;

    /** Arguments shared by the English and Sumerian dictionary search RPCs. */
    type dictionary_search_params;

    [@mel.obj]
    external dictionary_search_params: (
      ~search_text: string,
      ~contains_match: bool,
      unit,
    ) => dictionary_search_params = "";

    /** Start a query against a table or view. */
    [@mel.send]
    external from: (string, [@mel.this] client) => query_builder = "from";

    /** Execute a query and return the results. */
    [@mel.send]
    external select: (string, [@mel.this] query_builder) => Js.Promise.t(Js.Json.t) = "select";

    /** Call a Supabase Postgres function. */
    [@mel.send]
    external rpc: (
      string,
      dictionary_search_params,
      [@mel.this] client,
    ) => Js.Promise.t(Js.Json.t) = "rpc";
};

module Filter = {
    /** An opaque PostgREST filter returned by query_builder.filter(column, operator, value). */
    type filter;

    /** Filter a query by columns equal to a value */
    [@mel.send]
    external eq: (~column: string, ~value: string, [@mel.this] Js.Promise.t(Js.Json.t)) => Js.Promise.t(Js.Json.t) = "eq";

    /** Filter a query by columns that are like a value */
    [@mel.send]
    external like: (~column: string, ~value: string, [@mel.this] Js.Promise.t(Js.Json.t)) => Js.Promise.t(Js.Json.t) = "like";

    /** Filter a query by columns that match a value case-insensitively. */
    [@mel.send]
    external ilike: (~column: string, ~value: string, [@mel.this] Js.Promise.t(Js.Json.t)) => Js.Promise.t(Js.Json.t) = "ilike";

    /** Combine raw PostgREST filters with OR. Prefer ilike_any for user input. */
    [@mel.send]
    external or_: (~filters: string, [@mel.this] Js.Promise.t(Js.Json.t)) => Js.Promise.t(Js.Json.t) = "or";

    let quote_filter_value = value => {
        let escaped =
            value
            |> Js.String.replaceByRe(
                ~regexp=Js.Re.fromStringWithFlags("\\\\", ~flags="g"),
                ~replacement="\\\\",
            )
            |> Js.String.replaceByRe(
                ~regexp=Js.Re.fromStringWithFlags("\"", ~flags="g"),
                ~replacement="\\\"",
            );
        "\"" ++ escaped ++ "\"";
    };

    /** Match any of several values case-insensitively. */
    let ilike_any = (~column, ~values, ~contains, query) => {
        let filters =
            values
            |> Array.map(value => {
                let pattern = contains ? "%" ++ value ++ "%" : value;
                column ++ ".ilike." ++ quote_filter_value(pattern);
            })
            |> Js.Array.join(~sep=",");
        query |> or_(~filters);
    };

    /** Filters a query by columns that match any word that starts with the provided value case-insensitively. */
    let starts_with_any = (~column, ~values, query) => {
        let filters =
            values
            |> Array.map(value => {
                let pattern = value ++ "%";
                column ++ ".ilike." ++ quote_filter_value(pattern);
            })
            |> Js.Array.join(~sep=",");
        query |> or_(~filters);
    };
}

module Modifier = {
    /** An opaque PostgREST modifier returned by query_builder.modify(modifier). */
    type modifier;

    /** Modify a query with a custom modifier. */
    type order_options = {ascending: bool};
    [@mel.send]
    external order: (~column: string, ~options: option(order_options), [@mel.this] Js.Promise.t(Js.Json.t)) => Js.Promise.t(Js.Json.t) = "order";

    /** The "limit" modifier */
    [@mel.send]
    external limit: (~count: int, [@mel.this] Js.Promise.t(Js.Json.t)) => Js.Promise.t(Js.Json.t) = "limit";
};

module Response = {
    /** 
     * Decode a JSON response into an array of dictionary_row records. 
    */
    
    type response = {
      success: bool,
      data: array(dictionary_row),
      error: option(string)
    }

    let decode_string_field = (obj, key) =>
      switch (Js.Dict.get(obj, key)) {
      | Some(value) =>
        switch (value |> Js.Json.decodeString) {
        | Some(value) => value
        | None => ""
        }
      | None => ""
      };

    let decode_marker = (obj, key) =>
      switch (Js.Dict.get(obj, key)) {
      | Some(value) =>
        switch (value |> Js.Json.decodeString) {
        | Some("A") => A
        | Some("E") => E
        | Some("N") => N
        | Some("C") => C
        | Some("L-Akk") => L_Akk
        | Some("L-Anc") => L_Anc
        | Some("L-Mod") => L_Mod
        | Some("X") => X
        | _ => X
        }
      | None => X
      };

    let decode_string_array = json =>
      switch (json |> Js.Json.decodeArray) {
      | Some(values) =>
        values
        |> Array.map(value =>
             switch (value |> Js.Json.decodeString) {
             | Some(value) => value
             | None => ""
             }
           )
      | None => [||]
      };

    let decode_row = json =>
      switch (json |> Js.Json.decodeObject) {
      | Some(obj) => {
          id: decode_string_field(obj, "id"),
          marker: decode_marker(obj, "marker"),
          headword: decode_string_field(obj, "headword"),
          word: decode_string_field(obj, "word"),
          translation: decode_string_field(obj, "translation"),
          part_of_speech: decode_string_field(obj, "part_of_speech"),
          meanings:
            switch (Js.Dict.get(obj, "meanings")) {
            | Some(value) => decode_string_array(value)
            | None => [||]
            },
          forms:
            switch (Js.Dict.get(obj, "forms")) {
            | Some(value) => decode_string_array(value)
            | None => [||]
            },
          cuneiforms:
            switch (Js.Dict.get(obj, "cuneiforms")) {
            | Some(value) => decode_string_array(value)
            | None => [||]
            },
          dc_title: decode_string_field(obj, "dc_title"),
          icount:
            switch (Js.Dict.get(obj, "icount")) {
            | Some(value) =>
              switch (value |> Js.Json.decodeNumber) {
              | Some(num) => int_of_float(num)
              | None => 0
              }
            | None => 0
            },
        }
      | None => {
          id: "",
          marker: X,
          headword: "",
          word: "",
          translation: "",
          part_of_speech: "",
          meanings: [||],
          forms: [||],
          cuneiforms: [||],
          dc_title: "",
          icount: 0,
        }
      };

    let decode_error = obj =>
      switch (Js.Dict.get(obj, "error")) {
      | None => None
      | Some(value) =>
        switch (Js.Json.classify(value)) {
        | Js.Json.JSONNull => None
        | Js.Json.JSONString(message) => Some(message)
        | Js.Json.JSONObject(error_obj) =>
          switch (Js.Dict.get(error_obj, "message")) {
          | Some(message) => message |> Js.Json.decodeString
          | None => Some(Js.Json.stringify(value))
          }
        | _ => Some(Js.Json.stringify(value))
        }
      };

    let decode = (json: Js.Json.t): response =>
      switch (json |> Js.Json.decodeObject) {
      | Some(obj) => {
          let error = decode_error(obj);
          let data =
            switch (Js.Dict.get(obj, "data")) {
            | Some(value) =>
              switch (value |> Js.Json.decodeArray) {
              | Some(rows) => rows |> Array.map(decode_row)
              | None => [||]
              }
            | None => [||]
            };
          {success: error === None, data, error};
        }
      | None => {
          success: false,
          data: [||],
          error: Some("Supabase returned an invalid response"),
        }
      };
}

let client =
  createClient(
    ~supabase_url=Config.supabaseUrl,
    ~supabase_key=Config.supabasePublishableKey,
  );
