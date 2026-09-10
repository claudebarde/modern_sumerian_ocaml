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

/** A row returned from the user-owned words_list table. */
type words_list_row = {
  user_id: string,
  dictionary_entry_id: string,
  english: string,
  sumerian_cuneiform: string,
  sumerian_transliteration: string,
  created_at: string,
};

/** A row returned from the user-owned bookmarks table. */
type bookmark_row = {
  id: string,
  user_id: string,
  grammar_note_slug: string,
  grammar_note_title: string,
  selected_text: string,
  prefix_context: string,
  suffix_context: string,
  bookmark_type: int,
  created_at: string,
};

/** Binding for the named createClient export from @supabase/supabase-js. */
[@mel.module "@supabase/supabase-js"]
external createClient: (~supabase_url: string, ~supabase_key: string) => client = "createClient";

module Auth = {
    /** The Supabase Auth client available through `supabase.auth`. */
    type t;

    /** Opaque values returned by Supabase Auth. */
    type user;
    type session;
    type auth_error;
    type response('data);
    type auth_data;
    type session_data;
    type user_data;
    type sign_out_response;
    type subscription;
    type subscription_data;
    type subscription_response;

    /** Input objects used by the email/password authentication methods. */
    type sign_up_options;
    type sign_up_credentials;
    type sign_in_options;
    type sign_in_credentials;
    type sign_out_options;

    [@mel.get]
    external from_client: client => t = "auth";

    [@mel.obj]
    external make_sign_up_options: (
      ~emailRedirectTo: string=?,
      ~data: Js.Dict.t(Js.Json.t)=?,
      ~captchaToken: string=?,
      unit,
    ) => sign_up_options = "";

    [@mel.obj]
    external make_sign_up_credentials: (
      ~email: string,
      ~password: string,
      ~options: sign_up_options=?,
      unit,
    ) => sign_up_credentials = "";

    [@mel.obj]
    external make_sign_in_options: (
      ~captchaToken: string=?,
      unit,
    ) => sign_in_options = "";

    [@mel.obj]
    external make_sign_in_credentials: (
      ~email: string,
      ~password: string,
      ~options: sign_in_options=?,
      unit,
    ) => sign_in_credentials = "";

    [@mel.obj]
    external make_sign_out_options: (
      ~scope: [
        | `global
        | `local
        | `others
      ],
      unit,
    ) => sign_out_options = "";

    [@mel.send]
    external sign_up: (
      sign_up_credentials,
      [@mel.this] t,
    ) => Js.Promise.t(response(auth_data)) = "signUp";

    [@mel.send]
    external sign_in_with_password: (
      sign_in_credentials,
      [@mel.this] t,
    ) => Js.Promise.t(response(auth_data)) = "signInWithPassword";

    /** Sign out with Supabase's default global scope. */
    [@mel.send]
    external sign_out: (
      [@mel.this] t,
    ) => Js.Promise.t(sign_out_response) = "signOut";

    /** Pass `{scope: `local}` to sign out only the current browser session. */
    [@mel.send]
    external sign_out_with_options: (
      sign_out_options,
      [@mel.this] t,
    ) => Js.Promise.t(sign_out_response) = "signOut";

    [@mel.send]
    external get_session: (
      [@mel.this] t,
    ) => Js.Promise.t(response(session_data)) = "getSession";

    [@mel.send]
    external get_user: (
      [@mel.this] t,
    ) => Js.Promise.t(response(user_data)) = "getUser";

    /**
     * Subscribe immediately after creating the client and call `unsubscribe`
     * from the React effect cleanup function.
     */
    [@mel.send]
    external on_auth_state_change: (
      (string, Js.Nullable.t(session)) => unit,
      [@mel.this] t,
    ) => subscription_response = "onAuthStateChange";

    [@mel.get]
    external data: response('data) => 'data = "data";

    [@mel.get] [@mel.return nullable]
    external error: response('data) => option(auth_error) = "error";

    [@mel.get] [@mel.return nullable]
    external auth_user: auth_data => option(user) = "user";

    [@mel.get] [@mel.return nullable]
    external auth_session: auth_data => option(session) = "session";

    [@mel.get] [@mel.return nullable]
    external current_session: session_data => option(session) = "session";

    [@mel.get] [@mel.return nullable]
    external current_user: user_data => option(user) = "user";

    [@mel.get] [@mel.return nullable]
    external sign_out_error: sign_out_response => option(auth_error) = "error";

    [@mel.get]
    external subscription_data: subscription_response => subscription_data = "data";

    [@mel.get]
    external subscription: subscription_data => subscription = "subscription";

    [@mel.send]
    external unsubscribe: ([@mel.this] subscription) => unit = "unsubscribe";

    /** User fields needed by the account UI and user-owned database rows. */
    [@mel.get]
    external user_id: user => string = "id";

    [@mel.get] [@mel.return nullable]
    external user_email: user => option(string) = "email";

    [@mel.get]
    external user_created_at: user => string = "created_at";

    [@mel.get]
    external user_metadata: user => Js.Json.t = "user_metadata";

    /** Session fields useful for authentication state and authenticated calls. */
    [@mel.get]
    external session_user: session => user = "user";

    [@mel.get]
    external access_token: session => string = "access_token";

    [@mel.get]
    external refresh_token: session => string = "refresh_token";

    [@mel.get] [@mel.return nullable]
    external expires_at: session => option(float) = "expires_at";

    /** Stable AuthError fields suitable for displaying or logging failures. */
    [@mel.get]
    external error_message: auth_error => string = "message";

    [@mel.get] [@mel.return nullable]
    external error_status: auth_error => option(int) = "status";

    [@mel.get] [@mel.return nullable]
    external error_code: auth_error => option(string) = "code";
};

module Query = {
    /** An opaque PostgREST query builder returned by client.from(table). */
    type query_builder;

    /** A row inserted into the user-owned words_list table. */
    type words_list_insert;

    /** A row inserted into the user-owned bookmarks table. */
    type bookmark_insert;

    /** The response returned by a PostgREST mutation. */
    type mutation_response;
    type postgrest_error;

    /** Arguments shared by the English and Sumerian dictionary search RPCs. */
    type dictionary_search_params;

    /** Arguments for the protected Etsy listing lookup RPC. */
    type etsy_listing_params;

    [@mel.obj]
    external dictionary_search_params: (
      ~search_text: string,
      ~contains_match: bool,
      unit,
    ) => dictionary_search_params = "";

    [@mel.obj]
    external etsy_listing_params: (
      ~p_listing_id: string,
      unit,
    ) => etsy_listing_params = "";

    [@mel.obj]
    external make_words_list_insert: (
      ~user_id: string,
      ~dictionary_entry_id: string,
      ~english: string,
      ~sumerian_cuneiform: string,
      ~sumerian_transliteration: string,
      unit,
    ) => words_list_insert = "";

    [@mel.obj]
    external make_bookmark_insert: (
      ~user_id: string,
      ~grammar_note_slug: string,
      ~grammar_note_title: string,
      ~selected_text: string,
      ~prefix_context: string,
      ~suffix_context: string,
      ~bookmark_type: int,
      unit,
    ) => bookmark_insert = "";

    /** Start a query against a table or view. */
    [@mel.send]
    external from: (string, [@mel.this] client) => query_builder = "from";

    /** Execute a query and return the results. */
    [@mel.send]
    external select: (string, [@mel.this] query_builder) => Js.Promise.t(Js.Json.t) = "select";

    /** Insert one or more rows into words_list. */
    [@mel.send]
    external insert_words_list: (
      array(words_list_insert),
      [@mel.this] query_builder,
    ) => Js.Promise.t(mutation_response) = "insert";

    /** Insert one or more rows into bookmarks. */
    [@mel.send]
    external insert_bookmarks: (
      array(bookmark_insert),
      [@mel.this] query_builder,
    ) => Js.Promise.t(mutation_response) = "insert";

    /** Start a deletion from bookmarks. Filters must be applied before awaiting it. */
    [@mel.send]
    external delete_bookmarks: (
      [@mel.this] query_builder,
    ) => Js.Promise.t(mutation_response) = "delete";

    /** Apply an equality filter to a bookmarks mutation. */
    [@mel.send]
    external eq_bookmarks_mutation: (
      ~column: string,
      ~value: string,
      [@mel.this] Js.Promise.t(mutation_response),
    ) => Js.Promise.t(mutation_response) = "eq";

    /** Start a deletion from words_list. Filters must be applied before awaiting it. */
    [@mel.send]
    external delete_words_list: (
      [@mel.this] query_builder,
    ) => Js.Promise.t(mutation_response) = "delete";

    /** Apply an equality filter to a words_list mutation. */
    [@mel.send]
    external eq_words_list_mutation: (
      ~column: string,
      ~value: string,
      [@mel.this] Js.Promise.t(mutation_response),
    ) => Js.Promise.t(mutation_response) = "eq";

    [@mel.get] [@mel.return nullable]
    external mutation_error: mutation_response => option(postgrest_error) = "error";

    [@mel.get]
    external postgrest_error_message: postgrest_error => string = "message";

    /** Call a Supabase Postgres function. */
    [@mel.send]
    external rpc: (
      string,
      dictionary_search_params,
      [@mel.this] client,
    ) => Js.Promise.t(Js.Json.t) = "rpc";

    /** Call the protected Etsy listing lookup Postgres function. */
    [@mel.send]
    external rpc_etsy_listing: (
      string,
      etsy_listing_params,
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

    type words_list_response = {
      success: bool,
      data: array(words_list_row),
      error: option(string),
    };

    type bookmarks_response = {
      success: bool,
      data: array(bookmark_row),
      error: option(string),
    };

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

    let decode_words_list_row = json =>
      switch (Js.Json.decodeObject(json)) {
      | Some(obj) =>
        Some({
          user_id: decode_string_field(obj, "user_id"),
          dictionary_entry_id: decode_string_field(obj, "dictionary_entry_id"),
          english: decode_string_field(obj, "english"),
          sumerian_cuneiform: decode_string_field(obj, "sumerian_cuneiform"),
          sumerian_transliteration: decode_string_field(obj, "sumerian_transliteration"),
          created_at: decode_string_field(obj, "created_at"),
        })
      | None => None
      };

    let decode_words_list = (json: Js.Json.t): words_list_response =>
      switch (Js.Json.decodeObject(json)) {
      | Some(obj) => {
          let error = decode_error(obj);
          let data =
            switch (Js.Dict.get(obj, "data")) {
            | Some(value) =>
              switch (Js.Json.decodeArray(value)) {
              | Some(rows) =>
                rows
                |> Array.fold_left((decoded_rows, row) =>
                  switch (decode_words_list_row(row)) {
                  | Some(row) => [row, ...decoded_rows]
                  | None => decoded_rows
                  }, [])
                |> List.rev
                |> Array.of_list
              | None => [||]
              }
            | None => [||]
            };
          {
            success: error === None,
            data,
            error,
          };
        }
      | None => {
          success: false,
          data: [||],
          error: Some("Supabase returned an invalid words list response"),
        }
      };

    let decode_bookmark_row = json =>
      switch (Js.Json.decodeObject(json)) {
      | Some(obj) =>
        Some({
          id: decode_string_field(obj, "id"),
          user_id: decode_string_field(obj, "user_id"),
          grammar_note_slug: decode_string_field(obj, "grammar_note_slug"),
          grammar_note_title: decode_string_field(obj, "grammar_note_title"),
          selected_text: decode_string_field(obj, "selected_text"),
          prefix_context: decode_string_field(obj, "prefix_context"),
          suffix_context: decode_string_field(obj, "suffix_context"),
          bookmark_type:
            switch (Js.Dict.get(obj, "bookmark_type")) {
            | Some(value) =>
              switch (Js.Json.decodeNumber(value)) {
              | Some(number) => int_of_float(number)
              | None => 0
              }
            | None => 0
            },
          created_at: decode_string_field(obj, "created_at"),
        })
      | None => None
      };

    let decode_bookmarks = (json: Js.Json.t): bookmarks_response =>
      switch (Js.Json.decodeObject(json)) {
      | Some(obj) => {
          let error = decode_error(obj);
          let data =
            switch (Js.Dict.get(obj, "data")) {
            | Some(value) =>
              switch (Js.Json.decodeArray(value)) {
              | Some(rows) =>
                rows
                |> Array.fold_left((decoded_rows, row) =>
                  switch (decode_bookmark_row(row)) {
                  | Some(decoded_row) => [decoded_row, ...decoded_rows]
                  | None => decoded_rows
                  }, [])
                |> List.rev
                |> Array.of_list
              | None => [||]
              }
            | None => [||]
            };
          {success: error === None, data, error};
        }
      | None => {
          success: false,
          data: [||],
          error: Some("Supabase returned an invalid bookmarks response"),
        }
      };
}

let client =
  createClient(
    ~supabase_url=Config.supabaseUrl,
    ~supabase_key=Config.supabasePublishableKey,
  );

/** Shared Auth client used by account/session components. */
let auth = Auth.from_client(client);
