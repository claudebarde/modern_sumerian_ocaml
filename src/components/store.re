open Bindings;

type app_state = {
  display_language: Ui_translation.display_language,
  current_user: option(Supabase.Auth.user),
  current_session: option(Supabase.Auth.session),
  is_auth_loading: bool,
  set_display_language: Ui_translation.display_language => unit,
  set_authentication: option(Supabase.Auth.session) => unit,
  set_auth_loading: bool => unit,
  clear_authentication: unit => unit,
};

let app_store: Zustand.store(app_state) =
  Zustand.create_store((set, _get, _store) => {
    {
      display_language: Ui_translation.English,
      current_user: None,
      current_session: None,
      is_auth_loading: true,

      set_display_language: language =>
        Zustand.apply_update(set, state => {
          ...state,
          display_language: language,
        }),

      set_authentication: session =>
        Zustand.apply_update(set, state => {
          ...state,
          current_user:
            switch session {
            | Some(session) => Some(Supabase.Auth.session_user(session))
            | None => None
            },
          current_session: session,
          is_auth_loading: false,
        }),

      set_auth_loading: is_loading =>
        Zustand.apply_update(set, state => {
          ...state,
          is_auth_loading: is_loading,
        }),

      clear_authentication: () =>
        Zustand.apply_update(set, state => {
          ...state,
          current_user: None,
          current_session: None,
          is_auth_loading: false,
        }),
    };
  });
