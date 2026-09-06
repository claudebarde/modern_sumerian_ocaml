open Bindings;

type app_state = {
  display_language: Ui_translation.display_language,
  current_user: option(Supabase.Auth.user),
  is_auth_loading: bool,
  set_display_language: Ui_translation.display_language => unit,
  set_current_user: option(Supabase.Auth.user) => unit,
};

let app_store: Zustand.store(app_state) =
  Zustand.create_store((set, _get, _store) => {
    {
      display_language: Ui_translation.English,
      current_user: None,
      is_auth_loading: true,

      set_display_language: language =>
        Zustand.apply_update(set, state => {
          ...state,
          display_language: language,
        }),

      set_current_user: user =>
        Zustand.apply_update(set, state => {
          ...state,
          current_user: user,
        }),
    };
  });
