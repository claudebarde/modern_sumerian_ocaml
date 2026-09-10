[%%mel.raw {|import "./styles/index.scss"|}];

open Bindings;
open Mui;

let theme =
  Theme.create({
    "typography": {
      "fontFamily": {js|"National Park", sans-serif|js},
    },
    "palette": {
      "primary": {
        "main": Config.colors##protonRed,
        "contrastText": Config.colors##whiteSmoke,
      },
      "secondary": {
        "main": Config.colors##darkRift,
        "contrastText": Config.colors##whiteSmoke,
      },
      "background": {
        "default": Config.colors##whiteSmoke,
        "paper": "white",
      },
      "text": {
        "primary": Config.colors##darkRift,
        "secondary": Config.colors##botanicalNight,
      },
    },
  });

module App = {
  [@react.component]
  let make = () => {
    open Components.Store;

    let setAuthentication =
      app_store |> Zustand.use_store(store => store.set_authentication);
    let setAuthLoading =
      app_store |> Zustand.use_store(store => store.set_auth_loading);

    React.useEffect0(() => {
      setAuthLoading(true);

      Supabase.auth
      |> Supabase.Auth.get_session
      |> Js.Promise.then_(response => {
        switch (Supabase.Auth.error(response)) {
        | Some(error) => {
            Js.log2(
              "Unable to restore the Supabase session:",
              Supabase.Auth.error_message(error),
            );
            setAuthentication(None);
          }
        | None =>
          response
          |> Supabase.Auth.data
          |> Supabase.Auth.current_session
          |> setAuthentication
        };

        Js.Promise.resolve();
      })
      |> Js.Promise.catch(_error => {
        setAuthentication(None);
        Js.Promise.resolve();
      })
      |> ignore;

      None;
    });

    <ThemeProvider theme={theme}>
      <CssBaseline />
      <Header />
      <Body />
      <Footer />
    </ThemeProvider>;
  };
};

ReactDOM.querySelector("#root")
->(
    fun
    | Some(root_elem) => {
        let root = ReactDOM.Client.createRoot(root_elem);
        ReactDOM.Client.render(root, <App />);
      }
    | None =>
      Js.Console.error(
        "Failed to start React: couldn't find the #root element",
      )
  );
