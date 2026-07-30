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
        "main": Config.colors##botanicalNight,
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

