[%%mel.raw {|import "./styles/index.scss"|}];

module App = {
  [@react.component]
  let make = () => {
    <>
      <Header />
      <Body />
      <Footer />
    </>;
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

