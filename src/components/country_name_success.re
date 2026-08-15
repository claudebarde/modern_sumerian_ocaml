[@mel.module "../styles/WorldMap.module.scss"] external css: Js.t({..}) = "default";

[@react.component]
let make = () => {
  open Bindings;
  open Mui;

  <main className=css##countryNameSuccess>
    <h1>{"Thank you for your contribution!" |> React.string}</h1>
    <p>
      {"Your country name suggestion has been submitted successfully."
       |> React.string}
    </p>
    <Button
      component=RootComponent.htmlElement("a")
      href="/worldmap"
      variant=`contained
      color=Color.primary
      endIcon={<TablerReact.IconWorldMap />}
    >
      {"Return to World Map" |> React.string}
    </Button>
    <Button
      component=RootComponent.htmlElement("a")
      href="/"
      variant=`contained
      color=Color.primary
      endIcon={<TablerReact.IconHome />}
    >
      {"Return to Home" |> React.string}
    </Button>
  </main>;
};
