[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 

type category = Words | Cuneiform;

[@react.component]
let make = (~entries, ~category) => {
    open Bindings;
    open Mui;

    let (_test_data, _set_test_data) = React.useState(_ => entries);
    // let (turn, set_turn) = React.useState(_ => 1);
    // let (right_answers, set_right_answers) = React.useState(_ => [||]);
    // let (wrong_answers, set_wrong_answers) = React.useState(_ => [||]);

    <Typography 
        variant=Typography.Variant.subtitle1
    >
        {
            switch category {
                | Words => "Translate the word below in English"
                | Cuneiform => "Translate the cuneiform below in English"
            } |> React.string
        }
    </Typography>
}
