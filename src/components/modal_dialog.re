// [@react.component]
// let make = (~children: React.element, ~is_open: bool) => {
//     // let modal_ref = React.useRef(Js.Nullable.null);
//     let modal_ref = React.createRef();

//     React.useEffect1(() => {
//         let _ = switch (Js.Nullable.toOption(modal_ref.current)) {
//         | None => ()
//         | Some(modal) => {
//             if (is_open) {
//                 modal##showModal();
//             } else {
//                 modal##close();
//             }
//         }
//         };

//         None
//     }, [|is_open|]);

//     <dialog ref={ReactDOM.Ref.domRef(modal_ref)}>
//         children
//     </dialog>
// }