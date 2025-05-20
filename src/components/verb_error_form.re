type errorType = 
    | VerbForm
    | Cuneiform;

[@react.component]
let make = (~verb: option(Conjugator.t)) => {
    let (username, set_username) = React.useState(_ => None);
    let (email, set_email) = React.useState(_ => None);
    let (error_type, set_error_type) = React.useState(_ => None);
    let (message, set_message) = React.useState(_ => None);

    React.cloneElement(
        <form name="contact" method="POST">
            <h1>{"Report an error" |> React.string}</h1>
            <p>
                <label>
                    {"Your Name: " |> React.string}
                    <input 
                        type_="text" 
                        name="name" 
                        value={switch username {
                            | Some(name) => name
                            | None => ""
                        }}
                        onChange={ev => {
                            let target = React.Event.Form.target(ev)
                            let value: string = target##value
                            set_username(_ => Some(value))
                        }}
                    />
                </label>
            </p>
            <p>
                <label>
                    {"Your Email: " |> React.string}
                    <input 
                        type_="email" 
                        name="email" 
                        value={switch email {
                            | Some(email) => email
                            | None => ""
                        }}
                        onChange={ev => {
                            let target = React.Event.Form.target(ev)
                            let value: string = target##value
                            set_email(_ => Some(value))
                        }}
                    />
                </label>
            </p>
            <p>
                {"Error on: " |> React.string}
                <span style={ReactDOM.Style.make(~textDecoration="underline", ~marginLeft="10px", ())}>
                {
                    switch (verb) {
                        | Some(verb_form) => {
                            switch (Conjugator.print(verb_form, None)) {
                                | Ok({ verb, _ }) => { "`" ++ verb ++ "`" |> React.string }
                                | Error(_) => React.null
                            }
                        }
                        | None => React.null
                    }
                }
                </span>
            </p>
            <p>
                <span>{"Error with:" |> React.string}</span>
                <label>
                    <input 
                        type_="radio" 
                        name="error-type" 
                        value="verb-form" 
                        checked={switch error_type {
                            | Some(VerbForm) => true
                            | Some(Cuneiform) => false
                            | None => false
                        }}
                        onChange={ev => {
                            let target = React.Event.Form.target(ev)
                            let checked: bool = target##checked
                            set_error_type(_ => if (checked) { Some(VerbForm) } else { None })
                        }}
                    />
                    {"Verb form" |> React.string}
                </label>
                <label>
                    <input 
                        type_="radio" 
                        name="error-type" 
                        value="cuneiform" 
                        checked={switch error_type {
                            | Some(VerbForm) => false
                            | Some(Cuneiform) => true
                            | None => false
                        }}
                        onChange={ev => {
                            let target = React.Event.Form.target(ev)
                            let checked: bool = target##checked
                            set_error_type(_ => if (checked) { Some(Cuneiform) } else { None })
                        }}
                    />
                    {"Cuneiforms" |> React.string}
                </label>
            </p>
            <p>
                <label>
                    {"Additional details: " |> React.string}
                    <textarea 
                        name="message" 
                        maxLength=300
                        value={switch message {
                            | Some(msg) => msg
                            | None => ""
                        }}
                        onChange={ev => {
                            let target = React.Event.Form.target(ev)
                            let value: string = target##value
                            set_message(_ => Some(value))
                        }}
                    ></textarea>
                </label>
            </p>
            <p>
                <button 
                    type_="submit"
                    disabled={
                        username |> Option.is_none 
                        || email |> Option.is_none 
                        || error_type |> Option.is_none
                        || verb |> Option.is_none
                    }
                >
                    {"Send" |> React.string}
                </button>
            </p>
        </form>,
        {"data-netlify": "true"}
    )
}