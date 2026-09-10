let is_valid_email: string => bool = [%mel.raw {|
    email => /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)
|}];

[@react.component]
let make = (
    ~isSignupDialogOpen,
    ~setSignupDialogOpen,
    ~isSignUp,
    ~setIsSignUp,
) => {
    open Bindings;
    open Mui;
    open Store;

    let displayLanguage =
        app_store |> Zustand.use_store(store => store.display_language);
    let setAuthentication =
        app_store |> Zustand.use_store(store => store.set_authentication);

    let (email_address, set_email_address) = React.useState(_ => None);
    let (password, set_password) = React.useState(_ => None);
    let (showPassword, setShowPassword) = React.useState(_ => false);
    let (authentication_error, set_authentication_error) = React.useState(_ => None);
    let (authentication_message, set_authentication_message) = React.useState(_ => None);
    let (is_authenticating, set_is_authenticating) = React.useState(_ => false);

    let handleSignUp = () => {
        let email =
            switch email_address {
            | Some(value) => value |> String.trim
            | None => ""
            };
        let password_value =
            switch password {
            | Some(value) => value
            | None => ""
            };

        set_authentication_error(_ => None);
        set_authentication_message(_ => None);

        if (email === "") {
            set_authentication_error(_ => Some("Enter your email address."));
        } else if (!is_valid_email(email)) {
            set_authentication_error(_ => Some("Enter a valid email address."));
        } else if (String.length(password_value) < 8) {
            set_authentication_error(_ => Some("Your password must contain at least 8 characters."));
        } else {
            set_is_authenticating(_ => true);

            let credentials =
                Supabase.Auth.make_sign_up_credentials(
                    ~email,
                    ~password=password_value,
                    (),
                );

            let _ =
                Supabase.auth
                |> Supabase.Auth.sign_up(credentials)
                |> Js.Promise.then_(response => {
                    set_is_authenticating(_ => false);

                    switch (Supabase.Auth.error(response)) {
                    | Some(error) =>
                        set_authentication_error(_ => Some(Supabase.Auth.error_message(error)))
                    | None =>
                        let auth_data = Supabase.Auth.data(response);

                        switch (Supabase.Auth.auth_session(auth_data)) {
                        | Some(session) => {
                            setAuthentication(Some(session));
                            set_authentication_message(_ => Some("Your account has been created and you are signed in."));
                        }
                        | None => {
                            setAuthentication(None);
                            set_authentication_message(_ => Some("Your account has been created. Check your email to confirm it."))
                        }
                        }
                    };

                    Js.Promise.resolve();
                })
                |> Js.Promise.catch(_error => {
                    set_is_authenticating(_ => false);
                    set_authentication_error(_ => Some("Unable to create your account. Check your connection and try again."));
                    Js.Promise.resolve();
                });
            ();
        };
    };

    let handleSignIn = () => {
        let email =
            switch email_address {
            | Some(value) => value |> String.trim
            | None => ""
            };
        let password_value =
            switch password {
            | Some(value) => value
            | None => ""
            };

        set_authentication_error(_ => None);
        set_authentication_message(_ => None);

        if (email === "") {
            set_authentication_error(_ => Some("Enter your email address."));
        } else if (!is_valid_email(email)) {
            set_authentication_error(_ => Some("Enter a valid email address."));
        } else if (password_value === "") {
            set_authentication_error(_ => Some("Enter your password."));
        } else {
            set_is_authenticating(_ => true);

            let credentials =
                Supabase.Auth.make_sign_in_credentials(
                    ~email,
                    ~password=password_value,
                    (),
                );

            let _ =
                Supabase.auth
                |> Supabase.Auth.sign_in_with_password(credentials)
                |> Js.Promise.then_(response => {
                    set_is_authenticating(_ => false);

                    switch (Supabase.Auth.error(response)) {
                    | Some(error) =>
                        set_authentication_error(_ => Some(Supabase.Auth.error_message(error)))
                    | None =>
                        let auth_data = Supabase.Auth.data(response);

                        switch (Supabase.Auth.auth_session(auth_data)) {
                        | Some(session) => {
                            setAuthentication(Some(session));
                            set_email_address(_ => None);
                            set_password(_ => None);
                            setSignupDialogOpen(_ => false);
                        }
                        | None =>
                            set_authentication_error(_ => Some("Unable to start your session. Please try again."))
                        }
                    };

                    Js.Promise.resolve();
                })
                |> Js.Promise.catch(_error => {
                    set_is_authenticating(_ => false);
                    set_authentication_error(_ => Some("Unable to sign in. Check your connection and try again."));
                    Js.Promise.resolve();
                });
            ();
        };
    };

    <Dialog 
        _open=isSignupDialogOpen
        maxWidth=MaxWidth.xs
        onClose={(_, _) => setSignupDialogOpen(_ => false)}
    >
        <DialogTitle>
            {
                Ui_translation.display_to(
                    ~sentence=(isSignUp ? "sign_up" : "sign_in"),
                    ~language=displayLanguage,
                    ~size=Some(Ui_translation.Large)
                )
            }
        </DialogTitle>
        <DialogContent>
            <DialogContentText sx={{"wordWrap": "break-word"}}>
                {isSignUp 
                    ? Ui_translation.display_to(~sentence="sign_up_message", ~language=displayLanguage, ~size=Some(Ui_translation.Medium))
                    : Ui_translation.display_to(~sentence="sign_in_message", ~language=displayLanguage, ~size=Some(Ui_translation.Medium))
                }
            </DialogContentText>
            <TextField
                autoFocus=true
                required=true
                margin=`dense
                label={Ui_translation.display_to(~sentence="email_address", ~language=displayLanguage, ~size=Some(Ui_translation.Small))}
                type_="email"
                fullWidth=true
                variant=`standard
                value={
                    switch email_address {
                    | Some(value) => value
                    | None => ""
                    }
                }
                onChange={event => {
                    set_email_address(_ => Some(React.Event.Form.target(event)##value));
                    set_authentication_error(_ => None);
                    set_authentication_message(_ => None);
                }}
            />
            <FormControl variant=`standard fullWidth=true sx={{"marginTop": "1rem"}}>
                <InputLabel htmlFor={"signing-password-input"}>
                    {
                        Ui_translation.display_to(~sentence="password", ~language=displayLanguage, ~size=Some(Ui_translation.Small))
                    }
                </InputLabel>
                <Input
                    id="signing-password-input"
                    type_={showPassword ? "text" : "password"}
                    value={
                        switch password {
                        | Some(value) => value
                        | None => ""
                        }
                    }
                    onChange={event => {
                        set_password(_ => Some(React.Event.Form.target(event)##value));
                        set_authentication_error(_ => None);
                        set_authentication_message(_ => None);
                    }}
                    endAdornment={
                        <InputAdornment position=`end_>
                            <IconButton
                                ariaLabel={
                                    showPassword ? "hide the password" : "display the password"
                                }
                                onClick={_ => setShowPassword(prev => !prev)}
                                edge=`end_
                            >
                                {showPassword ? <TablerReact.IconEye /> : <TablerReact.IconEyeClosed />}
                            </IconButton>
                        </InputAdornment>
                    }
                />
            </FormControl>
            {
                switch authentication_error {
                | Some(message) =>
                    <Alert severity=`error sx={{"marginTop": "1rem"}}>
                        {message |> React.string}
                    </Alert>
                | None => React.null
                }
            }
            {
                switch authentication_message {
                | Some(message) =>
                    <Alert severity=`success sx={{"marginTop": "1rem"}}>
                        {message |> React.string}
                    </Alert>
                | None => React.null
                }
            }
        </DialogContent>
        <DialogActions>
            <Button 
                onClick={_ => setSignupDialogOpen(_ => false)}
            >
                {Ui_translation.display_to(~sentence="cancel", ~language=displayLanguage, ~size=Some(Ui_translation.Small))}
            </Button>
            {
                switch authentication_message {
                    | None => {
                        <>
                            <Button
                                disabled=is_authenticating
                                onClick={_ => {
                                    set_authentication_error(_ => None);
                                    set_authentication_message(_ => None);
                                    setIsSignUp(current => !current);
                                }}
                            >
                                {
                                    Ui_translation.display_to(
                                        ~sentence=(isSignUp ? "sign_in" : "sign_up"),
                                        ~language=displayLanguage,
                                        ~size=Some(Ui_translation.Small),
                                    )
                                }
                            </Button>
                            <Button
                                disabled=is_authenticating
                                onClick={_ => {
                                    if (isSignUp) {
                                        handleSignUp()
                                    } else {
                                        handleSignIn()
                                    }
                                }}
                            >
                                {
                                    is_authenticating
                                        ? (isSignUp ? "Creating account..." : "Signing in...") |> React.string
                                        : Ui_translation.display_to(~sentence=(isSignUp ? "sign_up" : "sign_in"), ~language=displayLanguage, ~size=Some(Ui_translation.Small))
                                }
                            </Button>
                        </>
                    }
                    | Some(_) => {
                        <Button
                            onClick={_ => setSignupDialogOpen(_ => false)}
                        >
                            {
                                Ui_translation.display_to(~sentence="close", ~language=displayLanguage, ~size=Some(Ui_translation.Small))
                            }
                        </Button>
                    }
                }
            }
        </DialogActions>
    </Dialog>
}
