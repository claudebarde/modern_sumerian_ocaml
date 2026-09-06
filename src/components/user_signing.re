[@react.component]
let make = (~isSignupDialogOpen, ~setSignupDialogOpen, ~isSignUp) => {
    open Bindings;
    open Mui;
    open Store;

    let displayLanguage =
        app_store |> Zustand.use_store(store => store.display_language);

    let (email_address, set_email_address) = React.useState(_ => None);
    let (password, set_password) = React.useState(_ => None);
    let (showPassword, setShowPassword) = React.useState(_ => false);

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
                onChange={event => set_email_address(_ => Some(React.Event.Form.target(event)##value))}
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
                    onChange={event => set_password(_ => Some(React.Event.Form.target(event)##value))}
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
        </DialogContent>
        <DialogActions>
            <Button 
                onClick={_ => setSignupDialogOpen(_ => false)}
            >
                {Ui_translation.display_to(~sentence="cancel", ~language=displayLanguage, ~size=Some(Ui_translation.Small))}
            </Button>
            <Button>
                {Ui_translation.display_to(~sentence=(isSignUp ? "sign_up" : "sign_in"), ~language=displayLanguage, ~size=Some(Ui_translation.Small))}
            </Button>
        </DialogActions>
    </Dialog>
}
