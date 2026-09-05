[@react.component]
let make = (~isSignupDialogOpen, ~setSignupDialogOpen, ~isSignUp) => {
    open Bindings;
    open Mui;
    open Web_utils;
    open Store;

    let displayLanguage =
        app_store |> Zustand.use_store(store => store.display_language);

    let (showPassword, setShowPassword) = React.useState(_ => false);

    <Dialog 
        _open=isSignupDialogOpen
        maxWidth=MaxWidth.xs
        onClose={(_, _) => setSignupDialogOpen(_ => false)}
    >
        <DialogTitle>
            {
                Translation.display_to(
                    ~sentence=(isSignUp ? "sign_up" : "sign_in"),
                    ~language=displayLanguage,
                    ~size=Some(Translation.Large)
                )
            }
        </DialogTitle>
        <DialogContent>
            <DialogContentText sx={{"word-wrap": "break-word"}}>
                {isSignUp 
                    ? Translation.display_to(~sentence="sign_up_message", ~language=displayLanguage, ~size=Some(Translation.Medium))
                    : Translation.display_to(~sentence="sign_in_message", ~language=displayLanguage, ~size=Some(Translation.Medium))
                }
            </DialogContentText>
            <TextField
                autoFocus=true
                required=true
                margin=`dense
                label={Translation.display_to(~sentence="email_address", ~language=displayLanguage, ~size=Some(Translation.Small))}
                type_="email"
                fullWidth=true
                variant=`standard
            />
            <FormControl variant=`standard fullWidth=true sx={{"marginTop": "1rem"}}>
                <InputLabel htmlFor={"signing-password-input"}>
                    {
                        Translation.display_to(~sentence="password", ~language=displayLanguage, ~size=Some(Translation.Small))
                    }
                </InputLabel>
                <Input
                    id="signing-password-input"
                    type_={showPassword ? "text" : "password"}
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
                {Translation.display_to(~sentence="cancel", ~language=displayLanguage, ~size=Some(Translation.Small))}
            </Button>
            <Button>
                {Translation.display_to(~sentence=(isSignUp ? "sign_up" : "sign_in"), ~language=displayLanguage, ~size=Some(Translation.Small))}
            </Button>
        </DialogActions>
    </Dialog>
}