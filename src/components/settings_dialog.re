[@react.component]
let make = (
    ~isSettingsDialogOpen,
    ~setSettingsDialogOpen,
    ~restoreSettingsButtonFocus,
) => {
    open Bindings;
    open Mui;
    open Store;

    let displayLanguage =
        app_store |> Zustand.use_store(store => store.display_language);
    let setDisplayLanguage =
        app_store |> Zustand.use_store(store => store.set_display_language);

    <Dialog 
        _open=isSettingsDialogOpen
        disableRestoreFocus=true
        maxWidth=MaxWidth.xs
        onTransitionExited=restoreSettingsButtonFocus
        onClose={(_, _) => {
            // TODO: add code to save in the settings in the database if user is logged in
            setSettingsDialogOpen(_ => false)
        }}
    >
        <DialogTitle>
            {Ui_translation.display_to(~sentence="settings", ~language=displayLanguage, ~size=Some(Ui_translation.Large))}
        </DialogTitle>
        <DialogContent>
            <FormControl>
                <FormLabel id="display-language-settings">
                    {
                        Ui_translation.display_to(
                            ~sentence="language_choice", 
                            ~language=displayLanguage, 
                            ~size=Some(Ui_translation.Medium)
                        )
                    }
                </FormLabel>
                <RadioGroup 
                    row=true 
                    ariaLabelledby="display-language-settings"
                    value=displayLanguage
                >
                    <FormControlLabel 
                        value=Ui_translation.English
                        control={<Radio />} 
                        label={"English" |> React.string} 
                        onChange={_ => setDisplayLanguage(Ui_translation.English)}
                    />
                    <FormControlLabel 
                        className="cuneiforms x-small"
                        disableTypography=true
                        value=Ui_translation.SuxCuneiform
                        control={<Radio />} 
                        label={{js|𒅴𒄀|js} |> React.string}
                        onChange={_ => setDisplayLanguage(Ui_translation.SuxCuneiform)}
                        sx={{"paddingRight": "16px"}}
                    />
                    <FormControlLabel 
                        value=Ui_translation.SuxLatin
                        control={<Radio />} 
                        label={{js|Emeĝir|js} |> React.string} 
                        onChange={_ => setDisplayLanguage(Ui_translation.SuxLatin)}
                    />
                </RadioGroup>
            </FormControl>
        </DialogContent>
            <DialogActions>
            <Button 
                onClick={_ => setSettingsDialogOpen(_ => false)}
            >
                {
                    Ui_translation.display_to(~sentence="close", ~language=displayLanguage, ~size=Some(Ui_translation.Small))
                }
            </Button>
            <Button 
                onClick={_ => setSettingsDialogOpen(_ => false)}
            >
                {
                    Ui_translation.display_to(~sentence="save", ~language=displayLanguage, ~size=Some(Ui_translation.Small))
                }
            </Button>
        </DialogActions>
    </Dialog>
}
