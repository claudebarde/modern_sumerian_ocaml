[@mel.module "../styles/WordsList.module.scss"] external css: Js.t({..}) = "default"; 
[@mel.module "../styles/Dictionary.module.scss"] external dictionary: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    open Bindings;
    open Mui;
    open Store;

    let current_user =
        app_store |> Zustand.use_store(state => state.current_user);
    let is_auth_loading =
        app_store |> Zustand.use_store(state => state.is_auth_loading);
    let (words_list, set_words_list) =
        React.useState(_ => (None: option(array(Supabase.words_list_row))));
    let (rowsPerPage, setRowsPerPage) = React.useState(_ => 8);
    let (page, setPage) = React.useState(_ => 0);
    let (open_snackbar, set_open_snackbar) = React.useState(_ => false);
    let (removed_word, set_removed_word) = React.useState(_ => None);
    let (deleting_word_id, set_deleting_word_id) = React.useState(_ => None);

    React.useEffect2(() => {
        if (is_auth_loading) {
            set_words_list(_ => None);
        } else {
            switch current_user {
            | None => set_words_list(_ => Some([||]))
            | Some(user) => {
                set_words_list(_ => None);

                Supabase.client
                |> Supabase.Query.from("words_list")
                |> Supabase.Query.select(
                    "user_id,dictionary_entry_id,english,sumerian_cuneiform,sumerian_transliteration,created_at",
                )
                |> Supabase.Filter.eq(
                    ~column="user_id",
                    ~value=Supabase.Auth.user_id(user),
                )
                |> Js.Promise.then_(response => {
                    let decoded = Supabase.Response.decode_words_list(response);

                    if (decoded.success) {
                        set_words_list(_ => Some(decoded.data));
                    } else {
                        switch decoded.error {
                        | Some(error) => Js.log2("Unable to fetch the words list:", error)
                        | None => ()
                        };
                        set_words_list(_ => Some([||]));
                    };

                    Js.Promise.resolve();
                })
                |> Js.Promise.catch(error => {
                    Js.log2("Unable to fetch the words list:", error);
                    set_words_list(_ => Some([||]));
                    Js.Promise.resolve();
                })
                |> ignore;
            }
            };
        };

        None;
    }, (current_user, is_auth_loading));

    let is_mobile = UseMediaQuery.use("(max-width:599px)");

    let handleChangePage = (_event, newPage) => {
        setPage(_ => newPage);
    };

    let handleChangeRowsPerPage = event => {
        setRowsPerPage(_ =>
            event
            |> React.Event.Form.target
            |> target => target##value
        );
        setPage(_ => 0);
    };

    let delete_word = (word: Supabase.words_list_row) => {
        switch current_user {
        | None => Js.log("The user must be signed in to remove a saved word.")
        | Some(user) => {
            set_deleting_word_id(_ => Some(word.dictionary_entry_id));

            Supabase.client
            |> Supabase.Query.from("words_list")
            |> Supabase.Query.delete_words_list
            |> Supabase.Query.eq_words_list_mutation(
                ~column="user_id",
                ~value=Supabase.Auth.user_id(user),
            )
            |> Supabase.Query.eq_words_list_mutation(
                ~column="dictionary_entry_id",
                ~value=word.dictionary_entry_id,
            )
            |> Js.Promise.then_(response => {
                set_deleting_word_id(_ => None);

                switch (Supabase.Query.mutation_error(response)) {
                | Some(error) =>
                    Js.log2(
                        "Unable to remove the word:",
                        Supabase.Query.postgrest_error_message(error),
                    )
                | None => {
                    set_words_list(current_words =>
                        current_words
                        |> Option.map(words =>
                            words
                            |> Js.Array.filter(~f=(saved_word: Supabase.words_list_row) =>
                                saved_word.dictionary_entry_id !== word.dictionary_entry_id
                            )
                        )
                    );
                    set_removed_word(_ => Some((word.english, word.sumerian_transliteration)));
                    set_open_snackbar(_ => true);
                }
                };

                Js.Promise.resolve();
            })
            |> Js.Promise.catch(error => {
                set_deleting_word_id(_ => None);
                Js.log2("Unable to remove the word:", error);
                Js.Promise.resolve();
            })
            |> ignore;
        }
        };
    };

    <>
        <div className=css##wordsList>
            <h1>{"My Words List" |> React.string}</h1>
            {
                switch (words_list) {
                | None => 
                    <Typography variant=Typography.Variant.h6>
                        {"No words in the list." |> React.string}
                    </Typography>
                | Some(words) when Array.length(words) === 0 =>
                    <Typography variant=Typography.Variant.h6>
                        {"No words in the list." |> React.string}
                    </Typography>
                | Some(words) =>
                    <>
                    // DESKTOP VIEW
                        <TableContainer
                            className=css##tableContainer
                            component=RootComponent.reactComponent(Paper.make)
                            sx={{"width": "60%"}}
                        >
                            <div className=dictionary##tableScroll>
                                <Table 
                                    stickyHeader=true 
                                    className=dictionary##resultsList
                                    size=`small
                                >
                                    <TableHead>
                                        <TableRow>
                                            <TableCell sx={{"textAlign": "center"}}>{"Cuneiforms" |> React.string}</TableCell>
                                            <TableCell sx={{"textAlign": "center"}}>{"Word" |> React.string}</TableCell>
                                            <TableCell sx={{"textAlign": "center"}}>{"Translation" |> React.string}</TableCell>
                                            <TableCell sx={{"textAlign": "center"}}>{"EPSD2 Link" |> React.string}</TableCell>
                                            <TableCell sx={{"textAlign": "center"}}>{"Actions" |> React.string}</TableCell>
                                        </TableRow>
                                    </TableHead>
                                    <TableBody>
                                    {
                                        words
                                        |> Array.map((word: Supabase.words_list_row) =>
                                            <TableRow key=word.dictionary_entry_id>
                                                <TableCell sx={{"textAlign": "center"}}>
                                                    <span className="cuneiforms small">
                                                        {word.sumerian_cuneiform |> React.string}
                                                    </span>
                                                </TableCell>
                                                <TableCell sx={{"textAlign": "center"}}>
                                                    {word.sumerian_transliteration |> React.string}
                                                </TableCell>
                                                <TableCell sx={{"textAlign": "center"}}>
                                                    {word.english |> React.string}
                                                </TableCell>
                                                <TableCell sx={{"textAlign": "center"}}>
                                                    <IconButton
                                                        href={"https://oracc.museum.upenn.edu/epsd2/sux/" ++ word.dictionary_entry_id}
                                                        target="_blank"
                                                        rel="noopener noreferrer"
                                                        color=Color.primary
                                                    >
                                                        <TablerReact.IconLink />
                                                    </IconButton>
                                                </TableCell>
                                                <TableCell sx={{"textAlign": "center"}}>
                                                    <IconButton
                                                        disabled={deleting_word_id === Some(word.dictionary_entry_id)}
                                                        onClick={_ => delete_word(word)}
                                                        color=Color.primary
                                                    >
                                                        <TablerReact.IconTrashFilled />
                                                    </IconButton>
                                                </TableCell>
                                            </TableRow>
                                            )
                                        |> React.array
                                    }
                                    </TableBody>
                                </Table>
                            </div>
                            <TablePagination
                                className=dictionary##pagination
                                rowsPerPageOptions={[|8, 12, 20|]}
                                component={RootComponent.htmlElement("div")}
                                count={Array.length(words)}
                                rowsPerPage={rowsPerPage}
                                page={page}
                                onPageChange={handleChangePage}
                                onRowsPerPageChange={handleChangeRowsPerPage}
                            />
                        </TableContainer>
                    // MOBILE VIEW
                        {
                            is_mobile
                            ? <List sx={{"width": "100%"}}>
                                {
                                    words
                                    |> Array.map((word: Supabase.words_list_row) =>
                                        <React.Fragment key=word.dictionary_entry_id>
                                            <ListItem>
                                                <ListItemAvatar>
                                                    <span className="cuneiforms small" style=ReactDOM.Style.make(~margin="0", ())>
                                                        {word.sumerian_cuneiform |> React.string}
                                                    </span>
                                                </ListItemAvatar>
                                                <ListItemText
                                                    primary={word.sumerian_transliteration |> React.string}
                                                    secondary={word.english |> React.string}
                                                    sx={{"marginLeft": "16px"}}
                                                />
                                                <ListItemSecondaryAction>
                                                    <IconButton
                                                        href={"https://oracc.museum.upenn.edu/epsd2/sux/" ++ word.dictionary_entry_id}
                                                        target="_blank"
                                                        rel="noopener noreferrer"
                                                        color=Color.primary
                                                    >
                                                        <TablerReact.IconLink />
                                                    </IconButton>
                                                    <IconButton
                                                        disabled={deleting_word_id === Some(word.dictionary_entry_id)}
                                                        onClick={_ => delete_word(word)}
                                                        color=Color.primary
                                                    >
                                                        <TablerReact.IconTrashFilled />
                                                    </IconButton>
                                                </ListItemSecondaryAction>
                                            </ListItem>
                                            <Divider />
                                        </React.Fragment>
                                        )
                                    |> React.array
                                }
                            </List>
                            : React.null
                        }
                    </>
                }
            }
        </div>
        <Snackbar
            _open={open_snackbar}
            anchorOrigin={{
                vertical: `bottom,
                horizontal: `right,
            }}
            autoHideDuration={3000}
            onClose={_ => set_open_snackbar(_ => false)}
        >
            <Alert
                severity=`success
                variant=`filled
                sx={{ "width": "100%" }}
            >
                {
                    switch removed_word {
                    | Some((english, sumerian)) => 
                        {"Removed \"" 
                        ++ (english |> Web_utils.Format.from_phonetic_to_standard) 
                        ++ "\" (" ++ sumerian ++ ") from my words list!" |> React.string}
                    | None => React.null
                    }
                }
            </Alert>
        </Snackbar>
    </>
};
