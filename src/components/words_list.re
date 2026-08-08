[@mel.module "../styles/WordsList.module.scss"] external css: Js.t({..}) = "default"; 
[@mel.module "../styles/Dictionary.module.scss"] external dictionary: Js.t({..}) = "default"; 

[@react.component]
let make = () => {
    open Bindings;
    open Mui;

    let (words_list, set_words_list) = React.useState(_ => LocalStorage.get_words_list());
    let (rowsPerPage, setRowsPerPage) = React.useState(_ => 8);
    let (page, setPage) = React.useState(_ => 0);
    let (open_snackbar, set_open_snackbar) = React.useState(_ => false);
    let (removed_word, set_removed_word) = React.useState(_ => None);

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

    <>
        <div className=css##wordsList>
            <h1>{"My Words List" |> React.string}</h1>
            {
                switch (words_list) {
                | None => 
                    <Typography variant=Typography.Variant.h6>
                        {"No words in the list." |> React.string}
                    </Typography>
                | Some(words) when Array.length(Js.Dict.keys(words)) === 0 =>
                    <Typography variant=Typography.Variant.h6>
                        {"No words in the list." |> React.string}
                    </Typography>
                | Some(words) =>
                    <TableContainer
                        className=dictionary##tableContainer
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
                                    Js.Dict.entries(words)
                                    |> Array.map(((english, (cuneiforms, sumerian, epsd_code))) =>
                                        <TableRow key=english>
                                            <TableCell sx={{"textAlign": "center"}}>
                                                <span className="cuneiforms small">
                                                    {cuneiforms |> React.string}
                                                </span>
                                            </TableCell>
                                            <TableCell sx={{"textAlign": "center"}}>
                                                {sumerian |> React.string}
                                            </TableCell>
                                            <TableCell sx={{"textAlign": "center"}}>
                                                {english |> React.string}
                                            </TableCell>
                                            <TableCell sx={{"textAlign": "center"}}>
                                                <IconButton
                                                    href={"https://oracc.museum.upenn.edu/epsd2/sux/" ++ epsd_code}
                                                    target="_blank"
                                                    rel="noopener noreferrer"
                                                    color=Color.primary
                                                >
                                                    <TablerReact.IconLink />
                                                </IconButton>
                                            </TableCell>
                                            <TableCell sx={{"textAlign": "center"}}>
                                                <IconButton
                                                    onClick={_ => {
                                                        set_words_list(_ =>
                                                            LocalStorage.remove_word(
                                                                ~english,
                                                            )
                                                        );
                                                        set_removed_word(_ => Some((english, sumerian)));
                                                        set_open_snackbar(_ => true);
                                                    }}
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
                            count={Array.length(Js.Dict.entries(words))}
                            rowsPerPage={rowsPerPage}
                            page={page}
                            onPageChange={handleChangePage}
                            onRowsPerPageChange={handleChangeRowsPerPage}
                        />
                    </TableContainer>
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
