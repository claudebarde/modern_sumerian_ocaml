[@mel.module "../styles/Learn.module.scss"] external css: Js.t({..}) = "default"; 
external dom_element_from_event_target: Js.t({..}) => Dom.element = "%identity";

module ScrollableElement = {
    type scroll_to_options;

    [@mel.obj]
    external make_scroll_to_options:
        (~top: float, ~behavior: string, unit) => scroll_to_options = "";

    [@mel.get]
    external scroll_top: Dom.element => float = "scrollTop";

    [@mel.get]
    external scroll_height: Dom.element => float = "scrollHeight";

    [@mel.get]
    external client_height: Dom.element => float = "clientHeight";

    [@mel.send]
    external scroll_to:
        (scroll_to_options, [@mel.this] Dom.element) => unit = "scrollTo";
};

module ElementViewport = {
    type rect;

    [@mel.send]
    external get_bounding_client_rect: Dom.element => rect = "getBoundingClientRect";

    [@mel.get]
    external top: rect => float = "top";

    [@mel.get]
    external bottom: rect => float = "bottom";
};

type grammar_note = {
    slug: string,
    title: string,
    file: string,
    banner: string,
    category: string,
    visible: bool,
};

type bookmark_notification =
    | BookmarkSaved
    | NoBookmarkTextSelected
    | BookmarkSaveFailed
    | BookmarkRemoved
    | BookmarkRemoveFailed
    | BookmarkAuthenticationRequired;

type rendered_text_segment = {
    node: Dom.node,
    start_offset: int,
    end_offset: int,
};

type bookmark_marker = {
    id: string,
    bookmark_type: int,
    top: float,
};

let decode_string_field = (object_, field) =>
    switch (Js.Dict.get(object_, field)) {
    | Some(value) => Js.Json.decodeString(value)
    | None => None
    };

let decode_boolean_field = (object_, field) =>
    switch (Js.Dict.get(object_, field)) {
    | Some(value) => Js.Json.decodeBoolean(value)
    | None => None
    };

let decode_grammar_note = json =>
    switch (Js.Json.decodeObject(json)) {
    | Some(object_) =>
        switch (
            decode_string_field(object_, "slug"),
            decode_string_field(object_, "title"),
            decode_string_field(object_, "file"),
            decode_string_field(object_, "banner"),
            decode_string_field(object_, "category"),
            decode_boolean_field(object_, "visible"),
        ) {
        | (Some(slug), Some(title), Some(file), Some(banner), Some(category), Some(visible)) =>
            Some({slug, title, file, banner, category, visible})
        | _ => None
        }
    | None => None
    };

let parse_grammar_notes_index = json =>
    switch (Js.Json.decodeArray(json)) {
    | Some(rows) =>
        let rec decode_rows = (index, notes) =>
            if (index >= Array.length(rows)) {
                Ok(notes |> Stdlib.List.rev |> Array.of_list);
            } else {
                switch (decode_grammar_note(rows[index])) {
                | Some(note) => decode_rows(index + 1, [note, ...notes])
                | None =>
                    Error(
                        "Invalid grammar note data at array index "
                        ++ Js.Int.toString(index),
                    )
                };
            };

        decode_rows(0, []);
    | None => Error("The grammar notes index JSON root must be an array")
    };

[@react.component]
let make = () => {
    open Bindings;
    open Mui; 
    open Store;

    let current_user =
        app_store
        |> Zustand.use_store(state => state.current_user);

    let url = ReasonReactRouter.useUrl();
    let slug = switch url.path {
    | ["learn", "grammar_notes", slug] => Some(slug)
    | _ => None
    };
    let (grammar_notes, set_grammar_notes) =
        React.useState(() => [||]);
    let (index_error, set_index_error) =
        React.useState(() => (None: option(string)));
    let (markdown, set_markdown) =
        React.useState(() => (None: option(string)));
    let (markdown_error, set_markdown_error) =
        React.useState(() => (None: option(string)));
    let (scroll_percentage, set_scroll_percentage) =
        React.useState(() => 0);
    let (is_banner_visible, set_is_banner_visible) =
        React.useState(() => true);
    let grammar_note_ref: React.ref(Js.nullable(Dom.element)) =
        React.useRef(Js.Nullable.null);
    let banner_ref: React.ref(Js.nullable(Dom.element)) =
        React.useRef(Js.Nullable.null);
    let grammar_note_content_ref: React.ref(Js.nullable(Dom.element)) =
        React.useRef(Js.Nullable.null);
    let (bookmark_popover_open, set_bookmark_popover_open) =
        React.useState(() => false);
    let (bookmark_popover_pos, set_bookmark_popover_pos) =
        React.useState(() => ({left: 0, top: 0}: Popover.anchorPos));
    let (bookmark_text, set_bookmark_text) =
        React.useState(() => None);
    let (bookmark_prefix_context, set_bookmark_prefix_context) =
        React.useState(() => "");
    let (bookmark_suffix_context, set_bookmark_suffix_context) =
        React.useState(() => "");
    let (bookmark_snackbar_open, set_bookmark_snackbar_open) =
        React.useState(() => false);
    let (bookmark_notification, set_bookmark_notification) =
        React.useState(() => (None: option(bookmark_notification)));
    let (saving_bookmark, set_saving_bookmark) =
        React.useState(() => None);
    let (bookmark_highlight_revision, set_bookmark_highlight_revision) =
        React.useState(() => 0);
    let (bookmark_layout_revision, set_bookmark_layout_revision) =
        React.useState(() => 0);
    let (bookmark_markers, set_bookmark_markers) =
        React.useState(() => ([||]: array(bookmark_marker)));
    let (bookmark_menu_anchor, set_bookmark_menu_anchor) =
        React.useState(() =>
            (Js.Nullable.null: Js.Nullable.t(Dom.element))
        );
    let bookmark_menu_open = !Js.Nullable.isNullable(bookmark_menu_anchor);
    let (selected_bookmark_id, set_selected_bookmark_id) =
        React.useState(() => (None: option(string)));

    let is_mobile = UseMediaQuery.use("(max-width:599px)");

    React.useEffect1(() => {
        switch (Js.Nullable.toOption(grammar_note_content_ref.current)) {
        | Some(content_element) =>
            let observer =
                Browser.ResizeObserver.make((_entries, _observer) =>
                    set_bookmark_layout_revision(revision => revision + 1)
                );
            Browser.ResizeObserver.observe(observer, content_element);
            Some(() => Browser.ResizeObserver.disconnect(observer));
        | None => None
        };
    }, [|markdown|]);

    React.useEffect0(() => {
        Browser.Fetch.get("/grammar_notes/index.json")
        |> Js.Promise.then_(response => {
            if (Browser.Fetch.ok(response)) {
                response
                |> Browser.Fetch.json
                |> Js.Promise.then_(json => {
                    switch (parse_grammar_notes_index(json)) {
                    | Ok(notes) => set_grammar_notes(_ => notes)
                    | Error(message) => set_index_error(_ => Some(message))
                    };
                    Js.Promise.resolve();
                });
            } else {
                set_index_error(_ => Some("Unable to load the grammar notes index"));
                Js.Promise.resolve();
            };
        })
        |> Js.Promise.catch(_error => {
            set_index_error(_ => Some("Unable to load the grammar notes index"));
            Js.Promise.resolve();
        })
        |> ignore;

        None;
    });

    let selected_note = switch slug {
    | Some(slug) => Array.find_opt(note => note.slug == slug, grammar_notes)
    | None => None
    };
    let selected_note_file = switch selected_note {
    | Some(note) => note.file
    | None => ""
    };

    React.useEffect1(() => {
        let cancelled = ref(false);
        set_markdown(_ => None);
        set_markdown_error(_ => None);
        set_scroll_percentage(_ => 0);
        set_is_banner_visible(_ => true);

        switch selected_note {
        | Some(note) =>
            Browser.Fetch.get(note.file)
            |> Js.Promise.then_(response => {
                let response_is_html =
                    switch (
                        response
                        |> Browser.Fetch.headers
                        |> Browser.Fetch.get_header("Content-Type")
                    ) {
                    | Some(content_type) =>
                        content_type
                        |> Js.String.toLowerCase
                        |> Js.String.includes(~search="text/html")
                    | None => false
                    };

                if (Browser.Fetch.ok(response) && !response_is_html) {
                    response
                    |> Browser.Fetch.text
                    |> Js.Promise.then_(content => {
                        if (!cancelled^) {
                            set_markdown(_ => Some(content));
                        };
                        Js.Promise.resolve();
                    });
                } else {
                    if (!cancelled^) {
                        set_markdown_error(_ => Some("Unable to load this grammar note"));
                    };
                    Js.Promise.resolve();
                };
            })
            |> Js.Promise.catch(_error => {
                if (!cancelled^) {
                    set_markdown_error(_ => Some("Unable to load this grammar note"));
                };
                Js.Promise.resolve();
            })
            |> ignore
        | None => ()
        };

        Some(() => cancelled := true);
    }, [|selected_note_file|]);

    let handle_scroll = (event: React.Event.UI.t) => {
        let element =
            event
            |> React.Event.UI.currentTarget
            |> dom_element_from_event_target;
        let maximum_scroll =
            ScrollableElement.scroll_height(element)
            -. ScrollableElement.client_height(element);
        let percentage =
            if (maximum_scroll <= 0.0) {
                100;
            } else {
                let calculated_percentage =
                    ScrollableElement.scroll_top(element)
                    /. maximum_scroll
                    *. 100.0
                    |> Js.Math.round
                    |> int_of_float;

                if (calculated_percentage < 0) {
                    0;
                } else if (calculated_percentage > 100) {
                    100;
                } else {
                    calculated_percentage;
                };
            };

        set_scroll_percentage(_ => percentage);

        switch (
            Js.Nullable.toOption(grammar_note_ref.current),
            Js.Nullable.toOption(banner_ref.current),
        ) {
        | (Some(container), Some(banner)) =>
            let container_rect = ElementViewport.get_bounding_client_rect(container);
            let banner_rect = ElementViewport.get_bounding_client_rect(banner);
            let banner_is_visible =
                ElementViewport.bottom(banner_rect) > ElementViewport.top(container_rect)
                && ElementViewport.top(banner_rect) < ElementViewport.bottom(container_rect);

            if (banner_is_visible != is_banner_visible) {
                set_is_banner_visible(_ => banner_is_visible);
            }
        | _ => ()
        };
    };

    let scroll_to_top = () =>
        switch (Js.Nullable.toOption(grammar_note_ref.current)) {
        | Some(element) =>
            ScrollableElement.scroll_to(
                ScrollableElement.make_scroll_to_options(
                    ~top=0.0,
                    ~behavior="smooth",
                    (),
                ),
                element,
            )
        | None => ()
        };

    let share_grammar_note = () => {
        let _ =
            Browser.Window.location_href
            |> Browser.Clipboard.write_text
            |> Js.Promise.catch(error => {
                Js.log2("Could not copy the grammar note URL:", error);
                Js.Promise.resolve();
            });
        ();
    };

    let close_bookmark_popover = () => {
        switch (Browser.Document.active_element) {
        | Some(element) => Browser.Element.blur(element)
        | None => ()
        };
        set_bookmark_popover_open(_ => false);
    };

    let close_bookmark_menu = () => {
        switch (Browser.Document.active_element) {
        | Some(element) => Browser.Element.blur(element)
        | None => ()
        };
        set_bookmark_menu_anchor(_ => Js.Nullable.null);
        set_selected_bookmark_id(_ => None);
    };

    let show_bookmark_notification = notification => {
        set_bookmark_notification(_ => Some(notification));
        set_bookmark_snackbar_open(_ => true);
    };

    let get_bookmark_context = (range, content_element) => {
        let start_container = Browser.Range.start_container(range);
        let end_container = Browser.Range.end_container(range);

        if (
            Browser.Element.contains(start_container, content_element)
            && Browser.Element.contains(end_container, content_element)
        ) {
            let prefix_range = Browser.Range.clone_range(range);
            Browser.Range.select_node_contents(content_element, prefix_range);
            Browser.Range.set_end(
                start_container,
                Browser.Range.start_offset(range),
                prefix_range,
            );

            let suffix_range = Browser.Range.clone_range(range);
            Browser.Range.select_node_contents(content_element, suffix_range);
            Browser.Range.set_start(
                end_container,
                Browser.Range.end_offset(range),
                suffix_range,
            );

            let complete_prefix = Browser.Range.to_string(prefix_range);
            let complete_suffix = Browser.Range.to_string(suffix_range);
            let prefix_length = Js.String.length(complete_prefix);
            let context_length = 250;
            let prefix_start =
                prefix_length > context_length
                    ? prefix_length - context_length
                    : 0;

            Some((
                complete_prefix |> Js.String.slice(~start=prefix_start),
                complete_suffix
                |> Js.String.slice(~start=0, ~end_=context_length),
            ));
        } else {
            None;
        };
    };

    let collect_rendered_text = content_element => {
        let walker =
            Browser.Document.create_tree_walker(
                content_element,
                Browser.Document.show_text,
            );

        let rec collect = (offset, segments, text_parts) =>
            switch (Browser.TreeWalker.next_node(walker)) {
            | Some(node) =>
                let text =
                    switch (Browser.Node.value(node)) {
                    | Some(value) => value
                    | None => ""
                    };
                let length = Js.String.length(text);
                let segment = {
                    node,
                    start_offset: offset,
                    end_offset: offset + length,
                };
                collect(
                    offset + length,
                    length > 0 ? [segment, ...segments] : segments,
                    length > 0 ? [text, ...text_parts] : text_parts,
                );
            | None =>
                (
                    segments |> Stdlib.List.rev |> Array.of_list,
                    text_parts
                    |> Stdlib.List.rev
                    |> Array.of_list
                    |> Js.Array.join(~sep=""),
                )
            };

        collect(0, [], []);
    };

    let common_prefix_length = (first, second) => {
        let maximum =
            Js.String.length(first) < Js.String.length(second)
                ? Js.String.length(first)
                : Js.String.length(second);
        let rec compare = index =>
            if (
                index < maximum
                && Js.String.charAt(~index, first) == Js.String.charAt(~index, second)
            ) {
                compare(index + 1);
            } else {
                index;
            };
        compare(0);
    };

    let common_suffix_length = (first, second) => {
        let first_length = Js.String.length(first);
        let second_length = Js.String.length(second);
        let maximum = first_length < second_length ? first_length : second_length;
        let rec compare = matched =>
            if (
                matched < maximum
                && Js.String.charAt(~index=first_length - matched - 1, first)
                    == Js.String.charAt(~index=second_length - matched - 1, second)
            ) {
                compare(matched + 1);
            } else {
                matched;
            };
        compare(0);
    };

    let remove_markdown_escapes = text =>
        text
        |> Js.String.replaceByRe(
            ~regexp=Js.Re.fromStringWithFlags(
                {js|\\\\(.)|js},
                ~flags="g",
            ),
            ~replacement="$1",
        );

    let normalize_whitespace = text =>
        text
        |> Js.String.replaceByRe(
            ~regexp=Js.Re.fromStringWithFlags("\\s+", ~flags="g"),
            ~replacement=" ",
        );

    let is_whitespace = character =>
        character != "" && (character |> Js.String.trim) == "";

    /* Return offsets in the original rendered text while allowing whitespace in
       the stored selection to differ from the Markdown DOM. Browsers collapse a
       source newline visually, but Range.toString/text nodes still retain it. */
    let find_flexible_text_match = (rendered_text, search_text, search_start) => {
        let rendered_length = Js.String.length(rendered_text);
        let search_length = Js.String.length(search_text);

        let rec skip_whitespace = (text, index, length) =>
            if (
                index < length
                && (text |> Js.String.charAt(~index) |> is_whitespace)
            ) {
                skip_whitespace(text, index + 1, length);
            } else {
                index;
            };

        let rec matches_at = (rendered_index, search_index) =>
            if (search_index >= search_length) {
                Some(rendered_index);
            } else if (rendered_index >= rendered_length) {
                None;
            } else {
                let rendered_character =
                    rendered_text |> Js.String.charAt(~index=rendered_index);
                let search_character =
                    search_text |> Js.String.charAt(~index=search_index);

                if (
                    is_whitespace(rendered_character)
                    && is_whitespace(search_character)
                ) {
                    matches_at(
                        skip_whitespace(
                            rendered_text,
                            rendered_index,
                            rendered_length,
                        ),
                        skip_whitespace(search_text, search_index, search_length),
                    );
                } else if (rendered_character == search_character) {
                    matches_at(rendered_index + 1, search_index + 1);
                } else {
                    None;
                };
            };

        let rec try_at = candidate =>
            if (candidate >= rendered_length) {
                None;
            } else {
                switch (matches_at(candidate, 0)) {
                | Some(end_offset) => Some((candidate, end_offset))
                | None => try_at(candidate + 1)
                };
            };

        if (search_length === 0) {
            None;
        } else {
            let exact_offset =
                rendered_text
                |> Js.String.indexOf(~search=search_text, ~start=search_start);
            if (exact_offset >= 0) {
                Some((exact_offset, exact_offset + search_length));
            } else {
                try_at(search_start);
            };
        };
    };

    let find_bookmark_offset = (rendered_text, bookmark: Supabase.bookmark_row) => {
        /* Bookmarks normally contain text selected from the rendered DOM. Older or
           imported rows may contain Markdown source escapes (for example `\*`),
           which ReactMarkdown removes before rendering. Prefer an exact match so
           that a real backslash remains meaningful, and only unescape as a
           compatibility fallback. */
        let exact_text_is_present =
            rendered_text
            |> Js.String.indexOf(~search=bookmark.selected_text, ~start=0)
            >= 0;
        let selected_text =
            exact_text_is_present
                ? bookmark.selected_text
                : remove_markdown_escapes(bookmark.selected_text);
        let prefix_context =
            exact_text_is_present
                ? bookmark.prefix_context
                : remove_markdown_escapes(bookmark.prefix_context);
        let suffix_context =
            exact_text_is_present
                ? bookmark.suffix_context
                : remove_markdown_escapes(bookmark.suffix_context);
        let prefix_length = Js.String.length(prefix_context);
        let suffix_length = Js.String.length(suffix_context);

        if (Js.String.length(selected_text) === 0) {
            None;
        } else {
            let rec find = (search_start, best_offset, best_score) => {
                switch (
                    find_flexible_text_match(
                        rendered_text,
                        selected_text,
                        search_start,
                    )
                ) {
                | None =>
                    best_offset;
                | Some((candidate, end_offset)) =>
                    let prefix_start =
                        candidate > prefix_length
                            ? candidate - prefix_length
                            : 0;
                    let candidate_prefix =
                        rendered_text
                        |> Js.String.slice(
                            ~start=prefix_start,
                            ~end_=candidate,
                        );
                    let candidate_suffix =
                        rendered_text
                        |> Js.String.slice(
                            ~start=end_offset,
                            ~end_=end_offset + suffix_length,
                        );
                    let score =
                        common_suffix_length(
                            normalize_whitespace(candidate_prefix),
                            normalize_whitespace(prefix_context),
                        )
                        + common_prefix_length(
                            normalize_whitespace(candidate_suffix),
                            normalize_whitespace(suffix_context),
                        );
                    let (next_best_offset, next_best_score) =
                        score > best_score
                            ? (Some((candidate, end_offset)), score)
                            : (best_offset, best_score);

                    find(
                        candidate + 1,
                        next_best_offset,
                        next_best_score,
                    );
                };
            };

            find(0, None, -1);
        };
    };

    let range_from_offsets = (segments, start_offset, end_offset) => {
        let start_segment =
            segments
            |> Array.find_opt((segment: rendered_text_segment) =>
                start_offset >= segment.start_offset
                && start_offset < segment.end_offset
            );
        let end_segment =
            segments
            |> Array.find_opt((segment: rendered_text_segment) =>
                end_offset > segment.start_offset
                && end_offset <= segment.end_offset
            );

        switch (start_segment, end_segment) {
        | (Some(start_segment), Some(end_segment)) =>
            let range = Browser.Document.create_range();
            Browser.Range.set_start(
                start_segment.node,
                start_offset - start_segment.start_offset,
                range,
            );
            Browser.Range.set_end(
                end_segment.node,
                end_offset - end_segment.start_offset,
                range,
            );
            Some(range);
        | _ => None
        };
    };

    let highlight_names = [|
        "grammar-bookmark-pink",
        "grammar-bookmark-salmon",
        "grammar-bookmark-teal",
        "grammar-bookmark-blue",
    |];

    let remove_bookmark_highlights = registry =>
        highlight_names
        |> Array.iter(name => {
            Browser.CssHighlights.delete(name, registry) |> ignore;
        });

    React.useEffect5(() => {
        let cancelled = ref(false);
        let registry = Browser.CssHighlights.registry;

        set_bookmark_markers(_ => [||]);

        switch registry {
        | Some(registry) => remove_bookmark_highlights(registry)
        | None => ()
        };

        switch (
            current_user,
            selected_note,
            markdown,
            Js.Nullable.toOption(grammar_note_content_ref.current),
            registry,
        ) {
        | (Some(user), Some(note), Some(_), Some(content_element), Some(registry)) =>
            Supabase.client
            |> Supabase.Query.from("bookmarks")
            |> Supabase.Query.select(
                "id,user_id,grammar_note_slug,grammar_note_title,selected_text,prefix_context,suffix_context,bookmark_type,created_at",
            )
            |> Supabase.Filter.eq(
                ~column="user_id",
                ~value=Supabase.Auth.user_id(user),
            )
            |> Supabase.Filter.eq(
                ~column="grammar_note_slug",
                ~value=note.slug,
            )
            |> Js.Promise.then_(response => {
                if (!cancelled^) {
                    let decoded = Supabase.Response.decode_bookmarks(response);

                    if (decoded.success) {
                        let (segments, rendered_text) =
                            collect_rendered_text(content_element);
                        let content_top =
                            content_element
                            |> ElementViewport.get_bounding_client_rect
                            |> ElementViewport.top;
                        let resolved_markers: ref(list(bookmark_marker)) = ref([]);

                        let register_bookmark_type = (bookmark_type, name) => {
                            let ranges =
                                decoded.data
                                |> Array.fold_left((ranges, (bookmark: Supabase.bookmark_row)) =>
                                    if (bookmark.bookmark_type === bookmark_type) {
                                        switch (find_bookmark_offset(rendered_text, bookmark)) {
                                        | Some((start_offset, end_offset)) =>
                                            switch (
                                                range_from_offsets(
                                                    segments,
                                                    start_offset,
                                                    end_offset,
                                                )
                                            ) {
                                            | Some(range) => {
                                                let client_rects =
                                                    Browser.Range.get_client_rects(range);
                                                let marker_rect =
                                                    switch (
                                                        Browser.DomRectList.item(
                                                            0,
                                                            client_rects,
                                                        )
                                                    ) {
                                                    | Some(rect) => rect
                                                    | None =>
                                                        Browser.Range.get_bounding_client_rect(
                                                            range,
                                                        )
                                                    };
                                                resolved_markers := [{
                                                    id: bookmark.id,
                                                    bookmark_type,
                                                    top:
                                                        Browser.DomRect.top(marker_rect)
                                                        -. content_top
                                                        +. Browser.DomRect.height(marker_rect)
                                                        /. 2.0,
                                                }, ...resolved_markers^];
                                                [range, ...ranges];
                                            }
                                            | None => ranges
                                            }
                                        | None => ranges
                                        };
                                    } else {
                                        ranges;
                                    }
                                , []);

                            switch ranges {
                            | [] => ()
                            | ranges =>
                                Browser.CssHighlights.set(
                                    name,
                                    Browser.Highlight.make(
                                        ranges
                                        |> Stdlib.List.rev
                                        |> Array.of_list,
                                    ),
                                    registry,
                                )
                            };
                        };

                        register_bookmark_type(0, highlight_names[0]);
                        register_bookmark_type(1, highlight_names[1]);
                        register_bookmark_type(2, highlight_names[2]);
                        register_bookmark_type(3, highlight_names[3]);
                        set_bookmark_markers(_ =>
                            resolved_markers^
                            |> Stdlib.List.rev
                            |> Array.of_list
                        );
                    } else {
                        switch decoded.error {
                        | Some(error) => Js.log2("Unable to fetch bookmarks:", error)
                        | None => ()
                        };
                    };
                };

                Js.Promise.resolve();
            })
            |> Js.Promise.catch(error => {
                if (!cancelled^) {
                    Js.log2("Unable to fetch bookmarks:", error);
                };
                Js.Promise.resolve();
            })
            |> ignore
        | _ => ()
        };

        Some(() => {
            cancelled := true;
            switch registry {
            | Some(registry) => remove_bookmark_highlights(registry)
            | None => ()
            };
        });
    }, (
        current_user,
        selected_note_file,
        markdown,
        bookmark_highlight_revision,
        bookmark_layout_revision,
    ));

    let bookmark_marker_color = bookmark_type =>
        switch bookmark_type {
        | 0 => Config.bookmarkColors##pink
        | 1 => Config.bookmarkColors##salmon
        | 2 => Config.bookmarkColors##teal
        | _ => Config.bookmarkColors##blue
        };

    let save_bookmark = (bookmark_type: int) =>
        switch bookmark_text {
        | Some(text) =>
            switch (current_user, selected_note) {
            | (Some(user), Some(note)) =>
                set_saving_bookmark(_ => Some(bookmark_type));

                let row =
                    Supabase.Query.make_bookmark_insert(
                        ~user_id=Supabase.Auth.user_id(user),
                        ~grammar_note_slug=note.slug,
                        ~grammar_note_title=note.title,
                        ~selected_text=text,
                        ~prefix_context=bookmark_prefix_context,
                        ~suffix_context=bookmark_suffix_context,
                        ~bookmark_type,
                        (),
                    );

                Supabase.client
                |> Supabase.Query.from("bookmarks")
                |> Supabase.Query.insert_bookmarks([|row|])
                |> Js.Promise.then_(response => {
                    switch (Supabase.Query.mutation_error(response)) {
                    | Some(error) => {
                        Js.log2(
                            "Unable to save the bookmark:",
                            Supabase.Query.postgrest_error_message(error),
                        );
                        show_bookmark_notification(BookmarkSaveFailed);
                    }
                    | None => {
                        show_bookmark_notification(BookmarkSaved);
                        set_bookmark_highlight_revision(revision => revision + 1);
                        close_bookmark_popover();
                    }
                    };

                    set_saving_bookmark(_ => None);
                    Js.Promise.resolve();
                })
                |> Js.Promise.catch(error => {
                    Js.log2("Unable to save the bookmark:", error);
                    show_bookmark_notification(BookmarkSaveFailed);
                    set_saving_bookmark(_ => None);
                    Js.Promise.resolve();
                })
                |> ignore
            | _ => show_bookmark_notification(BookmarkSaveFailed)
            }
        | None =>
            show_bookmark_notification(NoBookmarkTextSelected);
        };

    let remove_bookmark = () =>
        switch (current_user, selected_bookmark_id) {
        | (Some(user), Some(bookmark_id)) =>
            Supabase.client
            |> Supabase.Query.from("bookmarks")
            |> Supabase.Query.delete_bookmarks
            |> Supabase.Query.eq_bookmarks_mutation(
                ~column="user_id",
                ~value=Supabase.Auth.user_id(user),
            )
            |> Supabase.Query.eq_bookmarks_mutation(
                ~column="id",
                ~value=bookmark_id,
            )
            |> Js.Promise.then_(response => {
                switch (Supabase.Query.mutation_error(response)) {
                | Some(error) => {
                    Js.log2(
                        "Unable to remove the bookmark:",
                        Supabase.Query.postgrest_error_message(error),
                    );
                    show_bookmark_notification(BookmarkRemoveFailed);
                }
                | None => {
                    show_bookmark_notification(BookmarkRemoved);
                    set_bookmark_highlight_revision(revision => revision + 1);
                }
                };

                close_bookmark_menu();
                Js.Promise.resolve();
            })
            |> Js.Promise.catch(error => {
                Js.log2("Unable to remove the bookmark:", error);
                show_bookmark_notification(BookmarkRemoveFailed);
                close_bookmark_menu();
                Js.Promise.resolve();
            })
            |> ignore
        | (None, _) => {
            show_bookmark_notification(BookmarkAuthenticationRequired);
            close_bookmark_menu();
        }
        | (_, None) => close_bookmark_menu()
        };

    <>
    <Grid className=css##grammarNotesContainer>
        {
            switch selected_note {
                | Some(note) =>
                    <>
                        <Box className=css##readProgress>
                            <CircularProgress
                                variant=`determinate
                                value={scroll_percentage |> Int.to_float}
                                enableTrackSlot=true
                            />
                            <Box
                                sx={{
                                    "top": 0,
                                    "left": 0,
                                    "bottom": 0,
                                    "right": 0,
                                    "position": "absolute",
                                    "display": "flex",
                                    "alignItems": "center",
                                    "justifyContent": "center",
                                }}
                            >
                                <Typography
                                    variant=Typography.Variant.caption
                                    component=RootComponent.htmlElement("div")
                                    sx={{ "color": "text.secondary" }}
                                >
                                    {(scroll_percentage |> Int.to_string) ++ "%" |> React.string}
                                </Typography>
                            </Box>
                        </Box>
                        <div
                            className=css##grammarNote
                            onScroll=handle_scroll
                            ref={ReactDOM.Ref.domRef(grammar_note_ref)}
                            style=ReactDOM.Style.make(~position="relative", ())
                        >
                            <Box
                                sx={{
                                    "position": "sticky",
                                    "top": "0",
                                    "zIndex": 2,
                                    "height": "0",
                                    "overflow": "visible",
                                    "pointerEvents": "none",
                                }}
                            >
                                <Typography 
                                    variant=Typography.Variant.h3
                                    align=`center
                                    className=css##grammarNoteTitle
                                    sx={{
                                        "padding": "20px",
                                        "backgroundColor": Config.colors##whiteSmoke,
                                        "opacity": is_banner_visible ? 0.0 : 1.0,
                                        "transform": is_banner_visible
                                            ? "translateY(-100%)"
                                            : "translateY(0)",
                                        "transition":
                                            "transform 900ms cubic-bezier(0.22, 1, 0.36, 1), opacity 900ms ease",
                                    }}
                                > 
                                    {note.title |> React.string}
                                </Typography>
                            </Box>
                            <img 
                                src={note.banner} 
                                alt={note.title} 
                                ref={ReactDOM.Ref.domRef(banner_ref)}
                                style=ReactDOM.Style.make(~width="100%", ~marginBottom="20px", ())
                            />
                            <Container
                                className=css##grammarNoteContent
                                ref={ReactDOM.Ref.domRef(grammar_note_content_ref)}
                                onMouseUp={event => {
                                    switch (Browser.Window.get_selection(), current_user) {
                                    | (Some(selection), Some(_))
                                        when Browser.Selection.range_count(selection) > 0
                                        && !Browser.Selection.is_collapsed(selection) =>
                                        let text = Browser.Selection.to_string(selection);
                                        let range = Browser.Selection.get_range_at(0, selection);
                                        let rect = Browser.Range.get_bounding_client_rect(range);
                                        let content_element =
                                            event
                                            |> React.Event.UI.currentTarget
                                            |> dom_element_from_event_target;

                                        let left =
                                            (
                                                Browser.DomRect.left(rect)
                                                +. Browser.DomRect.width(rect) /. 2.
                                            )
                                            |> Js.Math.round
                                            |> int_of_float;

                                        let top =
                                            Browser.DomRect.top(rect)
                                            |> Js.Math.round
                                            |> int_of_float;

                                        set_bookmark_popover_pos(_ => {left, top});
                                        set_bookmark_popover_open(_ => true);
                                        set_bookmark_text(_ => Some(text));
                                        switch (get_bookmark_context(range, content_element)) {
                                        | Some((prefix_context, suffix_context)) => {
                                            set_bookmark_prefix_context(_ => prefix_context);
                                            set_bookmark_suffix_context(_ => suffix_context);
                                        }
                                        | None => {
                                            set_bookmark_prefix_context(_ => "");
                                            set_bookmark_suffix_context(_ => "");
                                        }
                                        };
                                    | _ => ()
                                    };
                                }}
                            >
                                {
                                    is_mobile ? 
                                        React.null : 
                                        bookmark_markers
                                        |> Array.map((marker: bookmark_marker) =>
                                            <IconButton
                                                key=marker.id
                                                className=css##bookmarkMarker
                                                size=`small
                                                ariaLabel="Saved bookmark"
                                                sx={{"top": marker.top}}
                                                onClick={event => {
                                                    set_bookmark_menu_anchor(_ =>
                                                        event
                                                        |> React.Event.Mouse.currentTarget
                                                        |> dom_element_from_event_target
                                                        |> Js.Nullable.return
                                                    );
                                                    set_selected_bookmark_id(_ => Some(marker.id));
                                                }}
                                            >
                                                <TablerReact.IconBookmarkFilled
                                                    color={bookmark_marker_color(
                                                        marker.bookmark_type,
                                                    )}
                                                />
                                            </IconButton>
                                        )
                                        |> React.array
                                }
                                <Menu
                                    anchorEl=bookmark_menu_anchor
                                    _open=bookmark_menu_open
                                    anchorOrigin={{
                                        vertical: `center,
                                        horizontal: `right,
                                    }}
                                    onClose={_event => close_bookmark_menu()}
                                >
                                    <MenuItem dense=true onClick={_event => remove_bookmark()}>
                                        <ListItemIcon>
                                            <TablerReact.IconTrash />
                                        </ListItemIcon>
                                        <ListItemText>
                                            {"Remove" |> React.string}
                                        </ListItemText>
                                    </MenuItem>
                                </Menu>
                                <Popover
                                    _open=bookmark_popover_open
                                    anchorReference=`anchorPosition
                                    anchorPosition=bookmark_popover_pos
                                    disableAutoFocus=true
                                    transformOrigin={{
                                        vertical: `bottom,
                                        horizontal: `center,
                                    }}
                                    onClose={_event => close_bookmark_popover()}
                                    sx={{"padding": "16px 8px"}}
                                >
                                    {
                                        switch current_user {
                                            | Some(_) => {
                                                switch (saving_bookmark) {
                                                | Some(color) =>
                                                    <Stack direction=`row sx={{"alignItems": "center", "justifyContent": "center" }}>
                                                        {color !== 0 ? <IconButton size=`small disabled=true>
                                                            <TablerReact.IconBookmarkFilled color=Config.bookmarkColors##pink />
                                                        </IconButton> : <CircularProgress size=`Number(24) />}
                                                        {color !== 1 ? <IconButton size=`small disabled=true>
                                                            <TablerReact.IconBookmarkFilled color=Config.bookmarkColors##salmon />
                                                        </IconButton> : <CircularProgress size=`Number(24) />}
                                                        {color !== 2 ? <IconButton size=`small disabled=true>
                                                            <TablerReact.IconBookmarkFilled color=Config.bookmarkColors##teal />
                                                        </IconButton> : <CircularProgress size=`Number(24) />}
                                                        {color !== 3 ? <IconButton size=`small disabled=true>
                                                            <TablerReact.IconBookmarkFilled color=Config.bookmarkColors##blue />
                                                        </IconButton> : <CircularProgress size=`Number(24) />}
                                                    </Stack>
                                                | None =>
                                                    <Stack direction=`row sx={{"alignItems": "center", "justifyContent": "center" }}>
                                                        <IconButton size=`small onClick={_ => save_bookmark(Config.get_bookmark_number(Config.Pink))}>
                                                            <TablerReact.IconBookmarkFilled color=Config.bookmarkColors##pink />
                                                        </IconButton>
                                                        <IconButton size=`small onClick={_ => save_bookmark(Config.get_bookmark_number(Config.Salmon))}>
                                                            <TablerReact.IconBookmarkFilled color=Config.bookmarkColors##salmon />
                                                        </IconButton>
                                                        <IconButton size=`small onClick={_ => save_bookmark(Config.get_bookmark_number(Config.Teal))}>
                                                            <TablerReact.IconBookmarkFilled color=Config.bookmarkColors##teal />
                                                        </IconButton>
                                                        <IconButton size=`small onClick={_ => save_bookmark(Config.get_bookmark_number(Config.Blue))}>
                                                            <TablerReact.IconBookmarkFilled color=Config.bookmarkColors##blue />
                                                        </IconButton>
                                                    </Stack>
                                                }
                                            }
                                            | None =>
                                                <div> {"You must be logged in to bookmark notes." |> React.string} </div>
                                        }
                                    }
                                </Popover>
                                {
                                switch (markdown, markdown_error) {
                                | (Some(content), _) =>
                                    <ReactMarkdown
                                        markdown=content
                                        remarkPlugins=[|ReactMarkdown.remarkGfmWithoutSingleTilde|]
                                        rehypePlugins=[|ReactMarkdown.rehypeCuneiform|]
                                    />
                                    | (None, Some(message)) =>
                                        <p> {message |> React.string} </p>
                                    | (None, None) =>
                                        <p> {"Loading grammar note..." |> React.string} </p>
                                    }
                                }
                            </Container>
                        </div>
                    </>
                | None =>
                    switch index_error {
                    | Some(message) => <p> {message |> React.string} </p>
                    | None =>
                        <Container
                            sx={{
                                "display": "flex",
                                "flexDirection": "column",
                                "alignItems": "center",
                                "padding": "40px 20px",
                            }}
                        >
                            <Typography variant=Typography.Variant.h4>
                                {"Available grammar notes" |> React.string}
                            </Typography>
                            <List sx={{"width": "100%", "maxWidth": "600px"}}>
                                {
                                    grammar_notes
                                    |> Array.map(note =>
                                        <ListItem key=note.slug disablePadding=true>
                                            <ListItemButton
                                                onClick={_ =>
                                                    ReasonReactRouter.push(
                                                        "/learn/grammar_notes/" ++ note.slug,
                                                    )
                                                }
                                            >
                                                <ListItemIcon>
                                                    <TablerReact.IconNote />
                                                </ListItemIcon>
                                                <ListItemText
                                                    primary={note.title |> React.string}
                                                />
                                            </ListItemButton>
                                        </ListItem>
                                    )
                                    |> React.array
                                }
                            </List>
                        </Container>
                    }
            }
        }
        <Stack 
            className=css##sideButtons
            useFlexGap=true
            spacing=`Number(1)
        >
            <Tooltip 
                title={"Share this grammar note" |> React.string}
                placement=Tooltip.Placement.left
                arrow=true
            >
                <IconButton
                    size=`small
                    ariaLabel="Share this grammar note"
                    onClick={_ => share_grammar_note()}
                >
                    <TablerReact.IconLink color=Config.colors##protonRed />
                </IconButton>
            </Tooltip>
            <Tooltip 
                title={"Scroll to the top" |> React.string}
                placement=Tooltip.Placement.left
                arrow=true
            >
                <IconButton
                    size=`small
                    ariaLabel="Scroll to the top of the grammar note"
                    onClick={_ => scroll_to_top()}
                >
                    <TablerReact.IconArrowBigUpLinesFilled color=Config.colors##protonRed />
                </IconButton>
            </Tooltip>
        </Stack>
    </Grid>
    <Snackbar
        _open=bookmark_snackbar_open
        anchorOrigin={{
            vertical: `bottom,
            horizontal: `right,
        }}
        autoHideDuration=3000
        onClose={_ => set_bookmark_snackbar_open(_ => false)}
    >
        {
            switch bookmark_notification {
            | Some(BookmarkSaved) =>
                <Alert severity=`success variant=`filled sx={{"width": "100%"}}>
                    {"Bookmark saved." |> React.string}
                </Alert>
            | Some(NoBookmarkTextSelected) =>
                <Alert severity=`warning variant=`filled sx={{"width": "100%"}}>
                    {"No text was selected." |> React.string}
                </Alert>
            | Some(BookmarkSaveFailed) =>
                <Alert severity=`error variant=`filled sx={{"width": "100%"}}>
                    {"The bookmark could not be saved." |> React.string}
                </Alert>
            | Some(BookmarkRemoved) =>
                <Alert severity=`success variant=`filled sx={{"width": "100%"}}>
                    {"Bookmark removed." |> React.string}
                </Alert>
            | Some(BookmarkRemoveFailed) =>
                <Alert severity=`error variant=`filled sx={{"width": "100%"}}>
                    {"The bookmark could not be removed." |> React.string}
                </Alert>
            | Some(BookmarkAuthenticationRequired) =>
                <Alert severity=`warning variant=`filled sx={{"width": "100%"}}>
                    {"You must be logged in to remove a bookmark." |> React.string}
                </Alert>
            | None => React.null
            }
        }
    </Snackbar>
    </>
}
