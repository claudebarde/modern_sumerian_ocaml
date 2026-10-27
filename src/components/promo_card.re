[@mel.module "../styles/PromoCard.module.scss"] external css: Js.t({..}) = "default";

// A small, non-blocking promotion card pinned to the lower right corner of the
// page (full width at the bottom on small screens). It is fully controlled by
// its parent through `is_open` and `on_dismiss`, so it can promote anything:
//
// <Promo_card
//     is_open=show_promo
//     on_dismiss={() => set_show_promo(_ => false)}
//     title="..."
//     description="..."
//     image=some_image
//     image_alt="..."
//     cta_label="..."
//     cta_href="..."
// />
[@react.component]
let make = (
    ~is_open: bool,
    ~on_dismiss: unit => unit,
    ~title: string,
    ~description: string,
    ~cta_label: string,
    ~cta_href: string,
    ~dismiss_label: string="Dismiss",
    ~image: option(string)=?,
    ~image_alt: string="",
) => {
    open Bindings;
    open Mui;

    <Slide _in=is_open appear=true direction=`up timeout=400>
        <Paper
            className={css##promo}
            elevation=8
            component=RootComponent.htmlElement("aside")
        >
            <div className={css##body}>
                {switch (image) {
                | Some(src) => <img className={css##image} src alt=image_alt />
                | None => React.null
                }}
                <div className={css##text}>
                    <Typography
                        variant=Typography.Variant.subtitle1
                        component=RootComponent.htmlElement("h2")
                        className={css##title}
                    >
                        {title |> React.string}
                    </Typography>
                    <Typography variant=Typography.Variant.body2>
                        {description |> React.string}
                    </Typography>
                </div>
            </div>
            <div className={css##actions}>
                <Button onClick={_ => on_dismiss()} variant=`outlined size=`small>
                    {dismiss_label |> React.string}
                </Button>
                <Button
                    href=cta_href
                    target="_blank"
                    rel="noopener noreferrer"
                    variant=`contained
                    size=`small
                >
                    {cta_label |> React.string}
                </Button>
            </div>
        </Paper>
    </Slide>
}
