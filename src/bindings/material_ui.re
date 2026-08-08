/**
 * Typed bindings for React components exported by @mui/material.
 */

// TODO: once all the bindings are written, use AI to add default props like "id" to all the components

// HELPERS

type width = [
    | `xs
    | `sm
    | `md
    | `lg
    | `xl
];

type originPosition = {
    vertical: [`top | `center | `bottom],
    horizontal: [`left | `center | `right],
};

module RootComponent = {
    type t;

    external htmlElement: string => t = "%identity";
    external reactComponent: React.component('props) => t = "%identity";
};

module Color = {
    type t;

    external fromString: string => t = "%identity";

    let inherit_ = fromString("inherit");
    let primary = fromString("primary");
    let secondary = fromString("secondary");
    let success = fromString("success");
    let error = fromString("error");
    let info = fromString("info");
    let warning = fromString("warning");
    let transparent = fromString("transparent");
};

// MATERIAL UI COMPONENTS

module Accordion = {
    [@mel.module "@mui/material/Accordion"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~defaultExpanded: bool=?,
        ~disabled: bool=?,
        ~disableGutters: bool=?,
        ~expanded: bool=?,
        ~onChange: (React.Event.Synthetic.t, bool) => unit=?,
        // TODO: slots and slotProps are not supported yet
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module AccordionActions = {
    [@mel.module "@mui/material/AccordionActions"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~disableSpacing: bool=?,
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module AccordionDetails = {
    [@mel.module "@mui/material/AccordionDetails"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module AccordionSummary = {
    [@mel.module "@mui/material/AccordionSummary"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~expandIcon: React.element=?,
        ~focusVisibleClassName: string=?,
        // TODO: slots and slotProps are not supported yet
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module Alert = {
    module IconMapping = {
        type t;

        [@mel.obj]
        external make: (
            ~error: React.element=?,
            ~info: React.element=?,
            ~success: React.element=?,
            ~warning: React.element=?,
            unit
        ) => t = "";
    };

    [@mel.module "@mui/material/Alert"] [@react.component]
    external make: (
        ~action: React.element=?,
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~closeText: string=?,
        ~color: Color.t=?,
        ~icon: [@mel.unwrap] [
            | `Element(React.element)
            | `Boolean(bool)
        ]=?,
        ~iconMapping: IconMapping.t=?,
        ~onClose: (React.Event.Mouse.t => unit)=?,
        ~role: string=?,
        ~severity: [`error | `info | `success | `warning]=?,
        // TODO: slots and slotProps are not supported yet
        ~sx: Js.t({..})=?,
        ~variant: [`filled | `outlined | `standard]=?, // TODO: a string can be passed to variant
        unit
    ) => React.element = "default";
};

module AppBar = {
    [@mel.module "@mui/material/AppBar"] [@react.component]
    external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~elevation: int=?,
    ~position: [`fixed | `absolute | `sticky | `static | `relative]=?,
    ~enableColorOnDark: bool=?,
    ~square: bool=?,
    ~sx: Js.t({..})=?,
    unit
    ) => React.element = "default";
};

// https://mui.com/material-ui/api/autocomplete/
module Autocomplete = {
    module BlurOnSelect = {
        type t;

        external fromBool: bool => t = "%identity";
        external fromString: string => t = "%identity";

        let mouse = fromString("mouse");
        let touch = fromString("touch");
    };

    module ForcePopupIcon = {
        type t;

        external fromBool: bool => t = "%identity";
        external fromString: string => t = "%identity";

        let auto = fromString("auto");
    };

    module OptionKey = {
        type t;

        external fromString: string => t = "%identity";
        external fromInt: int => t = "%identity";
    };

    type renderOptionState = {
        . "inputValue": string,
        "index": int,
        "selected": bool,
    };

    [@mel.module "@mui/material/Autocomplete"] [@react.component]
    external make: (
        ~autoComplete: bool=?,
        ~autoHighlight: bool=?,
        ~autoSelect: bool=?,
        ~blurOnSelect: BlurOnSelect.t=?,
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~clearIcon: React.element=?,
        ~clearOnBlur: bool=?,
        ~clearOnEscape: bool=?,
        ~clearText: string=?,
        ~closeText: string=?,
        ~defaultValue: 'value=?,
        ~disableClearable: bool=?,
        ~disableCloseOnSelect: bool=?,
        ~disabled: bool=?,
        ~disabledItemsFocusable: bool=?,
        ~disableListWrap: bool=?,
        ~disablePortal: bool=?,
        ~filterOptions: ('options, 'state) => array('option)=?,
        ~filterSelectedOptions: bool=?,
        ~forcePopupIcon: ForcePopupIcon.t=?,
        ~freeSolo: bool=?,
        ~fullWidth: bool=?,
        ~getLimitTagsText: (int => React.element)=?,
        ~getOptionDisabled: ('option => bool)=?,
        ~getOptionKey: ('option => OptionKey.t)=?,
        ~getOptionLabel: ('option => string)=?,
        ~groupBy: ('option => string)=?,
        ~handleHomeEndKeys: bool=?,
        ~id: string=?,
        ~includeInputInList: bool=?,
        ~inputValue: string=?,
        ~isOptionEqualToValue: ('value, 'value => bool)=?,
        ~limitTags: int=?,
        ~loading: bool=?,
        ~loadingText: React.element=?,
        ~multiple: bool=?,
        ~noOptionsText: React.element=?,
        ~onChange: (React.Event.Synthetic.t, Js.Nullable.t('value)) => unit=?,
        ~onClose: (React.Event.Synthetic.t, string) => unit=?,
        ~onHighlightChange: (
            React.Event.Synthetic.t,
            Js.Nullable.t('option),
            [`keyboard | `mouse | `touch],
        ) => unit=?,
        ~onInputChange: (React.Event.Synthetic.t, string) => unit=?,
        ~onOpen: (React.Event.Synthetic.t => unit)=?,
        ~_open: bool=?,
        ~openOnFocus: bool=?,
        ~openText: string=?,
        ~options: array('option)=?,
        ~popupIcon: React.element=?,
        ~readOnly: bool=?,
        ~renderGroup: ('params => React.element)=?,
        ~renderInput: ('params => React.element)=?,
        ~renderOption: (
            Js.t({..}),
            'option,
            renderOptionState,
            Js.t({..}),
        ) => React.element=?,
        ~renderValue: (
            'value,
            Js.t({..}) => Js.t({..}),
            Js.t({..}),
        ) => React.element=?,
        ~resetHighlightOnMouseLeave: bool=?,
        ~selectOnFocus: bool=?,
        ~size: [`small | `medium]=?, // TODO: a string can be passed to size
        ~sx: Js.t({..})=?,
        ~value: Js.Nullable.t('value)=?,
        unit
    ) => React.element = "default";
};

module Avatar = {
    [@mel.module "@mui/material/Avatar"] [@react.component]
    external make: (
        ~alt: string=?,
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~component: RootComponent.t=?,
        ~sizes: string=?,
        ~src: string=?,
        ~srcSet: string=?,
        ~variant: [`circular | `rounded | `square]=?, // TODO: a string can be passed to variant
        // TODO: slots and slotProps are not supported yet
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module Box = {
    [@mel.module "@mui/material/Box"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~className: string=?,
        ~component: RootComponent.t=?,
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module Button = {
    [@mel.module "@mui/material/Button"] [@react.component]
    external make: (
        ~ariaLabel: [@mel.as "aria-label"] string=?,
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~color: Color.t=?,
        ~component: RootComponent.t=?,
        ~disabled: bool=?,
        ~disableElevation: bool=?,
        ~disableFocusRipple: bool=?,
        ~disableRipple: bool=?,
        ~endIcon: React.element=?,
        ~fullWidth: bool=?,
        ~href: string=?,
        ~onClick: (React.Event.Mouse.t => unit)=?,
        ~rel: string=?,
        ~size: [`small | `medium | `large]=?, // TODO: a string can be passed to size
        ~startIcon: React.element=?,
        ~target: string=?,
        ~variant: [`text | `outlined | `contained]=?, // TODO: a string can be passed to variant
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module Card = {
    [@mel.module "@mui/material/Card"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~raised: bool=?,
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module CardActions = {
    [@mel.module "@mui/material/CardActions"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~disableSpacing: bool=?,
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module CardActionArea = {
    [@mel.module "@mui/material/CardActionArea"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        // TODO: slots and slotProps are not supported yet
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module CardContent = {
    [@mel.module "@mui/material/CardContent"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~component: RootComponent.t=?,
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module CardHeader = {
    [@mel.module "@mui/material/CardHeader"] [@react.component]
    external make: (
        ~action: React.element=?,
        ~avatar: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~component: RootComponent.t=?,
        ~disableTypography: bool=?,
        // TODO: slots and slotProps are not supported yet
        ~subheader: React.element=?,
        ~sx: Js.t({..})=?,
        ~title: React.element=?,
        unit
    ) => React.element = "default";
};

module CardMedia = {
    [@mel.module "@mui/material/CardMedia"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~component: RootComponent.t=?,
        ~image: string=?,
        ~src: string=?,
        ~sx: Js.t({..})=?,
        unit
    ) => React.element = "default";
};

module Checkbox = {
    [@mel.module "@mui/material/Checkbox"] [@react.component]
    external make: (
        ~checked: bool=?,
        ~checkedIcon: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~color: Color.t=?,
        ~defaultChecked: bool=?,
        ~disabled: bool=?,
        ~disableRipple: bool=?,
        ~icon: React.element=?,
        ~id: string=?,
        ~indeterminate: bool=?,
        ~indeterminateIcon: React.element=?,
        ~onChange: (React.Event.Form.t => unit)=?,
        ~required: bool=?,
        ~size: [`small | `medium]=?, // TODO: a string can be passed to size
        ~sx: Js.t({..})=?,
        // TODO: slots and slotProps are not supported yet
        ~value: 'value=?,
        unit
    ) => React.element = "default";
};

module Chip = {
    [@mel.module "@mui/material/Chip"] [@react.component]
    external make: (
        ~avatar: React.element=?,
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~clickable: bool=?,
        ~color: Color.t=?,
        ~component: RootComponent.t=?,
        ~deleteIcon: React.element=?,
        ~disabled: bool=?,
        ~icon: React.element=?,
        ~label: string=?,
        ~nativeButton: bool=?,
        ~onClick: (React.Event.Mouse.t => unit)=?,
        ~onDelete: (React.Event.Mouse.t => unit)=?,
        ~size: [`small | `medium]=?, // TODO: a string can be passed to size
        ~skipFocusWhenDisabled: bool=?,
        // TODO: slots and slotProps are not supported yet
        ~sx: Js.t({..})=?,
        ~variant: [`filled | `outlined]=?, // TODO: a string can be passed to variant
        unit
    ) => React.element = "default";
};

module Collapse = {
    [@mel.module "@mui/material/Collapse"] [@react.component]
    external make: (
        ~addEndListener: (unit => unit)=?,
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~collapsedSize: [@mel.unwrap] [
            | `String(string)
            | `Number(int)
        ]=?,
        ~component: RootComponent.t=?,
        ~disablePrefersReducedMotion: bool=?,
        ~orientation: [`horizontal | `vertical]=?,
        ~in_: [@mel.as "in"] bool=?,
        ~sx: Js.t({..})=?,
        ~timeout: [@mel.unwrap] [
            | `auto
            | `Number(int)
            | `Object(Js.t({..}))
        ]=?,
        unit
    ) => React.element = "default";
};

module Container = {
  [@mel.module "@mui/material/Container"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~disableGutters: bool=?,
    ~fixed: bool=?,
    ~maxWidth: (
        [@mel.unwrap] [
            | `Width(width)
            | `Disabled(bool)
        ]
        )=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module Divider = {
  [@mel.module "@mui/material/Divider"] [@react.component]
  external make: (
    ~absolute: bool=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~flexItem: bool=?,
    ~orientation: [`horizontal | `vertical]=?,
    ~sx: Js.t({..})=?,
    ~textAlign: [`center | `right | `left]=?,
    ~variant: [`fullWidth | `inset | `middle]=?,
    unit
  ) => React.element = "default";
};

module Drawer = {
  [@mel.module "@mui/material/Drawer"] [@react.component]
  external make: (
    ~anchor: [`left | `top | `right | `bottom]=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~elevation: int=?,
    ~hideBackdrop: bool=?,
    ~_ModalProps: Js.t({..})=?,
    ~onClose: (React.Event.Synthetic.t => unit)=?,
    ~_open: bool=?,
    // TODO: slots and slotProps are not supported yet
    ~sx: Js.t({..})=?,
    ~transitionDuration: [@mel.unwrap] [
        | `auto
        | `Number(int)
        | `Object(Js.t({..}))
    ]=?,
    ~variant: [`permanent | `persistent | `temporary]=?,
    unit
  ) => React.element = "default";
};

module Fab = {
  [@mel.module "@mui/material/Fab"] [@react.component]
  external make: (
    ~ariaLabel: [@mel.as "aria-label"] string=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~component: RootComponent.t=?,
    ~disabled: bool=?,
    ~disableFocusRipple: bool=?,
    ~disableRipple: bool=?,
    ~href: string=?,
    ~onClick: (React.Event.Mouse.t => unit)=?,
    ~rel: string=?,
    ~size: [`small | `medium | `large]=?, // TODO: a string can be passed to size
    ~sx: Js.t({..})=?,
    ~target: string=?,
    ~variant: [`circular | `extended]=?, // TODO: a string can be passed to variant
    unit
  ) => React.element = "default";
};

module FormControl = {
  [@mel.module "@mui/material/FormControl"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~component: RootComponent.t=?,
    ~disabled: bool=?,
    ~error: bool=?,
    ~focused: bool=?,
    ~fullWidth: bool=?,
    ~hiddenLabel: bool=?,
    ~margin: [`dense | `normal | `none]=?,
    ~required: bool=?,
    ~size: [`small | `medium]=?,
    ~sx: Js.t({..})=?,
    ~variant: [`standard | `outlined | `filled]=?,
    unit
  ) => React.element = "default";
};

module FormControlLabel = {
  [@mel.module "@mui/material/FormControlLabel"] [@react.component]
  external make: (
    ~checked: bool=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~control: React.element,
    ~disabled: bool=?,
    ~disableTypography: bool=?,
    ~inputRef: React.ref(Js.Nullable.t(Dom.element))=?,
    ~label: React.element,
    ~labelPlacement: [[@mel.as "end"] `end_ | `start | `top | `bottom]=?,
    ~onChange: (React.Event.Form.t => unit)=?,
    ~required: bool=?,
    ~sx: Js.t({..})=?,
    ~value: 'value=?,
    unit
  ) => React.element = "default";
};

module FormGroup = {
  [@mel.module "@mui/material/FormGroup"] [@react.component]
  external make: (
    ~ariaLabelledby: [@mel.as "aria-labelledby"] string=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~row: bool=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module FormLabel = {
  [@mel.module "@mui/material/FormLabel"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~component: RootComponent.t=?,
    ~disabled: bool=?,
    ~error: bool=?,
    ~filled: bool=?,
    ~focused: bool=?,
    ~id: string=?,
    ~required: bool=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module Grid = {
    module ResponsiveSize = {
        type t;

        [@mel.obj]
        external make: (
            ~xs: int=?,
            ~sm: int=?,
            ~md: int=?,
            ~lg: int=?,
            ~xl: int=?,
            unit
        ) => t = "";
    };

  [@mel.module "@mui/material/Grid"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~columns: [@mel.unwrap] [
        | `Number(int)
        | `Array(array(int))
        | `Object(Js.t({..}))
    ]=?,
    ~columnSpacing: int=?,
    ~component: RootComponent.t=?,
    ~container: bool=?,
    ~direction: [`row | `rowReverse | `column | `columnReverse]=?,
    ~offset: [@mel.unwrap] [
        | `String(string)
        | `Number(int)
        | `Object(Js.t({..}))
    ]=?,
    ~rowSpacing: int=?,
    ~size: [@mel.unwrap] [
        | `String(string)
        | `Number(int)
        | `Object(ResponsiveSize.t)
        | `Boolean(bool)
    ]=?,
    ~spacing: [@mel.unwrap] [
        | `String(string)
        | `Number(int)
        | `Object(ResponsiveSize.t)
        // TODO: implement Array<string | number> support for spacing
    ]=?,
    ~sx: Js.t({..})=?,
    ~wrap: [`nowrap | `wrap | `wrapReverse]=?,
    unit
  ) => React.element = "default";
};

module IconButton = {
  [@mel.module "@mui/material/IconButton"] [@react.component]
  external make: (
    ~ariaLabel: [@mel.as "aria-label"] string=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~component: RootComponent.t=?,
    ~disabled: bool=?,
    ~disableElevation: bool=?,
    ~disableFocusRipple: bool=?,
    ~disableRipple: bool=?,
    ~edge: [`start | `end_ | `false_]=?,
    ~endIcon: React.element=?,
    ~fullWidth: bool=?,
    ~href: string=?,
    ~loading: bool=?,
    ~loadingIndicator: React.element=?,
    ~onClick: (React.Event.Mouse.t => unit)=?,
    ~rel: string=?,
    ~size: [`small | `medium | `large]=?, // TODO: a string can be passed to size
    ~startIcon: React.element=?,
    ~target: string=?,
    ~variant: [`text | `outlined | `contained]=?, // TODO: a string can be passed to variant
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module InputBase = {
  [@mel.module "@mui/material/InputBase"] [@react.component]
  external make: (
    ~autoComplete: string=?,
    ~autoFocus: bool=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~defaultValue: 'value=?,
    ~disabled: bool=?,
    ~disableInjectingGlobalStyles: bool=?,
    ~endAdornment: React.element=?,
    ~error: bool=?,
    ~fullWidth: bool=?,
    ~id: string=?,
    ~inputComponent: RootComponent.t=?,
    ~inputProps: Js.t({..})=?,
    ~inputRef: React.ref(Js.Nullable.t(Dom.element))=?,
    ~margin: [`dense | `none]=?,
    ~maxRows: int=?,
    ~minRows: int=?,
    ~multiline: bool=?,
    ~name: string=?,
    ~onBlur: (React.Event.Focus.t => unit)=?,
    ~onChange: (React.Event.Form.t => unit)=?,
    ~onInvalid: (React.Event.Form.t => unit)=?,
    ~onKeyDown: (React.Event.Keyboard.t => unit)=?,
    ~onKeyUp: (React.Event.Keyboard.t => unit)=?,
    ~onPaste: (React.Event.Clipboard.t => unit)=?,
    ~placeholder: string=?,
    ~readOnly: bool=?,
    ~required: bool=?,
    ~rows: int=?,
    ~size: [`small | `medium]=?, // TODO: a string can be passed to size
    // TODO: slots and slotProps are not supported yet
    ~startAdornment: React.element=?,
    ~sx: Js.t({..})=?,
    ~type_: string=?,
    ~value: 'value=?,
    unit
  ) => React.element = "default";
};

module InputLabel = {
  [@mel.module "@mui/material/InputLabel"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~disableAnimation: bool=?,
    ~disabled: bool=?,
    ~error: bool=?,
    ~focused: bool=?,
    ~id: string=?,
    ~margin: [`dense | `none]=?,
    ~required: bool=?,
    ~shrink: bool=?,
    ~size: [`small | `medium]=?, // TODO: a string can be passed to size
    ~variant: [`standard | `outlined | `filled]=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module Link = {
    type variant = [
        | `inherit_
        | `body1
        | `body2
        | `button
        | `caption
        | `h1
        | `h2
        | `h3
        | `h4
        | `h5
        | `h6
        | `overline
        | `subtitle1
        | `subtitle2
    ];

  [@mel.module "@mui/material/Link"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~component: RootComponent.t=?,
    ~href: string=?,
    ~_TypographyClasses: Js.t({..})=?,
    ~underline: [`none | `hover | `always]=?,
    ~sx: Js.t({..})=?,
    ~variant: variant=?,
    unit
  ) => React.element = "default";
};

module List = {
  [@mel.module "@mui/material/List"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~dense: bool=?,
    ~disablePadding: bool=?,
    ~subheader: React.element=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module ListItem = {
  [@mel.module "@mui/material/ListItem"] [@react.component]
  external make: (
    ~alignItems: [`flexStart | `center]=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~dense: bool=?,
    ~disableGutters: bool=?,
    ~disablePadding: bool=?,
    ~divider: bool=?,
    ~href: string=?,
    ~onClick: (React.Event.Synthetic.t => unit)=?,
    ~secondarAction: React.element=?,
    // TODO: slots and slotProps are not supported yet
    ~sx: Js.t({..})=?,
    ~target: string=?,
    unit
  ) => React.element = "default";
};

module ListItemAvatar = {
  [@mel.module "@mui/material/ListItemAvatar"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module ListItemButton = {
  [@mel.module "@mui/material/ListItemButton"] [@react.component]
  external make: (
    ~alignItems: [`flexStart | `center]=?,
    ~autoFocus: bool=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~dense: bool=?,
    ~disabled: bool=?,
    ~disableGutters: bool=?,
    ~divider: bool=?,
    ~focusVisibleClassName: string=?,
    ~onClick: (React.Event.Synthetic.t => unit)=?,
    ~selected: bool=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module ListItemIcon = {
  [@mel.module "@mui/material/ListItemIcon"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module ListItemText = {
  [@mel.module "@mui/material/ListItemText"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~disableTypography: bool=?,
    ~primary: React.element=?,
    ~inset: bool=?,
    ~secondary: React.element=?,
    ~sx: Js.t({..})=?,
    // TODO: slots and slotProps are not supported yet
    unit
  ) => React.element = "default";
};

module ListSubheader = {
  [@mel.module "@mui/material/ListSubheader"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: [`default | `primary | `inherit_]=?,
    ~component: RootComponent.t=?,
    ~disableGutters: bool=?,
    ~disableSticky: bool=?,
    ~inset: bool=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module Menu = {
  [@mel.module "@mui/material/Menu"] [@react.component]
  external make: (
    ~anchorOrigin: originPosition=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~anchorEl: Js.Nullable.t(Dom.element)=?,
    ~disableAutoFocusItem: bool=?,
    ~onClose: (React.Event.Synthetic.t => unit)=?,
    ~_open: bool=?,
    ~_PopoverClasses: Js.t({..})=?,
    ~slotProps: Js.t({..})=?,
    ~slots: Js.t({..})=?,
    ~sx: Js.t({..})=?,
    ~transformOrigin: originPosition=?,
    ~transitionDuration: [@mel.unwrap] [
        | `auto
        | `Number(int)
        | `Object(Js.t({..}))
    ]=?,
    ~variant: [`menu | `selectedMenu]=?,
    unit
  ) => React.element = "default";
};

module MenuItem = {
  [@mel.module "@mui/material/MenuItem"] [@react.component]
  external make: (
    ~autoFocus: bool=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~dense: bool=?,
    ~disableGutters: bool=?,
    ~divider: bool=?,
    ~focusVisibleClassName: string=?,
    ~onClick: (React.Event.Synthetic.t => unit)=?,
    ~selected: bool=?,
    ~sx: Js.t({..})=?,
    ~value: 'value=?,
    unit
  ) => React.element = "default";
};

module Paper = {
  [@mel.module "@mui/material/Paper"] [@react.component]
  external make: (
    ~ariaLabel: [@mel.as "aria-label"] string=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~elevation: int=?,
    ~square: bool=?,
    ~sx: Js.t({..})=?,
    ~variant: [`elevation | `outlined]=?,
    unit
  ) => React.element = "default";
};

module Popover = {
    type anchorPos = { left: int, top: int };

    module Actions = {
        type t;

        [@mel.send]
        external update_position: ([@mel.this] t) => unit = "updatePosition";
    };

    [@mel.module "@mui/material/Popover"] [@react.component]
    external make: (
        ~action: React.ref(Js.Nullable.t(Actions.t))=?,
        ~anchorEl: Js.Nullable.t(Dom.element)=?,
        ~anchorOrigin: originPosition=?,
        ~anchorPosition: anchorPos=?,
        ~anchorReference: [`anchorEl | `anchorPosition | `none]=?,
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~container: Dom.element=?,
        ~disableAutoFocus: bool=?,
        ~disableScrollLock: bool=?,
        ~elevation: int=?,
        ~marginThreshold: int=?,
        ~onClose: (React.Event.Synthetic.t => unit)=?,
        ~_open: bool=?,
        ~_PaperProps: Js.t({..})=?,
        ~_PopoverClasses: Js.t({..})=?,
        ~slotProps: Js.t({..})=?,
        ~slots: Js.t({..})=?,
        ~sx: Js.t({..})=?,
        ~transformOrigin: originPosition=?,
        ~transitionDuration: [@mel.unwrap] [
            | `auto
            | `Number(int)
            | `Object(Js.t({..}))
        ]=?,
        unit
    ) => React.element = "default";
};

module Radio = {
  [@mel.module "@mui/material/Radio"] [@react.component]
  external make: (
    ~checked: bool=?,
    ~checkedIcon: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~disabled: bool=?,
    ~disableRipple: bool=?,
    ~icon: React.element=?,
    ~id: string=?,
    ~name: string=?,
    ~onChange: (React.Event.Form.t => unit)=?,
    ~onClick: (React.Event.Mouse.t => unit)=?,
    ~required: bool=?,
    ~size: [`small | `medium]=?,
    // TODO: slots and slotProps are not supported yet
    ~sx: Js.t({..})=?,
    ~value: 'value=?,
    unit
  ) => React.element = "default";
};

module RadioGroup = {
  [@mel.module "@mui/material/RadioGroup"] [@react.component]
  external make: (
    ~ariaLabelledby: [@mel.as "aria-labelledby"] string=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~defaultValue: 'value=?,
    ~name: string=?,
    ~onChange: (React.Event.Form.t => unit)=?,
    ~row: bool=?,
    ~sx: Js.t({..})=?,
    ~value: 'value=?,
    unit
  ) => React.element = "default";
};

module Select = {
  module Value = {
    type t;

    external fromString: string => t = "%identity";
    external fromObject: Js.t({..}) => t = "%identity";
    external fromOption: option('value) => t = "%identity";
  };

  [@mel.module "@mui/material/Select"] [@react.component]
  external make: (
    ~autoWidth: bool=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~defaultOpen: bool=?,
    ~defaultValue: Js.t({..})=?,
    ~disabled: bool=?,
    ~displayEmpty: bool=?,
    ~_IconComponent: React.component('props)=?,
    ~id: string=?,
    ~input: React.element=?,
    ~inputProps: Js.t({..})=?,
    ~label: React.element=?,
    ~labelId: string=?,
    ~_MenuProps: Js.t({..})=?,
    ~multiple: bool=?,
    ~native: bool=?,
    ~onChange: (
      (Js.t({..}), Js.Undefined.t(React.element)) => unit
    )=?,
    ~onClose: (React.Event.Synthetic.t => unit)=?,
    ~onOpen: (React.Event.Synthetic.t => unit)=?,
    ~_open: bool=?,
    ~renderValue: (Js.t({..}) => React.element)=?,
    ~_SelectDisplayProps: Js.t({..})=?,
    ~value: Value.t,
    ~variant: [`standard | `outlined | `filled]=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module Snackbar = {
  module TransitionDuration = {
    type t;

    [@mel.obj]
    external make: (
      ~appear: int=?,
      ~enter: int=?,
      ~exit: int=?,
      unit
    ) => t = "";
  };

  [@mel.module "@mui/material/Snackbar"] [@react.component]
  external make: (
    ~action: React.element=?,
    ~anchorOrigin: originPosition=?,
    ~autoHideDuration: int=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~disableWindowBlurListener: bool=?,
    ~message: React.element=?,
    ~onClose: (React.Event.Synthetic.t => unit)=?,
    ~_open: bool=?,
    ~resumeHideDuration: int=?,
    // TODO: slots and slotProps are not supported yet
    ~transitionDuration: [@mel.unwrap] [
      | `Number(int)
      | `Object(TransitionDuration.t)
    ]=?,
    unit
  ) => React.element = "default";
};

module Stack = {
    module ResponsiveDirection = {
        type t;

        [@mel.obj]
        external make: (
            ~xs: [`row | `column]=?,
            ~sm: [`row | `column]=?,
            ~md: [`row | `column]=?,
            ~lg: [`row | `column]=?,
            ~xl: [`row | `column]=?,
            unit
        ) => t = "";
    };

    module ResponsiveSpacing = {
        type t;

        [@mel.obj]
        external make: (
            ~xs: int=?,
            ~sm: int=?,
            ~md: int=?,
            ~lg: int=?,
            ~xl: int=?,
            unit
        ) => t = "";
    };

    [@mel.module "@mui/material/Stack"] [@react.component]
    external make: (
        ~children: React.element=?,
        ~classes: Js.t({..})=?,
        ~className: string=?,
        ~component: RootComponent.t=?,
        ~direction: [@mel.unwrap] [
            | `row
            | `column
            | `rowReverse
            | `columnReverse
            | `Object(ResponsiveDirection.t)
        ]=?,
        ~divider: React.element=?,
        ~spacing: [@mel.unwrap] [
            | `Number(int)
            | `String(string)
            | `Object(ResponsiveSpacing.t)
        ]=?,
        ~sx: Js.t({..})=?,
        ~useFlexGap: bool=?,
        unit
    ) => React.element = "default";
};

module Switch = {
  [@mel.module "@mui/material/Switch"] [@react.component]
  external make: (
    ~checked: bool=?,
    ~checkedIcon: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~defaultChecked: bool=?,
    ~disabled: bool=?,
    ~disableRipple: bool=?,
    ~icon: React.element=?,
    ~id: string=?,
    ~onChange: (React.Event.Form.t => unit)=?,
    ~required: bool=?,
    ~size: [`small | `medium]=?,
    // TODO: slots and slotProps are not supported yet
    ~sx: Js.t({..})=?,
    ~value: 'value=?,
    unit
  ) => React.element = "default";
};

module Table = {
  [@mel.module "@mui/material/Table"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~padding: [`checkbox | `none | `normal]=?,
    ~size: [`small | `medium]=?,
    ~stickyHeader: bool=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module TableBody = {
  [@mel.module "@mui/material/TableBody"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module TableCell = {
  [@mel.module "@mui/material/TableCell"] [@react.component]
  external make: (
    ~align: [@mel.string] [
      | [@mel.as "inherit"] `inherit_
      | `left
      | `center
      | `right
      | `justify
    ]=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~padding: [`checkbox | `none | `normal]=?,
    ~scope: string=?,
    ~size: [`small | `medium]=?,
    ~sortDirection: [`asc | `desc]=?,
    ~sx: Js.t({..})=?,
    ~variant: [`head | `body | `footer]=?,
    unit
  ) => React.element = "default";
};

module TableContainer = {
  [@mel.module "@mui/material/TableContainer"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module TableFooter = {
  [@mel.module "@mui/material/TableFooter"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module TableHead = {
  [@mel.module "@mui/material/TableHead"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module TablePagination = {
  [@mel.module "@mui/material/TablePagination"] [@react.component]
  external make: (
    ~_ActionsComponent: React.component('props)=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~count: int=?,
    ~disabled: bool=?,
    ~getItemAriaLabel: ([`first | `last | `next | `previous] => string)=?,
    ~labelDisplayedRows: ({. "from": int, "to": int, "count": int} => React.element)=?,
    ~labelRowsPerPage: React.element=?,
    ~onPageChange: ((React.Event.Synthetic.t, int) => unit)=?,
    ~onRowsPerPageChange: (React.Event.Form.t => unit)=?,
    ~page: int=?,
    ~rowsPerPage: int=?,
    ~rowsPerPageOptions: array(int)=?,
    ~showFirstButton: bool=?,
    ~showLastButton: bool=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module TablePaginationActions = {
  [@mel.module "@mui/material/TablePaginationActions"] [@react.component]
  external make: (
    ~getItemAriaLabel: ([`first | `last | `next | `previous] => string)=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~disabled: bool=?,
    unit
  ) => React.element = "default";
};

module TableRow = {
  [@mel.module "@mui/material/TableRow"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~component: RootComponent.t=?,
    ~hover: bool=?,
    ~selected: bool=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module TableSortLabel = {
  [@mel.module "@mui/material/TableSortLabel"] [@react.component]
  external make: (
    ~active: bool=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~direction: [`asc | `desc]=?,
    ~hideSortIcon: bool=?,
    ~_IconComponent: React.component('props)=?,
    ~onClick: (React.Event.Synthetic.t => unit)=?,
    // TODO: slots and slotProps are not supported yet
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module TextField = {
  [@mel.module "@mui/material/TextField"] [@react.component]
  external make: (
    ~autoComplete: string=?,
    ~autoFocus: bool=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~defaultValue: string=?,
    ~disabled: bool=?,
    ~error: bool=?,
    ~fullWidth: bool=?,
    ~helperText: React.element=?,
    ~id: string=?,
    ~inputRef: React.ref(Js.Nullable.t(Dom.element))=?,
    ~label: React.element=?,
    ~margin: [`none | `dense | `normal]=?,
    ~maxRows: int=?,
    ~minRows: int=?,
    ~multiline: bool=?,
    ~name: string=?,
    ~onChange: (React.Event.Form.t => unit)=?,
    ~onKeyDown: (React.Event.Keyboard.t => unit)=?,
    ~onKeyUp: (React.Event.Keyboard.t => unit)=?,
    ~onKeyEnter: (React.Event.Keyboard.t => unit)=?,
    ~onPaste: (React.Event.Clipboard.t => unit)=?,
    ~placeholder: string=?,
    ~required: bool=?,
    ~rows: int=?,
    ~select: bool=?,
    ~size: [`small | `medium]=?,
    ~sx: Js.t({..})=?,
    // TODO: slots and slotProps are not supported yet
    ~type_: string=?,
    ~value: string=?,
    ~variant: [`standard | `outlined | `filled]=?,
    unit
  ) => React.element = "default";
};

module ToggleButton = {
  [@mel.module "@mui/material/ToggleButton"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~disabled: bool=?,
    ~disableFocusRipple: bool=?,
    ~fullWidth: bool=?,
    ~onChange: ((React.Event.Mouse.t, 'value) => unit)=?,
    ~onClick: ((React.Event.Mouse.t, 'value) => unit)=?,
    ~selected: bool=?,
    ~size: [`small | `medium | `large]=?,
    ~sx: Js.t({..})=?,
    ~value: 'value,
    unit
  ) => React.element = "default";
};

module ToggleButtonGroup = {
  [@mel.module "@mui/material/ToggleButtonGroup"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~disabled: bool=?,
    ~exclusive: bool=?,
    ~fullWidth: bool=?,
    ~onChange: ((React.Event.Mouse.t, 'value) => unit)=?,
    ~orientation: [`horizontal | `vertical]=?,
    ~size: [`small | `medium | `large]=?,
    ~sx: Js.t({..})=?,
    ~value: 'value,
    unit
  ) => React.element = "default";
};

module Toolbar = {
    module Variant = {
        type t;

        external fromString: string => t = "%identity";

        let dense = fromString("dense");
        let regular = fromString("regular");
    };

  [@mel.module "@mui/material/Toolbar"] [@react.component]
  external make: (
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~disableGutters: bool=?,
    ~variant: Variant.t=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module Typography = {
    module Variant = {
        type t;

        external fromString: string => t = "%identity";

        let h1 = fromString("h1");
        let h2 = fromString("h2");
        let h3 = fromString("h3");
        let h4 = fromString("h4");
        let h5 = fromString("h5");
        let h6 = fromString("h6");
        let subtitle1 = fromString("subtitle1");
        let subtitle2 = fromString("subtitle2");
        let body1 = fromString("body1");
        let body2 = fromString("body2");
        let caption = fromString("caption");
        let button = fromString("button");
        let overline = fromString("overline");
        let inherit_ = fromString("inherit");
    };

    module Color = {
        type t;

        external fromString: string => t = "%identity";

        let primary = fromString("primary");
        let secondary = fromString("secondary");
        let textPrimary = fromString("textPrimary");
        let textSecondary = fromString("textSecondary");
        let success = fromString("success");
        let error = fromString("error");
        let info = fromString("info");
        let warning = fromString("warning");
        let textDisabled = fromString("textDisabled");
    };

  [@mel.module "@mui/material/Typography"] [@react.component]
  external make: (
    ~align: [@mel.string] [
      | [@mel.as "inherit"] `inherit_
      | `left
      | `center
      | `right
      | `justify
    ]=?,
    ~children: React.element=?,
    ~classes: Js.t({..})=?,
    ~className: string=?,
    ~color: Color.t=?,
    ~component: RootComponent.t=?,
    ~display: string=?,
    ~gutterBottom: bool=?,
    ~noWrap: bool=?,
    ~paragraph: bool=?,
    ~variant: Variant.t=?,
    ~variantMapping: Js.t({..})=?,
    ~sx: Js.t({..})=?,
    unit
  ) => React.element = "default";
};

module UseMediaQuery = {
  [@mel.module "@mui/material/useMediaQuery"]
  external use: string => bool = "default";
};

// THEME BINDINGS

module Theme = {
  type t;

  [@mel.module "@mui/material/styles"]
  external create: Js.t({..}) => t = "createTheme";
};

module ThemeProvider = {
  [@mel.module "@mui/material/styles"] [@react.component]
  external make: (
    ~theme: Theme.t,
    ~children: React.element,
    unit
  ) => React.element = "ThemeProvider";
};

module CssBaseline = {
  [@mel.module "@mui/material/CssBaseline"] [@react.component]
  external make: (
    ~enableColorScheme: bool=?,
    unit
  ) => React.element = "default";
};
