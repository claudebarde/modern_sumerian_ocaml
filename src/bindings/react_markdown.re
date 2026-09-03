/**
 * Minimal bindings for react-markdown 10.x.
 *
 * The JavaScript component receives the Markdown source through `children`.
 * The Reason API calls that prop `markdown` to make its expected type clear.
 * Plugin and custom-component bindings can be added separately when needed.
 */
type remark_plugin;

[@mel.module "remark-gfm"]
external remarkGfm: remark_plugin = "default";

[@mel.module "react-markdown"] [@react.component]
external make: (
    ~markdown: [@mel.as "children"] string,
    ~allowedElements: array(string)=?,
    ~disallowedElements: array(string)=?,
    ~remarkPlugins: array(remark_plugin)=?,
    ~skipHtml: bool=?,
    ~unwrapDisallowed: bool=?,
    unit,
) => React.element = "default";
