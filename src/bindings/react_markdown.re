/**
 * Minimal bindings for react-markdown 10.x.
 *
 * The JavaScript component receives the Markdown source through `children`.
 * The Reason API calls that prop `markdown` to make its expected type clear.
 * Plugin and custom-component bindings can be added separately when needed.
 */
type remark_plugin;
type remark_plugin_entry;
type rehype_plugin;

[@mel.module "remark-gfm"]
external remarkGfm: remark_plugin = "default";

external remarkPluginEntry: remark_plugin => remark_plugin_entry = "%identity";

let configureRemarkGfm: remark_plugin => remark_plugin_entry = [%mel.raw {|
  plugin => [plugin, {singleTilde: false}]
|}];

let remarkGfmWithoutSingleTilde = configureRemarkGfm(remarkGfm);

/**
 * Wrap runs of Unicode cuneiform characters in
 * <span class="cuneiforms">...</span> after Markdown has been parsed.
 *
 * Covered blocks:
 * - Cuneiform (U+12000-U+123FF)
 * - Cuneiform Numbers and Punctuation (U+12400-U+1247F)
 * - Early Dynastic Cuneiform (U+12480-U+1254F)
 */
let rehypeCuneiform: rehype_plugin = [%mel.raw {|
  function rehypeCuneiform() {
    const cuneiformRun = /([\u{12000}-\u{123FF}\u{12400}-\u{1247F}\u{12480}-\u{1254F}]+)/gu;
    const onlyCuneiform = /^[\u{12000}-\u{123FF}\u{12400}-\u{1247F}\u{12480}-\u{1254F}]+$/u;

    return function transform(tree) {
      function visit(node) {
        if (!node || !Array.isArray(node.children)) return;

        const transformedChildren = [];

        for (const child of node.children) {
          if (child.type === "text" && typeof child.value === "string") {
            const parts = child.value.split(cuneiformRun);

            for (const part of parts) {
              if (part.length === 0) continue;

              if (onlyCuneiform.test(part)) {
                transformedChildren.push({
                  type: "element",
                  tagName: "span",
                  properties: {className: ["cuneiforms x-small"]},
                  children: [{type: "text", value: part}]
                });
              } else {
                transformedChildren.push({type: "text", value: part});
              }
            }
          } else {
            visit(child);
            transformedChildren.push(child);
          }
        }

        node.children = transformedChildren;
      }

      visit(tree);
    };
  }
|}];

[@mel.module "react-markdown"] [@react.component]
external make: (
    ~markdown: [@mel.as "children"] string,
    ~allowedElements: array(string)=?,
    ~disallowedElements: array(string)=?,
    ~rehypePlugins: array(rehype_plugin)=?,
    ~remarkPlugins: array(remark_plugin_entry)=?,
    ~skipHtml: bool=?,
    ~unwrapDisallowed: bool=?,
    unit,
) => React.element = "default";
