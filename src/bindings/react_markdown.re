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

/**
 * An entry from a grammar note's `toc` frontmatter: a heading's stable anchor
 * id alongside the exact Markdown heading text it belongs to.
 */
type toc_entry_js;

[@mel.obj]
external make_toc_entry_js:
    (~id: string, ~level: int, ~heading: string, ~title: string, unit) => toc_entry_js = "";

/**
 * Assign `id` attributes to heading elements (h1-h6) whose rendered text
 * matches a `heading` entry from the Markdown's TOC frontmatter, so the
 * table of contents can link to them with plain `#id` anchors.
 */
let make_rehype_heading_ids: array(toc_entry_js) => rehype_plugin = [%mel.raw {|
  function (tocEntries) {
    const idsByHeadingText = new Map();
    for (const entry of tocEntries) {
      if (!idsByHeadingText.has(entry.heading)) {
        idsByHeadingText.set(entry.heading, entry.id);
      }
    }

    function textContent(node) {
      if (node.type === "text") return node.value;
      if (Array.isArray(node.children)) {
        return node.children.map(textContent).join("");
      }
      return "";
    }

    return function rehypeHeadingIds() {
      return function transform(tree) {
        function visit(node) {
          if (!node || !Array.isArray(node.children)) return;
          for (const child of node.children) {
            if (child.type === "element" && /^h[1-6]$/.test(child.tagName)) {
              const text = textContent(child).trim();
              const id = idsByHeadingText.get(text);
              if (id) {
                child.properties = child.properties || {};
                child.properties.id = id;
              }
            }
            visit(child);
          }
        }
        visit(tree);
      };
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
