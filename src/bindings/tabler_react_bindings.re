/**
 * Typed bindings for React components exported by @tabler/icons-react.
 *
 * Add another icon by copying the module below and changing both occurrences
 * of its JavaScript export name.
 */
module IconSearch = {
  [@mel.module "@tabler/icons-react"] [@react.component]
  external make: (
    ~size: int=?,
    ~stroke: float=?,
    ~color: string=?,
    ~className: string=?,
    ~title: string=?,
    unit
  ) => React.element = "IconSearch";
};

module IconRefresh = {
  [@mel.module "@tabler/icons-react"] [@react.component]
  external make: (
    ~size: int=?,
    ~stroke: float=?,
    ~color: string=?,
    ~className: string=?,
    ~title: string=?,
    unit
  ) => React.element = "IconRefresh";
};
