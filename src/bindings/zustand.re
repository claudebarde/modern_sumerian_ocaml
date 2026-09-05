/**
 * Minimal bindings for a Zustand vanilla store used from React.
 *
 * Keeping store creation separate from the React hook gives us a regular
 * store that can also be read and updated outside React components.
 */

/** A Zustand store containing values of type `'state`. */
type store('state);

/** Return a new state from the current state. */
type updater('state) = 'state => 'state;

/** Zustand's JavaScript `set` function passed to a store initializer. */
type set_state('state);

/** The `get` function passed to a store initializer. */
type get_state('state) = unit => 'state;

/** Stop listening to store updates. */
type unsubscribe = unit => unit;

/* Call Zustand's JavaScript setter without Melange's currying adapter. */
[%%mel.raw {|
function applyZustandUpdate(set, updater) {
  set(updater);
}
|}];

external apply_update_internal: (
  set_state('state),
  updater('state),
) => unit = "applyZustandUpdate";

let apply_update = (set, updater) => apply_update_internal(set, updater);

/**
 * Create a vanilla store.
 *
 * The initializer receives Zustand's `set`, `get`, and store API values.
 */
[@mel.module "zustand/vanilla"]
external create_store: (
  (set_state('state), get_state('state), store('state)) => 'state
) => store('state) = "createStore";

/**
 * React hook for subscribing to one selected part of the store.
 * Call this only at the top level of a React component or custom hook.
 */
[@mel.module "zustand"]
external use_store_internal: (
  store('state),
  'state => 'selected,
) => 'selected = "useStore";

/** Selector-first wrapper so a store can be passed with Reason's `|>` pipe. */
let use_store = (selector, store) => use_store_internal(store, selector);

/** Read the current state without subscribing a React component. */
[@mel.send]
external get_current_state: (
  [@mel.this] store('state)
) => 'state = "getState";

/** Update the store from its current state. */
[@mel.send]
external update_state: (
  updater('state),
  [@mel.this] store('state),
) => unit = "setState";

/** Listen to every store update and receive the new and previous states. */
[@mel.send]
external subscribe: (
  ('state, 'state) => unit,
  [@mel.this] store('state),
) => unsubscribe = "subscribe";
