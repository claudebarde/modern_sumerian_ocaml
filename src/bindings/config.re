/**
 * Public browser configuration injected by Vite.
 *
 * Vite loads these values from .env.local during local development and from
 * the deployment environment on Netlify.
 */
let supabaseUrl: string =
  [%mel.raw {|import.meta.env.VITE_SUPABASE_URL|}];

let supabasePublishableKey: string =
  [%mel.raw {|import.meta.env.VITE_SUPABASE_PUBLISHABLE_KEY|}];

let max_keyboard_search_results: int = 30;

let colors = {
  "protonRed": "#840804",
  "nycTaxi": "#f7b732",
  "whiteSmoke": "#f5f5f5",
  "silverSetting": "#d8dadb",
  "cerealFlake": "#efd7ab",
  "crustoseLichen": "#c04e01",
  "botanicalNight": "#12403c",
  "darkRift": "#060b14",
  "pacificTeal": "#3e8083",
  "moroccanBlue": "#115674",
  "oxfordBlue": "#002147",
  "seaBlue": "#afc9dc",
  "aspenGold": "#ffd662"
};

let bookmarkColors = {
  "salmon": "#FFBE98",
  "pink": "#F05A7E",
  "teal": "#0B8494",
  "blue": "#125B9A"
};

type bookmarkValue = 
  | Pink
  | Salmon
  | Teal
  | Blue;

let get_bookmark_number = (value: bookmarkValue): int =>
  switch value {
  | Pink => 0
  | Salmon => 1
  | Teal => 2
  | Blue => 3
  };
