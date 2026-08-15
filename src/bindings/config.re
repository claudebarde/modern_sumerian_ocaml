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

let isDevelopment: bool =
  [%mel.raw {|import.meta.env.DEV|}];

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
};
