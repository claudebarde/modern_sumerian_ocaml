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
