open Infixes

type tense =
    | Present
    | Past

type entry = {
  word: string;
  values: string * string * string;
}

let irregular_verbs : entry list = [
  { word = "be"; values = ("be", "was/were", "been") };
  { word = "beat"; values = ("beat", "beat", "beaten") };
  { word = "become"; values = ("become", "became", "become") };
  { word = "begin"; values = ("begin", "began", "begun") };
  { word = "bend"; values = ("bend", "bent", "bent") };
  { word = "bet"; values = ("bet", "bet", "bet") };
  { word = "bite"; values = ("bite", "bit", "bitten") };
  { word = "bleed"; values = ("bleed", "bled", "bled") };
  { word = "blow"; values = ("blow", "blew", "blown") };
  { word = "break"; values = ("break", "broke", "broken") };
  { word = "breed"; values = ("breed", "bred", "bred") };
  { word = "bring"; values = ("bring", "brought", "brought") };
  { word = "build"; values = ("build", "built", "built") };
  { word = "burn"; values = ("burn", "burned/ burnt", "burned/ burnt") };
  { word = "burst"; values = ("burst", "burst", "burst") };
  { word = "buy"; values = ("buy", "bought", "bought") };
  { word = "catch"; values = ("catch", "caught", "caught") };
  { word = "choose"; values = ("choose", "chose", "chosen") };
  { word = "come"; values = ("come", "came", "come") };
  { word = "cost"; values = ("cost", "cost", "cost") };
  { word = "cut"; values = ("cut", "cut", "cut") };
  { word = "do"; values = ("do", "did", "done") };
  { word = "draw"; values = ("draw", "drew", "drawn") };
  { word = "dream"; values = ("dream", "dreamed/ dreamt", "dreamed/ dreamt") };
  { word = "drink"; values = ("drink", "drank", "drunk") };
  { word = "drive"; values = ("drive", "drove", "driven") };
  { word = "eat"; values = ("eat", "ate", "eaten") };
  { word = "fall"; values = ("fall", "fell", "fallen") };
  { word = "feed"; values = ("feed", "fed", "fed") };
  { word = "feel"; values = ("feel", "felt", "felt") };
  { word = "fight"; values = ("fight", "fought", "fought") };
  { word = "find"; values = ("find", "found", "found") };
  { word = "fly"; values = ("fly", "flew", "flown") };
  { word = "forget"; values = ("forget", "forgot", "forgotten") };
  { word = "forgive"; values = ("forgive", "forgave", "forgiven") };
  { word = "freeze"; values = ("freeze", "froze", "frozen") };
  { word = "get"; values = ("get", "got", "gotten/ got") };
  { word = "give"; values = ("give", "gave", "given") };
  { word = "go"; values = ("go", "went", "gone") };
  { word = "grow"; values = ("grow", "grew", "grown") };
  { word = "hang"; values = ("hang", "hung", "hung") };
  { word = "have"; values = ("have", "had", "had") };
  { word = "hear"; values = ("hear", "heard", "heard") };
  { word = "hide"; values = ("hide", "hid", "hidden") };
  { word = "hit"; values = ("hit", "hit", "hit") };
  { word = "hold"; values = ("hold", "held", "held") };
  { word = "hurt"; values = ("hurt", "hurt", "hurt") };
  { word = "keep"; values = ("keep", "kept", "kept") };
  { word = "kneel"; values = ("kneel", "knelt", "knelt") };
  { word = "know"; values = ("know", "knew", "known") };
  { word = "lay"; values = ("lay", "laid", "laid") };
  { word = "lead"; values = ("lead", "led", "led") };
  { word = "lean"; values = ("lean", "leaned/ leant", "leaned/ leant") };
  { word = "leave"; values = ("leave", "left", "left") };
  { word = "lend"; values = ("lend", "lent", "lent") };
  { word = "let"; values = ("let", "let", "let") };
  { word = "lie"; values = ("lie", "lay", "lain") };
  { word = "light"; values = ("light", "lit", "lit") };
  { word = "lose"; values = ("lose", "lost", "lost") };
  { word = "make"; values = ("make", "made", "made") };
  { word = "mean"; values = ("mean", "meant", "meant") };
  { word = "meet"; values = ("meet", "met", "met") };
  { word = "pay"; values = ("pay", "paid", "paid") };
  { word = "put"; values = ("put", "put", "put") };
  { word = "read"; values = ("read", "read", "read") };
  { word = "ride"; values = ("ride", "rode", "ridden") };
  { word = "ring"; values = ("ring", "rang", "rung") };
  { word = "rise"; values = ("rise", "rose", "risen") };
  { word = "run"; values = ("run", "ran", "run") };
  { word = "say"; values = ("say", "said", "said") };
  { word = "see"; values = ("see", "saw", "seen") };
  { word = "seek"; values = ("seek", "sought", "sought") };
  { word = "sell"; values = ("sell", "sold", "sold") };
  { word = "send"; values = ("send", "sent", "sent") };
  { word = "set"; values = ("set", "set", "set") };
  { word = "shake"; values = ("shake", "shook", "shaken") };
  { word = "shine"; values = ("shine", "shone", "shone") };
  { word = "shoot"; values = ("shoot", "shot", "shot") };
  { word = "show"; values = ("show", "showed", "shown") };
  { word = "shut"; values = ("shut", "shut", "shut") };
  { word = "sing"; values = ("sing", "sang", "sung") };
  { word = "sink"; values = ("sink", "sank", "sunk") };
  { word = "sit"; values = ("sit", "sat", "sat") };
  { word = "sleep"; values = ("sleep", "slept", "slept") };
  { word = "speak"; values = ("speak", "spoke", "spoken") };
  { word = "speed"; values = ("speed", "sped", "sped") };
  { word = "spell"; values = ("spell", "spelt/ spelled", "spelt/ spelled") };
  { word = "spend"; values = ("spend", "spent", "spent") };
  { word = "spill"; values = ("spill", "spilt/ spilled", "spilt/ spilled") };
  { word = "spin"; values = ("spin", "spun", "spun") };
  { word = "spit"; values = ("spit", "spat", "spat") };
  { word = "split"; values = ("split", "split", "split") };
  { word = "spoil"; values = ("spoil", "spoilt/ spoiled", "spoilt/ spoiled") };
  { word = "spread"; values = ("spread", "spread", "spread") };
  { word = "stand"; values = ("stand", "stood", "stood") };
  { word = "steal"; values = ("steal", "stole", "stolen") };
  { word = "stick"; values = ("stick", "stuck", "stuck") };
  { word = "sting"; values = ("sting", "stung", "stung") };
  { word = "stink"; values = ("stink", "stank", "stunk") };
  { word = "strike"; values = ("strike", "struck", "struck") };
  { word = "swear"; values = ("swear", "swore", "sworn") };
  { word = "sweep"; values = ("sweep", "swept", "swept") };
  { word = "swim"; values = ("swim", "swam", "swum") };
  { word = "swing"; values = ("swing", "swung", "swung") };
  { word = "take"; values = ("take", "took", "taken") };
  { word = "teach"; values = ("teach", "taught", "taught") };
  { word = "tear"; values = ("tear", "tore", "torn") };
  { word = "tell"; values = ("tell", "told", "told") };
  { word = "think"; values = ("think", "thought", "thought") };
  { word = "throw"; values = ("throw", "threw", "thrown") };
  { word = "thrust"; values = ("thrust", "thrust", "thrust") };
  { word = "tread"; values = ("tread", "trod", "trodden") };
  { word = "understand"; values = ("understand", "understood", "understood") };
  { word = "wake"; values = ("wake", "woke", "woken") };
  { word = "wear"; values = ("wear", "wore", "worn") };
  { word = "weave"; values = ("weave", "wove", "woven") };
  { word = "weep"; values = ("weep", "wept", "wept") };
  { word = "win"; values = ("win", "won", "won") };
  { word = "wind"; values = ("wind", "wound", "wound") };
  { word = "write"; values = ("write", "wrote", "written") };
]

let search_verb (verb: string) (list: entry list) : (string * string * string) option =
  let rec search (entries: entry list) : (string * string * string) option =
    match entries with
    | [] -> None
    | { word = w; values = v } :: tail ->
        if w = verb then Some v else search tail
  in
  search list

let conjugate (verb_form: Constructs.conjugated_verb) (english_verb: string): string =
    let res = Array.make 4 "" in

    let subject = 
        match verb_form.subject with
        | Subject_prefix subj | Subject_suffix subj -> subj |> PersonParam.print Subject
        | _ -> ""
    in res.(0) <- subject;

    let object_ = 
        match verb_form.object_ with
        | Object_prefix obj | Object_suffix obj -> obj |> PersonParam.print Object
        | _ -> ""
    in res.(2) <- object_;

    let indirect_object = 
        match verb_form.indirect_object_prefix with
        | Some obj -> obj |> IndirectObjectPrefix.to_person |> PersonParam.print Indirect_object
        | None -> ""
    in res.(3) <- indirect_object;

    let continuous : verb:string -> pers:PersonParam.t -> tense:tense -> modal:FirstPrefix.t option -> string = 
        fun ~verb ~pers ~tense ~modal -> 
            let ing_form =
                if String.ends_with ~suffix:"e" verb
                then String.sub verb 0 (String.length verb - 1) ^ "ing"
                else verb ^ "ing"
            in
            match (tense, modal) with
            | (Present, None) -> 
                (
                    match pers with
                    | PersonParam.First_sing -> "am " ^ ing_form
                    | PersonParam.Third_sing_human | PersonParam.Third_sing_non_human -> "is " ^ ing_form
                    | _ -> "are " ^ ing_form
                )
            | (Present, Some(Modal)) -> "should be " ^ ing_form
            | (Present, Some(Negative)) -> 
                (
                    match pers with
                    | PersonParam.First_sing -> "am not " ^ ing_form
                    | PersonParam.Third_sing_human | PersonParam.Third_sing_non_human -> "is not " ^ ing_form
                    | _ -> "are not " ^ ing_form
                )
            | (Past, None) -> 
                (
                    match pers with
                    | PersonParam.First_sing -> "was " ^ ing_form
                    | PersonParam.Third_sing_human | PersonParam.Third_sing_non_human -> "was " ^ ing_form
                    | _ -> "were " ^ ing_form
                )
            | (Past, Some(Negative)) -> 
                (
                    match pers with
                    | PersonParam.First_sing -> "was not " ^ ing_form
                    | PersonParam.Third_sing_human | PersonParam.Third_sing_non_human -> "was not " ^ ing_form
                    | _ -> "were not " ^ ing_form
                )
            | _ -> verb
    in

    let conjugated_verb =
      (* PERFECTIVE *)
      if verb_form.is_perfective
      then 
          match verb_form.first_prefix with
          | Some FirstPrefix.Negative -> "didn't " ^ english_verb
          | Some FirstPrefix.Modal -> "should " ^ english_verb
          | _ -> 
            match search_verb english_verb irregular_verbs with
            | Some (_, past, _) -> past
            | None ->
                if String.ends_with ~suffix:"e" english_verb
                then String.sub english_verb 0 (String.length english_verb - 1) ^ "ed"
                else english_verb ^ "ed"
      else
        (* IMPERFECTIVE *)
        continuous 
          ~verb:english_verb 
          ~pers:(match verb_form.subject with | Subject_prefix subj | Subject_suffix subj -> subj | _ -> PersonParam.Third_sing_human)
          ~tense:Present 
          ~modal:verb_form.first_prefix
        (* match verb_form.subject with
        | Subject_prefix subj | Subject_suffix subj -> 
            (
                match subj with
                | PersonParam.Third_sing_human | PersonParam.Third_sing_non_human -> 
                    (
                        match verb_form.first_prefix with
                        | Some FirstPrefix.Negative -> continuous ~verb:english_verb ~pers:subj ~tense:Present ~negative:true
                        | _ -> continuous ~verb:english_verb ~pers:subj ~tense:Present ~negative:false
                    )
                | _ -> 
                    (
                        match verb_form.first_prefix with
                        | Some FirstPrefix.Negative -> continuous ~verb:english_verb ~pers:subj ~tense:Present ~negative:true
                        | _ -> continuous ~verb:english_verb ~pers:subj ~tense:Present ~negative:false
                    )
            )
        | _ -> english_verb *)
    in res.(1) <- conjugated_verb;

    Array.to_list res |> String.concat " " |> String.trim

let add_complements (verb: Constructs.conjugated_verb): string =
    let comitative = match (verb.comitative, verb.initial_person_prefix) with
        | (true, Some ipp) -> "with " ^ (ipp |> InitialPersonPrefix.to_person |> PersonParam.print Object)
        | (true, None) -> "with"
        | _ -> ""
    in
    let adverbial = match (verb.adverbial, verb.initial_person_prefix) with
        | (Some Infixes.Ablative, Some ipp) -> "from " ^ (ipp |> InitialPersonPrefix.to_person |> PersonParam.print Object)
        | (Some Infixes.Ablative, None) -> "from"
        | (Some Infixes.Terminative, Some ipp) -> "to " ^ (ipp |> InitialPersonPrefix.to_person |> PersonParam.print Object)
        | (Some Infixes.Terminative, None) -> "to"
        | _ -> ""
    in
    let locative = match (verb.locative, verb.initial_person_prefix) with
        | (Some In_with_initial_person, Some ipp) -> "in " ^ (ipp |> InitialPersonPrefix.to_person |> PersonParam.print Object)
        | (Some In_without_initial_person, _) -> "here"
        | (Some On_with_initial_person, Some ipp) -> "on " ^ (ipp |> InitialPersonPrefix.to_person |> PersonParam.print Object)
        | (Some On_without_initial_person, _) -> "on"
        | _ -> ""
    in comitative ^ " " ^ adverbial ^ " " ^ locative

(* let translate (verb: Constructs.conjugated_verb) (meaning: string option): string =
    match meaning with
    | Some m -> 
        (* isolates the verb root *)
        let re = Js.Re.fromString "\\(to ([a-z]+)\\)" in
        (
            match (Js.Re.exec ~str:m re) with
            | Some res -> (
                (* Captures the verb root *)
                let caps = Js.Re.captures res in
                if Array.length caps >= 2
                then 
                    (
                        match caps.(1) |> Js.Nullable.toOption with
                        | Some cap -> 
                            let conjugated_verb = conjugate verb cap in
                            let complements = add_complements verb in
                            (conjugated_verb ^ " " ^ complements) |> String.trim
                        | None -> verb.stem
                    )
                else verb.stem
                )
            | None -> verb.stem
        )
    | None -> verb.stem *)

let translate (verb: Constructs.conjugated_verb) (meaning: string option): string =
  match meaning with
  | Some m ->
      let conjugated_verb = conjugate verb m in
      let complements = add_complements verb in
      (conjugated_verb ^ " " ^ complements) |> String.trim
  | None -> verb.stem
