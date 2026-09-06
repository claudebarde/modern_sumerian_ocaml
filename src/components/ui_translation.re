type display_language = English | SuxCuneiform | SuxLatin;
type cuneiform_size = Small | Medium | Large;

let display_to(
    ~sentence: string, 
    ~language: display_language, 
    ~size: option(cuneiform_size)
): React.element = {
    let result = 
        switch (sentence) {
            | "settings" => switch (language) {
                | English => "Settings"
                | SuxCuneiform => {js|𒃻𒁺|js}
                | SuxLatin => {js|Niĝgub|js}
            }
            | "language_choice" => switch (language) {
                | English => "Choose your language"
                | SuxCuneiform => {js|𒅴𒍪·𒅆·𒊕𒂷𒀊|js}
                | SuxLatin => {js|Emezu igi saĝab|js}
            }
            | "sign_up" => switch (language) {
                | English => "Sign Up"
                | SuxCuneiform => {js|𒃻𒋃·𒀝𒀊|js}
                | SuxLatin => {js|Niĝkas akab|js}
            }
            | "sign_in" => switch (language) {
                | English => "Sign In"
                | SuxCuneiform => {js|𒃻𒋃𒀀·𒆭|js}
                | SuxLatin => {js|Niĝkasa kur|js}
            }
            | "password" => switch (language) {
                | English => "Password"
                | SuxCuneiform => {js|𒅗𒀜𒄬|js}
                | SuxLatin => "Inim adhal"
            }
            | "email_address" => switch (language) {
                | English => "Email Address"
                | SuxCuneiform => {js|𒈬𒊬𒊏𒆥𒄄𒀀|js}
                | SuxLatin => {js|Musara kiĝgia|js}
            }
            | "sign_up_message" => switch (language) {
                | English => "Enter your email address and choose a password to create an account"
                | SuxCuneiform => {js|𒃻𒋃·𒀝𒉈·𒈬𒊬𒊏·𒆥𒄄𒀀𒍪·𒊬𒊏𒀊·𒅗𒀜𒄬·𒁺𒁀𒀊|js}
                | SuxLatin => {js|Niĝkas akede, musara kiĝgiazu sarab, inim adhal gubab|js}
            }
            // TODO: sign_in_message "Enter your email address and password to sign in"
            | "cancel" => switch (language) {
                | English => "Cancel"
                | SuxCuneiform => {js|𒍣𒊏𒀊|js}
                | SuxLatin => "Zirab"
            }
            | "save" => switch (language) {
                | English => "Save"
                | SuxCuneiform => {js|𒃻𒊏𒉌𒅁|js}
                | SuxLatin => {js|Ĝaranib|js}
            }
            | "close" => switch (language) {
                | English => "Close"
                | SuxCuneiform => {js|𒆟𒊏𒀊|js}
                | SuxLatin => {js|Kesherab|js}
            }
            | "tools" => switch (language) {
                | English => "Tools"
                | SuxCuneiform => {js|𒀉𒃸|js}
                | SuxLatin => "Akar"
            }
            | "games" => switch (language) {
                | English => "Games"
                | SuxCuneiform => {js|𒂊𒉈𒁲|js}
                | SuxLatin => "Enedi"
            }
            | "learn" => switch (language) {
                | English => "Learn"
                | SuxCuneiform => {js|𒂍𒁾𒁀𒀀|js}
                | SuxLatin => "Edubba'a"
            }
            | _ => {js|𒃻𒉡𒍪|js} // Unknown
        }

    switch (language) {
    | English | SuxLatin => React.string(result)
    | SuxCuneiform => {
        switch size {
            | Some(Small) => <span className="cuneiforms x-small">{React.string(result)}</span>
            | Some(Medium) => <span className="cuneiforms small">{React.string(result)}</span>
            | Some(Large) => <span className="cuneiforms">{React.string(result)}</span>
            | None => <span className="cuneiforms">{React.string(result)}</span>
        }
    }
    }
}