type t = Js.Dict.t(list(string));

let fallbackDict = Js.Dict.fromArray([|
    ({js|ʔak|js}, ["0x1201D"]), 
    ("eme", ["0x12174"]), 
    ({js|ĝen|js}, ["0x1207A"]), 
    ({js|ĝir15|js}, ["0x120A0"]), 
    ("im", ["0x1214E"]), 
    ("la", ["0x121B7"]), 
    ("na-na", ["0x12158", "0x12158"]),
    ({js|naĝ|js}, ["0x12158"]),
    ({js|niĝ|js}, ["0x120FB"]), 
    ("ul", ["0x12109"]), 
    ("tuku", ["0x12307"]), 
    ({js|šum|js}, ["0x122E7"]), 
|]); 