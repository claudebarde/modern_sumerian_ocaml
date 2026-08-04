type origin;

[@mel.obj]
external make_origin: (
  ~x: float=?,
  ~y: float=?,
  unit,
) => origin = "";

type options;

[@mel.obj]
external make_options: (
  ~particleCount: int=?,
  ~spread: int=?,
  ~startVelocity: int=?,
  ~origin: origin=?,
  ~colors: array(string)=?,
  ~disableForReducedMotion: bool=?,
  unit,
) => options = "";

[@mel.module "canvas-confetti"]
external launch: options => unit = "default";
