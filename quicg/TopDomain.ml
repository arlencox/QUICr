type ctx = unit

type t = unit

let args = []

let init () = ()

let meet () () () = ()

let join () () () = ()

let widen () () () = ()

let narrow () () () = ()

let is_bottom () () = false

let is_top () () = true

let le () () () = true

let top () _ = ()

let bottom () _ = ()

let add_dim () () = ()
let rem_dim () () = ()
let get_dim () () = 0
let get_eq () () = []

let fmt ?escape:(escape=FormatHelp.Text) (():ctx) _ (ff : Format.formatter) (():unit) =
  Format.fprintf ff "true"

let transition () () _ = ()
let constrain () () _ = ()

let constrain_qc () () _ _ = ()

let rename_set () () _ _ = ()

let forget_set () () _ = ()
