open AST

module Color = struct
  type t =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Reset

  let esc = "\x1b"

  let color br c =
    esc
    ^ "["
    ^ (match c with
        | Black -> "30"
        | Red -> "31"
        | Green -> "32"
        | Yellow -> "33"
        | Blue -> "34"
        | Magenta -> "35"
        | Cyan -> "36"
        | White -> "37"
        | Reset -> "0"
      )
    ^ (if br then ";1" else "")
    ^ "m"

  let of_string c =
    let (bright, c) = match String.lowercase c with
      | "black" ->    (false, Black)
      | "black!" ->   (true,  Black)
      | "red" ->      (false, Red)
      | "red!" ->     (true,  Red)
      | "green" ->    (false, Green)
      | "green!" ->   (true,  Green)
      | "yellow" ->   (false, Yellow)
      | "yellow!" ->  (true,  Yellow)
      | "blue" ->     (false, Blue)
      | "blue!" ->    (true,  Blue)
      | "magenta" ->  (false, Magenta)
      | "magenta!" -> (true,  Magenta)
      | "cyan" ->     (false, Cyan)
      | "cyan!" ->    (true,  Cyan)
      | "white" ->    (false, White)
      | "white!" ->   (true,  White)
      | _ ->          (true,  Cyan)
    in
    color bright c

end

let print_final = ref false

let print_step = ref false

let color_inv = ref (Color.color true Color.Cyan)

let state_brace = ref false

let args = [
  "-final", Arg.Set print_final, " Print the final abstract state";
  "-step",  Arg.Set print_step,  " Print each step of the interpretation and final state";
  "-color", Arg.String (fun s -> color_inv := Color.of_string s), "<color> Set color of abstract states";
  "-brace", Arg.Set state_brace, " Show braces for abstract states";
]

module Make(D: Interface.Domain
            with type sym = int
             and type cnstr = int LogicSymbolicSet.t
             and type output = int LogicSymbolicSet.t
             and type query = int LogicSymbolicSet.q)
= struct

  let print_state renv state =
    (*let c = D.serialize state in*)
    (*let c = L.map_symbol (Hashtbl.find renv) c in*)
    let pp_sym ff s =
      try
        let n = Hashtbl.find renv s in
        Format.pp_print_string ff n
      with Not_found ->
        Format.pp_print_string ff "%";
        Format.pp_print_int ff s
    in

    if !state_brace then begin
      Format.print_cut ();
      Format.open_vbox 0;
      Format.open_vbox 2;
      Format.print_string !color_inv;
      Format.print_string "[";
      Format.print_cut ();
      D.pp_print pp_sym Format.std_formatter state;
      (*L.pp Format.pp_print_string Format.std_formatter c;*)
      Format.close_box ();
      Format.print_cut ();
      Format.print_string "]";
      Format.print_string (Color.color false Color.Reset);
      Format.close_box ()
    end else begin
      Format.print_cut ();
      Format.open_vbox 2;
      Format.print_string "  ";
      Format.print_string !color_inv;
      D.pp_print pp_sym Format.std_formatter state;
      (*L.pp Format.pp_print_string Format.std_formatter c;*)
      Format.print_string (Color.color false Color.Reset);
      Format.close_box ()
    end


    (*Format.printf "@,@[<v 0>[@[<v 2>@,%s%a%s@]@,]@]" !color_inv (L.pp Format.pp_print_string) c (Color.color false Color.Reset)*)

  let interpret p =
    let count = ref 0 in
    let env = Hashtbl.create 1023 in
    let renv = Hashtbl.create 1023 in
    let fresh sym =
      let id = !count in
      incr count;
      begin try
        let oid = Hashtbl.find env sym in
        Hashtbl.remove renv oid
      with Not_found ->
        ()
      end;
      Hashtbl.replace env sym id;
      Hashtbl.replace renv id sym;
      id
    in
    let get_or_fresh (sym: var) : int =
      try
        Hashtbl.find env sym
      with Not_found ->
        fresh sym
    in
    let temporary () =
      !count
    in
    let ctx = D.init () in
    let rec interpret state = function
      | Skip -> state
      | Kill v ->
        if !print_step then Format.printf "@,kill %s" v;
        let v = get_or_fresh v in
        let state = D.forget [v] state in
        if !print_step then print_state renv state;
        state
      | Rename (v1, v2) ->
        if !print_step then Format.printf "@,rename %s %s" v1 v2;
        let v1 = get_or_fresh v1 in
        let v2 = get_or_fresh v2 in
        let state = D.rename_symbols (Rename.singleton v1 v2) state in
        if !print_step then print_state renv state;
        state
      | Seq (t1, t2) -> interpret (interpret state t1) t2
      | Branch (t1, t2) ->
        if !print_step then Format.printf "@,@[<v 2>branch {";
        let s1 = interpret state t1 in

        if !print_step then Format.printf "@]@,@[<v 2>} else {";
        let s2 = interpret state t2 in
        if !print_step then Format.printf "@]@,}";
        let state = D.join s1 s2 in
        if !print_step then print_state renv state;
        state
      | Both (t1, t2) ->
        if !print_step then Format.printf "@,@[<v 2>both {";
        let s1 = interpret state t1 in

        if !print_step then Format.printf "@]@,@[<v 2>} and {";
        let s2 = interpret state t2 in
        if !print_step then Format.printf "@]@,}";
        let state = D.meet s1 s2 in
        if !print_step then print_state renv state;
        state
      | Loop t ->
        if !print_step then Format.printf "@,@[<v 2>loop {";
        if !print_step then print_state renv state;
        let rec loop i invstate =
          if !print_step then Format.printf "@,@[<v 2>%d: {" (i+1);
          let body_end = interpret invstate t in
          if !print_step then Format.printf "@]@,}";
          if D.le body_end invstate then
            (i,invstate)
          else begin
            let invstate = D.widening invstate body_end in
            if !print_step then print_state renv invstate;
            loop (i+1) invstate
          end
        in
        let (loopcount,state) = loop 0 state in
        if !print_step then Format.printf "@]@,}";
        if !print_step then print_state renv state;
        state
      | Assign (lhs, rhs) ->
        if !print_step then Format.printf "@,%s = %a" lhs (L.pp_e Format.pp_print_string) rhs;
        let rhs = L.map_symbol_e get_or_fresh rhs in
        let (do_rename,lid) = if Hashtbl.mem env lhs then (true,temporary ())
          else (false,fresh lhs) in
        let state = D.constrain (L.Eq(L.Var lid, rhs)) state in
        let state = if do_rename then
            let oldid = Hashtbl.find env lhs in
            state |>
            D.forget [oldid] |>
            D.rename_symbols (Rename.singleton lid oldid) (*[lid,oldid]*) (*(fun id -> if id = lid then oldid else id)*)
          else state in
        if !print_step then print_state renv state;
        state
      | Choose (lhs, rhs) ->
        if !print_step then Format.printf "@,%s = choose %s" lhs rhs;
        let rhs = get_or_fresh rhs in
        let (do_rename,lid) = if Hashtbl.mem env lhs then (true,temporary ())
          else (false,fresh lhs) in
        let state = D.constrain (L.SubEq(L.Sing lid, L.Var rhs)) state in
        let state = if do_rename then
            D.rename_symbols (Rename.singleton lid (Hashtbl.find env lhs)) state (*[lid, Hashtbl.find env lhs] state *) (*(fun id -> if id = lid then Hashtbl.find env lhs else id) state*)
            (*D.rename_symbols [(lid, Hashtbl.find env lhs)] state*)
          else state in
        if !print_step then print_state renv state;
        state
      | Assume c ->
        if !print_step then Format.printf "@,assume(%a)" (L.pp Format.pp_print_string) c;
        let c = L.map_symbol get_or_fresh c in
        let state = D.constrain c state in
        if !print_step then print_state renv state;
        state
      | Assert c ->
        if !print_step then Format.printf "@,assert(%a)" (L.pp Format.pp_print_string) c;
        let c = L.map_symbol get_or_fresh c in
        let state = if D.sat state c then
            state
          else begin
            Format.printf "Warning: Could not prove assertion@.";
            D.constrain c state
          end in
        if !print_step then print_state renv state;
        state
    in
    if !print_step || !print_final then Format.printf "@[<v 0>";
    let final = interpret (D.top ctx) p in
    if !print_final then
      print_state renv final;
    if !print_step || !print_final then Format.printf "@]@."
      


end


