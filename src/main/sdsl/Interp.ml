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
    if c = "none" then
      None
    else
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
      Some (color bright c)

  let print c pp ff a =
    match c with
    | None ->
      pp ff a
    | Some c ->
      Format.pp_print_string ff c;
      pp ff a;
      Format.pp_print_string ff (color false Reset)

end

let print_final = ref false

let print_step = ref false

let color_inv = ref (Some (Color.color true Color.Cyan))

let state_brace = ref false

let opt_time = ref false

let opt_eval = ref false

let args = [
  "-final", Arg.Set print_final, " Print the final abstract state";
  "-step",  Arg.Set print_step,  " Print each step of the interpretation and final state";
  "-color", Arg.String (fun s -> color_inv := Color.of_string s), "<color> Set color of abstract states";
  "-brace", Arg.Set state_brace, " Show braces for abstract states";
  "-time", Arg.Set opt_time, " Report analysis time";
  "-eval", Arg.Set opt_eval, " Report pass/fail for each assertion";
]

module Make(D: QUICr.I
            with type sym = int
             and type cnstr = int QUICr.Logic.SymbolicSet.t
             and type output = int QUICr.Logic.SymbolicSet.t
             and type query = int QUICr.Logic.SymbolicSet.q)
= struct

let print_state_raw ctx pp_sym state =
  if !state_brace then begin
    Format.print_cut ();
    Format.open_vbox 0;
    Format.open_vbox 2;
    Color.print !color_inv (fun ff () ->
        Format.print_string "[";
        Format.print_cut ();
        D.pp_print ctx pp_sym Format.std_formatter state;
        (*L.pp Format.pp_print_string Format.std_formatter c;*)
        Format.close_box ();
        Format.print_cut ();
        Format.print_string "]";
      ) Format.std_formatter ();
    Format.close_box ()
  end else begin
    Format.print_cut ();
    Format.open_vbox 2;
    Format.print_string "  ";
    Color.print !color_inv (fun ff () ->
        D.pp_print ctx pp_sym Format.std_formatter state;
        (*L.pp Format.pp_print_string Format.std_formatter c;*)
      ) Format.std_formatter ();
    Format.close_box ()
  end


  let print_state ctx renv state =
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

    print_state_raw ctx pp_sym state

    (*Format.printf "@,@[<v 0>[@[<v 2>@,%s%a%s@]@,]@]" !color_inv (L.pp Format.pp_print_string) c (Color.color false Color.Reset)*)

  let interpret p =
    let count = ref 0 in
    let env = Hashtbl.create 1023 in
    let renv = Hashtbl.create 1023 in
    let unnamed () =
      let id = !count in
      incr count;
      id
    in
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
    let start_time = Unix.gettimeofday () in
    let ctx = D.init () in
    let rec interpret state = function
      | Skip -> state
      | Kill l ->
        if !print_step then begin
          Format.printf "@,kill";
          List.iter (fun v -> Format.printf " %s" v) l
        end;
        let l = List.map get_or_fresh l in
        let state = D.forget ctx l state in
        if !print_step then print_state ctx renv state;
        state
      | Rename l ->
        if !print_step then begin
          Format.printf "@,rename";
          List.iter (fun (a,b) -> Format.printf " %s %s" a b) l
        end;
        let l = List.map (fun (a,b) -> (get_or_fresh a, get_or_fresh b)) l in
        let state = D.rename_symbols ctx (QUICr.Rename.of_assoc_list l) state in
        if !print_step then print_state ctx renv state;
        state
      | Seq (t1, t2) -> interpret (interpret state t1) t2
      | Branch (t1, t2) ->
        if !print_step then Format.printf "@,@[<v 2>branch {";
        let s1 = interpret state t1 in

        if !print_step then Format.printf "@]@,@[<v 2>} else {";
        let s2 = interpret state t2 in
        if !print_step then Format.printf "@]@,}";
        let state = D.join ctx s1 s2 in
        if !print_step then print_state ctx renv state;
        state
      | Both (t1, t2) ->
        if !print_step then Format.printf "@,@[<v 2>both {";
        let s1 = interpret state t1 in

        if !print_step then Format.printf "@]@,@[<v 2>} and {";
        let s2 = interpret state t2 in
        if !print_step then Format.printf "@]@,}";
        let state = D.meet ctx s1 s2 in
        if !print_step then print_state ctx renv state;
        state
      | Loop t ->
        if !print_step then Format.printf "@,@[<v 2>loop {";
        if !print_step then print_state ctx renv state;
        let rec loop i invstate =
          if !print_step then Format.printf "@,@[<v 2>%d: {" (i+1);
          let body_end = interpret invstate t in
          if !print_step then Format.printf "@]@,}";
          if D.le ctx body_end invstate then
            (i,invstate)
          else begin
            let invstate = D.widening ctx invstate body_end in
            if !print_step then print_state ctx renv invstate;
            loop (i+1) invstate
          end
        in
        let (loopcount,state) = loop 0 state in
        if !print_step then Format.printf "@]@,}";
        if !print_step then print_state ctx renv state;
        state
      | For(v,e,t) ->
        if !print_step then Format.printf "@,@[<v 2>for(%s in %a) {" v (L.pp_e Format.pp_print_string) e;
        if !print_step then print_state ctx renv state;

        let e = L.map_symbol_e get_or_fresh e in
        let vid = get_or_fresh v in
        let vid' = unnamed () in

        let visit_id = unnamed () in
        let visit_id' = unnamed () in

        let rec loop i invstate =
          if !print_step then Format.printf "@,@[<v 2>%d: {" (i+1);
          let body_end =
            invstate |>
            D.constrain ctx (L.In(vid', L.Diff(e, L.Var visit_id))) |>
            D.forget ctx [vid] |>
            D.rename_symbols ctx (QUICr.Rename.singleton vid' vid) |>
            (fun st -> interpret st t) |>
            D.constrain ctx (L.Eq(L.Var visit_id', L.DisjUnion(L.Var visit_id, L.Sing vid))) |>
            D.forget ctx [visit_id] |>
            D.rename_symbols ctx (QUICr.Rename.singleton visit_id' visit_id)
          in

          if !print_step then Format.printf "@]@,}";
          if D.le ctx body_end invstate then
            (i,invstate)
          else begin
            let invstate = D.widening ctx invstate body_end in
            if !print_step then print_state ctx renv invstate;
            loop (i+1) invstate
          end
        in

        (* set up for the loop *)
        let state = D.constrain ctx (L.Eq(L.Var visit_id, L.Empty)) state in

        (* run loop *)
        let (loopcount,state) = loop 0 state in

        (* clean up from loop *)
        let state = D.constrain ctx (L.Eq(L.Var visit_id, e)) state in
        let state = D.forget ctx [visit_id] state in


        if !print_step then Format.printf "@]@,}";
        if !print_step then print_state ctx renv state;
        state

      | Assign (lhs, rhs) ->
        if !print_step then Format.printf "@,%s = %a" lhs (L.pp_e Format.pp_print_string) rhs;
        let rhs = L.map_symbol_e get_or_fresh rhs in
        let (do_rename,lid) = if Hashtbl.mem env lhs then (true,temporary ())
          else (false,fresh lhs) in
        let state = D.constrain ctx (L.Eq(L.Var lid, rhs)) state in
        let state = if do_rename then
            let oldid = Hashtbl.find env lhs in
            state |>
            D.forget ctx [oldid] |>
            D.rename_symbols ctx (QUICr.Rename.singleton lid oldid) (*[lid,oldid]*) (*(fun id -> if id = lid then oldid else id)*)
          else state in
        if !print_step then print_state ctx renv state;
        state
      | Choose (lhs, rhs) ->
        if !print_step then Format.printf "@,%s = choose %a" lhs (L.pp_e Format.pp_print_string) rhs;
        let rhs = L.map_symbol_e get_or_fresh rhs in
        let (do_rename,lid) = if Hashtbl.mem env lhs then (true,temporary ())
          else (false,fresh lhs) in
        let state = D.constrain ctx (L.In(lid, rhs)) state in
        let state = if do_rename then
            let oldid = Hashtbl.find env lhs in
            state |>
            D.forget ctx [oldid] |>
            D.rename_symbols ctx (QUICr.Rename.singleton lid oldid) (*[lid,oldid]*) (*(fun id -> if id = lid then oldid else id)*)
          else state in
        if !print_step then print_state ctx renv state;
        state
      | Assume c ->
        if !print_step then Format.printf "@,assume(%a)" (L.pp Format.pp_print_string) c;
        let c = L.map_symbol get_or_fresh c in
        let state = D.constrain ctx c state in
        if !print_step then print_state ctx renv state;
        state
      | Assert (id, c) ->
        if !print_step then Format.printf "@,assert(%a)" (L.pp Format.pp_print_string) c;
        let c = L.map_symbol get_or_fresh c in
        let state = if D.sat ctx state c then begin
            if !opt_eval then Format.printf "(%d) : PASS@." id;
            state
          end else begin
            if !opt_eval then Format.printf "(%d) : FAIL@." id
            else Format.printf "Warning: Could not prove assertion@.";
            D.constrain ctx c state
          end in
        if !print_step then print_state ctx renv state;
        state
    in
    if !print_step || !print_final then Format.printf "@[<v 0>";
    let final = interpret (D.top ctx) p in
    let end_time = Unix.gettimeofday () in
    if !print_final then
      print_state ctx renv final;
    if !print_step || !print_final then Format.printf "@]@.";
    if !opt_time then
      Format.printf "Analysis time: %f seconds@." (end_time -. start_time)

      


end


