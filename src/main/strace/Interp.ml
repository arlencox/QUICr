module I = SDSL.Interp
module A = AST

module Make(D: QUICr.I
            with type sym = int
             and type cnstr = int QUICr.Logic.SymbolicSet.t
             and type output = int QUICr.Logic.SymbolicSet.t
             and type query = int QUICr.Logic.SymbolicSet.q)
= struct

  module ID = I.Make(D)

  module SMap = Map.Make(String)

  type stats = {
    sat_t: int;
    sat_c: int;
    le_t: int;
    le_c: int;
    is_bottom_t: int;
    is_bottom_c: int;
    is_top_t: int;
    is_top_c: int;
  }

  let interp_c ctx map c =
    let get s =
      try
        SMap.find s map
      with Not_found ->
        Format.printf "Error: Could not find variable %s" s;
        exit 1
    in

    let d = match c with
    | A.Top -> D.top ctx
    | A.Bottom -> D.bottom ctx
    | A.Constrain (c, s) ->
      D.constrain ctx c (get s)
    | A.Join (s1, s2) ->
      D.join ctx (get s1) (get s2)
    | A.Widening (s1, s2) ->
      D.widening ctx (get s1) (get s2)
    | A.Meet (s1, s2) ->
      D.meet ctx (get s1) (get s2)
    | A.Forget (l, s) ->
      D.forget ctx l (get s)
    | A.Rename (r, s) ->
      D.rename_symbols ctx (QUICr.Rename.of_assoc_list r) (get s)
    in
    if !I.print_step then ID.print_state_raw ctx Format.pp_print_int d;
    d

  let interp_p ctx map stats p =
    let get s =
      try
        SMap.find s map
      with Not_found ->
        Format.printf "Error: Could not find variable %s" s;
        exit 1
    in

    let (r,stats) = match p with
      | A.Sat(s,c) ->
        let r = D.sat ctx (get s) c in
        let stats = {stats with
                     sat_t = stats.sat_t + (if r then 1 else 0);
                     sat_c = stats.sat_c + 1} in
        (r,stats)
      | A.Le(s1,s2) ->
        let r = D.le ctx (get s1) (get s2) in
        let stats = {stats with
                     le_t = stats.le_t + (if r then 1 else 0);
                     le_c = stats.le_c + 1} in
        (r,stats)
      | A.IsBottom s ->
        let r = D.is_bottom ctx (get s) in
        let stats = {stats with
                     is_bottom_t = stats.is_bottom_t + (if r then 1 else 0);
                     is_bottom_c = stats.is_bottom_c + 1} in
        (r,stats)
    | A.IsTop s ->
        let r = D.is_top ctx (get s) in
        let stats = {stats with
                     is_top_t = stats.is_top_t + (if r then 1 else 0);
                     is_top_c = stats.is_top_c + 1} in
        (r,stats)
    in
    if !I.print_step then Format.printf "@,  %b" r;
    stats

  let interp_t ctx map stats t =
    if !I.print_step then Format.printf "@,%a" A.pp_t t;
    match t with
    | A.Trans (res, c) ->
      let d = interp_c ctx map c in
      (SMap.add res d map, stats)
    | A.Query p ->
      (map, interp_p ctx map stats p)

  let interpret t =
    if !I.print_step then Format.printf "@[<v 0>";
    let ctx = D.init () in
    let map = SMap.empty in
    let stats = {
      sat_t = 0;
      sat_c = 0;
      le_t = 0;
      le_c = 0;
      is_bottom_t = 0;
      is_bottom_c = 0;
      is_top_t = 0;
      is_top_c = 0;
    } in

    let start_time = Unix.gettimeofday () in
    let (map, stats) = List.fold_left (fun (map, stats) t ->
        interp_t ctx map stats t
      ) (map, stats) t in
    let end_time = Unix.gettimeofday () in
    if !I.print_step then Format.printf "@]";
    Format.printf "@.sat: %d/%d@." stats.sat_t stats.sat_c;
    Format.printf "le : %d/%d@." stats.le_t stats.le_c;
    Format.printf "bot: %d/%d@." stats.is_bottom_t stats.is_bottom_c;
    Format.printf "top: %d/%d@." stats.is_top_t stats.is_top_c;
    if !I.opt_time then
      Format.printf "Analysis time: %f seconds@." (end_time -. start_time)
end
