module Make(S: Interface.Sym) : Interface.Constant with type sym = S.t = struct
  type sym = S.t
  type cnstr = sym Commands.string_cnstr
  type t = string

  let rec constrain res cnstr : (sym * t) list option =
    match cnstr with
    | `Eq(`Var s, `Const c)
    | `Eq(`Const c, `Var s) ->
      Some ((s,c)::res)
    | `Eq(`Const s1, `Const s2) when s1 <> s2 ->
      None
    | `False ->
      None
    | `True ->
      Some res
    | `And(c1, c2) ->
      begin match constrain res c1 with
      | Some res ->
        constrain res c2
      | None ->
        None
      end
    | `Ne _
    | `Eq _
    | `Or _ -> Some res

  let constrain cnstr =
    constrain [] cnstr


  let rec sat res cnstr : (sym * t) list option =
    match cnstr with
    | `Eq(`Var s, `Const c)
    | `Eq(`Const c, `Var s) ->
      Some ((s,c)::res)
    | `Eq(`Const s1, `Const s2) when s1 <> s2 ->
      None
    | `False ->
      None
    | `True ->
      Some res
    | `And(c1, c2) ->
      begin match sat res c1 with
      | Some res ->
        sat res c2
      | None ->
        None
      end
    | `Ne _
    | `Eq _
    | `Or _ -> None

  let sat cnstr = sat [] cnstr

  let to_constraint assigns : cnstr =
    match List.fold_left (fun res (v,c) ->
        let v = `Eq(`Var v, `Const c) in
        match res with
        | None -> Some v
        | Some res -> Some (`And(res, v))
      ) None assigns with
    | Some v ->
      v
    | None ->
      `True

  let compare = String.compare
end
