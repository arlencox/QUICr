module Make(S: QGInterface.SetDomain) : QGInterface.SetNumDomain = struct
  type ctx = S.ctx
    
  type t =
  | Dom of S.t
  | Bottom

  let top ctx scalar set =
    Dom (S.top ctx scalar set)

  let bottom _ctx _scalar _set =
    Bottom

  let args = S.args

  let init = S.init

  let meet ctx a b =
    match (a,b) with
    | (Bottom, _)
    | (_, Bottom) ->
      Bottom
    | (Dom a, Dom b) ->
      Dom (S.meet ctx a b)

  let joina f ctx a b =
    match (a,b) with
    | (Dom a, Dom b) ->
      Dom (f ctx a b)
    | (Dom d, Bottom)
    | (Bottom, Dom d) ->
      Dom d
    | (Bottom, Bottom) ->
      Bottom

  let join ctx a b =
    joina S.join ctx a b

  let widen ctx a b =
    joina S.widen ctx a b

  let narrow ctx a b =
    match (a,b) with
    | (Bottom, _)
    | (_, Bottom) -> Bottom
    | (Dom a, Dom b) ->
      Dom (S.narrow ctx a b)

  let is_bottom ctx t =
    match t with
    | Bottom -> true
    | Dom d -> S.is_bottom ctx d

  let is_top ctx t =
    match t with
    | Bottom -> false
    | Dom d -> S.is_top ctx d

  let le ctx a b =
    match (a,b) with
    | (Dom a, Dom b) ->
      S.le ctx a b
    | (Bottom, _ ) ->
      true
    | (Dom a, Bottom) ->
      S.is_bottom ctx a

  let fmt ?escape:(escape=FormatHelp.Text) ctx fmt_scalar_var fmt_set_var ff st =
    match st with
    | Bottom ->
      Format.fprintf ff "%a" (FormatHelp.fmt_bottom escape) ()
    | Dom d ->
      S.fmt ~escape:escape ctx fmt_scalar_var fmt_set_var ff d

  let transition ctx t cmd =
    match t with
    | Bottom -> Bottom
    | Dom d -> Dom (S.transition ctx d cmd)

  let constrain ctx t guard =
    match t with
    | Bottom -> Bottom
    | Dom d -> Dom (S.constrain ctx d guard)

  let constrain_qc ctx t a b =
    match t with
    | Bottom -> Bottom
    | Dom d -> Dom (S.constrain_qc ctx d a b)
    
  let rename_set ctx t a b =
    match t with
    | Bottom -> Bottom
    | Dom d -> Dom (S.rename_set ctx d a b)

  let forget_set ctx t a =
    match t with
    | Bottom -> Bottom
    | Dom d -> Dom (S.forget_set ctx d a)

end
