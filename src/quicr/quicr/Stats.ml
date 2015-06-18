module S = LogicSymbolicSet

module Make(D: Interface.Domain
                  with type sym = int
                   and type cnstr = int S.t
                   and type output = int S.t
                   and type query = int S.q
                 )
  : Interface.Domain
    with type sym = int
     and type cnstr = int S.t
     and type output = int S.t
     and type query = int S.q
= struct

  type stats =
    {
      time: float;
      count: int;
    }

  let stats_init () =
    ref {
      time = 0.0;
      count = 0;
    }

  let stat r e =
    let start_time = Unix.gettimeofday () in
    let res = Lazy.force e in
    let end_time = Unix.gettimeofday () in
    r := {
      time = !r.time +. (end_time -. start_time);
      count = !r.count + 1;
    };
    res

  let pp_stat ff s =
    Format.fprintf ff "(%d) %f" !s.count !s.time


  type ctx = {
      ctx: D.ctx;
      top: stats ref;
      bottom: stats ref;
      symbols: stats ref;
      constrain: stats ref;
      serialize: stats ref;
      sat: stats ref;
      join: stats ref;
      widening: stats ref;
      meet: stats ref;
      le: stats ref;
      is_bottom: stats ref;
      is_top: stats ref;
      forget: stats ref;
      rename_symbols: stats ref;
      query: stats ref;
      combine: stats ref;
  }

  let pp_stats ff ctx =
    Format.fprintf ff "@,top: %a" pp_stat ctx.top;
    Format.fprintf ff "@,bottom: %a" pp_stat ctx.bottom;
    Format.fprintf ff "@,symbols: %a" pp_stat ctx.symbols;
    Format.fprintf ff "@,constrain: %a" pp_stat ctx.constrain;
    Format.fprintf ff "@,serialize: %a" pp_stat ctx.serialize;
    Format.fprintf ff "@,sat: %a" pp_stat ctx.sat;
    Format.fprintf ff "@,join: %a" pp_stat ctx.join;
    Format.fprintf ff "@,widening: %a" pp_stat ctx.widening;
    Format.fprintf ff "@,meet: %a" pp_stat ctx.meet;
    Format.fprintf ff "@,le: %a" pp_stat ctx.le;
    Format.fprintf ff "@,is_bottom: %a" pp_stat ctx.is_bottom;
    Format.fprintf ff "@,is_top: %a" pp_stat ctx.is_top;
    Format.fprintf ff "@,forget: %a" pp_stat ctx.forget;
    Format.fprintf ff "@,rename_symbols: %a" pp_stat ctx.rename_symbols;
    Format.fprintf ff "@,query: %a" pp_stat ctx.query;
    Format.fprintf ff "@,combine: %a" pp_stat ctx.combine


  type t = D.t

  type sym = int

  type cnstr = int S.t

  type output = int S.t

  type query = int S.q

  let init () = {
    ctx = D.init ();
    top = stats_init ();
    bottom = stats_init ();
    symbols = stats_init ();
    constrain = stats_init ();
    serialize = stats_init ();
    sat = stats_init ();
    join = stats_init ();
    widening = stats_init ();
    meet = stats_init ();
    le = stats_init ();
    is_bottom = stats_init ();
    is_top = stats_init ();
    forget = stats_init ();
    rename_symbols = stats_init ();
    query = stats_init ();
    combine = stats_init ();
  }


  let top ctx = 
    stat ctx.top (lazy (D.top ctx.ctx))

  let bottom ctx = 
    stat ctx.bottom (lazy (D.bottom ctx.ctx))

  let symbols ctx d =
    stat ctx.symbols (lazy (D.symbols ctx.ctx d))

  let constrain ctx cnstr d =
    stat ctx.constrain (lazy (D.constrain ctx.ctx cnstr d))

  let serialize ctx d =
    stat ctx.serialize (lazy (D.serialize ctx.ctx d))

  let sat ctx d cnstr =
    stat ctx.sat (lazy (D.sat ctx.ctx d cnstr))

  let join ctx a b =
    stat ctx.join (lazy (D.join ctx.ctx a b))

  let widening ctx a b =
    stat ctx.widening (lazy (D.widening ctx.ctx a b))

  let meet ctx a b =
    stat ctx.meet (lazy (D.meet ctx.ctx a b))

  let le ctx a b =
     stat ctx.le (lazy (D.le ctx.ctx a b))

  let is_bottom ctx d =
    stat ctx.is_bottom (lazy (D.is_bottom ctx.ctx d))

  let is_top ctx d =
    stat ctx.is_top (lazy (D.is_top ctx.ctx d))

  let forget ctx syms d =
    stat ctx.forget (lazy (D.forget ctx.ctx syms d))

  let rename_symbols ctx map d =
    stat ctx.rename_symbols (lazy (D.rename_symbols ctx.ctx map d))

  let query ctx d =
    stat ctx.query (lazy (D.query ctx.ctx d))

  let combine ctx q d =
    stat ctx.combine (lazy (D.combine ctx.ctx q d))

  let pp_print ctx pp_sym ff t =
    Format.fprintf ff "@[<v 0>%a" (D.pp_print ctx.ctx pp_sym) t;
    pp_stats ff ctx;
    Format.fprintf ff "@]"

  let pp_debug ctx pp_sym ff t =
    pp_print ctx pp_sym ff t
end
