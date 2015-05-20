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
    Format.fprintf ff "@,forget: %a" pp_stat ctx.forget;
    Format.fprintf ff "@,rename_symbols: %a" pp_stat ctx.rename_symbols;
    Format.fprintf ff "@,query: %a" pp_stat ctx.query;
    Format.fprintf ff "@,combine: %a" pp_stat ctx.combine


  type t = {
    d: D.t;
    c: ctx;
  }

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
    forget = stats_init ();
    rename_symbols = stats_init ();
    query = stats_init ();
    combine = stats_init ();
  }


  let top ctx = 
    {
      d = stat ctx.top (lazy (D.top ctx.ctx));
      c = ctx;
    }

  let bottom ctx = 
    {
      d = stat ctx.bottom (lazy (D.bottom ctx.ctx));
      c = ctx;
    }

  let context d = d.c

  let symbols {d;c} =
    stat c.symbols (lazy (D.symbols d))

  let constrain cnstr {d;c} =
    {c;d = stat c.constrain (lazy (D.constrain cnstr d))}

  let serialize {d;c} =
    stat c.serialize (lazy (D.serialize d))

  let sat {d;c} cnstr =
    stat c.sat (lazy (D.sat d cnstr))

  let join a b =
    {a with
     d= stat a.c.join (lazy (D.join a.d b.d))}

  let widening a b =
    {a with
     d= stat a.c.widening (lazy (D.widening a.d b.d))}

  let meet a b =
    {a with
     d= stat a.c.meet (lazy (D.meet a.d b.d))}

  let le a b =
     stat a.c.le (lazy (D.le a.d b.d))

  let is_bottom {d;c} =
    stat c.is_bottom (lazy (D.is_bottom d))

  let forget syms {d;c} =
    {c;d= stat c.forget (lazy (D.forget syms d))}

  let rename_symbols map {d;c} =
    {c;d= stat c.rename_symbols (lazy (D.rename_symbols map d))}

  let query {d;c} =
    stat c.query (lazy (D.query d))

  let combine q {d;c} =
    {c;d= stat c.combine (lazy (D.combine q d))}

  let pp_print pp_sym ff t =
    Format.fprintf ff "@[<v 0>%a" (D.pp_print pp_sym) t.d;
    pp_stats ff t.c;
    Format.fprintf ff "@]"

  let pp_debug pp_sym ff t =
    pp_print pp_sym ff t
end
