module type L = sig
  val file: string
end

module S = LogicSymbolicSet

module Make(L: L)(D: Interface.Domain
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

  type ctx = {
    fout: out_channel;
    ff: Format.formatter;
    ctx: D.ctx;
  }

  type t = {
    t: D.t;
    c: ctx;
  }

  type sym = int

  type cnstr = int S.t

  type output = int S.t

  type query = int S.q

  let pp ff t =
    S.pp Format.pp_print_int ff (D.serialize t.t)

  let ppc ff c =
    S.pp Format.pp_print_int ff c

  let init () = 
    let fout = open_out L.file in
    let ff = Format.formatter_of_out_channel fout in
    let ctx = D.init () in
    { fout; ff; ctx }

  let top ctx =
    Format.fprintf ctx.ff "top@.";
    {
      t = D.top ctx.ctx;
      c = ctx;
    }

  let bottom ctx =
    Format.fprintf ctx.ff "bottom@.";
    {
      t = D.bottom ctx.ctx;
      c = ctx;
    }

  let context t = 
    Format.fprintf t.c.ff "context@.";
    t.c

  let symbols t =
    Format.fprintf t.c.ff "symbols: %a@." pp t;
    D.symbols t.t

  let constrain cnstr t =
    Format.fprintf t.c.ff "@[<v 2>constrain:@,";
    Format.fprintf t.c.ff "cnstr: %a@," ppc cnstr;
    Format.fprintf t.c.ff "pre  : %a@," pp t;
    let t = {t with t = D.constrain cnstr t.t} in
    Format.fprintf t.c.ff "post : %a" pp t;
    Format.fprintf t.c.ff "@]@.";
    t


  let serialize t =
    Format.fprintf t.c.ff "serialize: %a@." pp t;
    D.serialize t.t

  let sat t cnstr =
    Format.fprintf t.c.ff "@[<v 2>sat:@,";
    Format.fprintf t.c.ff "pre  : %a@," pp t;
    Format.fprintf t.c.ff "cnstr: %a@," ppc cnstr;
    let res = D.sat t.t cnstr in
    Format.fprintf t.c.ff "res  : %b" res;
    Format.fprintf t.c.ff "@]@.";
    res

  let join a b =
    Format.fprintf a.c.ff "@[<v 2>join:@,";
    Format.fprintf a.c.ff "a  : %a@," pp a;
    Format.fprintf a.c.ff "b  : %a@," pp b;
    let res = {a with
               t = D.join a.t b.t} in
    Format.fprintf a.c.ff "res: %a" pp res;
    Format.fprintf a.c.ff "@]@.";
    res

  let widening a b =
    Format.fprintf a.c.ff "@[<v 2>widening:@,";
    Format.fprintf a.c.ff "a  : %a@," pp a;
    Format.fprintf a.c.ff "b  : %a@," pp b;
    let res = {a with
               t = D.widening a.t b.t} in
    Format.fprintf a.c.ff "res: %a" pp res;
    Format.fprintf a.c.ff "@]@.";
    res

  let meet a b =
    Format.fprintf a.c.ff "@[<v 2>meet:@,";
    Format.fprintf a.c.ff "a  : %a@," pp a;
    Format.fprintf a.c.ff "b  : %a@," pp b;
    let res = {a with
               t = D.meet a.t b.t} in
    Format.fprintf a.c.ff "res: %a" pp res;
    Format.fprintf a.c.ff "@]@.";
    res

  let le a b =
    Format.fprintf a.c.ff "@[<v 2>le:@,";
    Format.fprintf a.c.ff "a  : %a@," pp a;
    Format.fprintf a.c.ff "b  : %a@," pp b;
    let res = D.le a.t b.t in
    Format.fprintf a.c.ff "res: %b" res;
    Format.fprintf a.c.ff "@]@.";
    res

  let is_bottom t =
    Format.fprintf t.c.ff "@[<v 2>is_bottom:@,";
    Format.fprintf t.c.ff "pre: %a@," pp t;
    let res = D.is_bottom t.t in
    Format.fprintf t.c.ff "res: %b" res;
    Format.fprintf t.c.ff "@]@.";
    res


  let forget syms t =
    Format.fprintf t.c.ff "@[<v 2>forget:@,";
    Format.fprintf t.c.ff "syms: @[<h>%a@]@," (Format.pp_print_list Format.pp_print_int) syms;
    Format.fprintf t.c.ff "pre : %a@," pp t;
    let res = {t with t = D.forget syms t.t} in
    Format.fprintf t.c.ff "res: %a" pp res;
    Format.fprintf t.c.ff "@]@.";
    res

  let rename_symbols map t =
    Format.fprintf t.c.ff "@[<v 2>rename_symbols:@,";
    Format.fprintf t.c.ff "pre : %a@," pp t;
    let res = {t with t = D.rename_symbols map t.t} in
    Format.fprintf t.c.ff "res: %a" pp res;
    Format.fprintf t.c.ff "@]@.";
    res

  let query t =
    Format.fprintf t.c.ff "@[<v 2>query:@,";
    Format.fprintf t.c.ff "pre: %a" pp t;
    Format.fprintf t.c.ff "@]@.";
    D.query t.t

  let combine q t =
    Format.fprintf t.c.ff "@[<v 2>combine:@,";
    Format.fprintf t.c.ff "pre: %a@," pp t;
    let res = {t with t = D.combine q t.t} in
    Format.fprintf t.c.ff "res: %a" pp res;
    Format.fprintf t.c.ff "@]@.";
    res

  let pp_print pp_sym ff t =
    D.pp_print pp_sym ff t.t

  let pp_debug pp_sym ff t =
    D.pp_print pp_sym ff t.t


end
