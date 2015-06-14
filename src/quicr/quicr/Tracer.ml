module type L = sig
  val file: string
  val check: bool
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
    counter: int ref;
  }

  let fresh_id ctx =
    let id = !(ctx.counter) in
    incr ctx.counter;
    id

  type t = {
    id: int;
    t: D.t;
    c: ctx;
  }

  type sym = int

  type cnstr = int S.t

  type output = int S.t

  type query = int S.q

  let pp ff t =
    D.pp_print Format.pp_print_int ff t.t
    (*S.pp Format.pp_print_int ff (D.serialize t.t)*)

  let ppc ff c =
    S.pp Format.pp_print_int ff c

  let init () = 
    let fout = open_out L.file in
    let ff = Format.formatter_of_out_channel fout in
    let ctx = D.init () in
    let counter = ref 0 in
    { fout; ff; ctx; counter }

  let top ctx =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = top@." id;
    {
      id;
      t = D.top ctx.ctx;
      c = ctx;
    }

  let bottom ctx =
    let id = fresh_id ctx in
    Format.fprintf ctx.ff "let x%d = bottom@." id;
    {
      id;
      t = D.bottom ctx.ctx;
      c = ctx;
    }

  let context t = 
    t.c

  let symbols t =
    D.symbols t.t

  let constrain cnstr t =
    let id = fresh_id t.c in
    Format.fprintf t.c.ff "let x%d = constrain %a x%d@." id (LogicSymbolicSet.pp ~parse:true Format.pp_print_int) cnstr t.id;
    let dt = D.constrain cnstr t.t in
    if L.check then Format.fprintf t.c.ff "sat x%d %a@." id (LogicSymbolicSet.pp ~parse:true Format.pp_print_int) (D.serialize dt);
    {t with t = dt; id}


  let serialize t =
    D.serialize t.t

  let sat t cnstr =
    Format.fprintf t.c.ff "sat x%d %a@." t.id (LogicSymbolicSet.pp ~parse:true Format.pp_print_int) cnstr;
    D.sat t.t cnstr

  let join a b =
    let id = fresh_id a.c in
    Format.fprintf a.c.ff "let x%d = join x%d x%d@." id a.id b.id;
    {a with t = D.join a.t b.t; id}

  let widening a b =
    let id = fresh_id a.c in
    Format.fprintf a.c.ff "let x%d = widening x%d x%d@." id a.id b.id;
    {a with t = D.widening a.t b.t; id}

  let meet a b =
    let id = fresh_id a.c in
    Format.fprintf a.c.ff "let x%d = meet x%d x%d@." id a.id b.id;
    {a with t = D.meet a.t b.t; id}

  let le a b =
    Format.fprintf a.c.ff "le x%d x%d@." a.id b.id;
    D.le a.t b.t

  let is_bottom t =
    Format.fprintf t.c.ff "is_bottom x%d@." t.id;
    D.is_bottom t.t

  let is_top t =
    Format.fprintf t.c.ff "is_top x%d@." t.id;
    D.is_top t.t

  let forget syms t =
    let id = fresh_id t.c in
    Format.fprintf t.c.ff "let x%d = forget %a x%d@." id
      (Format.pp_print_list ~pp_sep:(fun ff () -> Format.pp_print_string ff " ") Format.pp_print_int) syms
      t.id;
    {t with t = D.forget syms t.t; id}

  let rename_symbols map t =
    let id = fresh_id t.c in
    let l = Rename.to_assoc_list map in
    Format.fprintf t.c.ff "let x%d = rename [%a] x%d@." id
      (Format.pp_print_list ~pp_sep:(fun ff () -> Format.pp_print_string ff "; ") (fun ff (a,b) -> Format.fprintf ff "%d -> %d" a b)) l
      t.id;
    {t with t = D.rename_symbols map t.t; id}

  let query t =
    failwith "query unsupported in trace"
    (*D.query t.t*)

  let combine q t =
    failwith "combine unsupported in trace"
    (*{t with t = D.combine q t.t}*)

  let pp_print pp_sym ff t =
    D.pp_print pp_sym ff t.t

  let pp_debug pp_sym ff t =
    D.pp_print pp_sym ff t.t


end
