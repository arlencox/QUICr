module S = LogicSymbolicSet

module Inner : sig
  type t = int
  val of_const : int -> t
  val incr : t ref -> unit
end = struct
  type t = int
  let of_const i = i
  let incr i =
    incr i
end

module Make(D: Interface.Domain
                  with type sym = Inner.t
                   and type cnstr = Inner.t S.t
                   and type output = Inner.t S.t
                   and type query = Inner.t S.q
                 )
  : Interface.Domain
    with type sym = int
     and type cnstr = int S.t
     and type output = int S.t
     and type query = int S.q
= struct


  type ctx = {
    c: D.ctx;
    m: (int,Inner.t) Hashtbl.t;
    r: (Inner.t,int) Hashtbl.t;
    i: Inner.t ref;
  }

  type t = D.t

  type sym = int

  type cnstr = int S.t

  type output = int S.t

  type query = int S.q

  let init () =
    {
      c = D.init ();
      m = Hashtbl.create 1023;
      r = Hashtbl.create 1023;
      i = ref (Inner.of_const 0);
    }

  let out_to_in ctx out_id =
    try
      Hashtbl.find ctx.m out_id
    with Not_found ->
      let in_id = !(ctx.i) in
      Inner.incr ctx.i;
      Hashtbl.replace ctx.m out_id in_id;
      Hashtbl.replace ctx.r in_id out_id;
      in_id

  exception Remap_unmapped

  let in_to_out ctx in_id =
    try
      Hashtbl.find ctx.r in_id
    with Not_found ->
      raise Remap_unmapped
      (*failwith "Returned internal id that doesn't correspond to external id"*)

  let out_cst_to_in_cst ctx out_cst =
    S.map_symbol (out_to_in ctx) out_cst

  let in_cst_to_out_cst ctx in_cst =
    S.map_symbol (in_to_out ctx) in_cst


  let top ctx = D.top ctx.c

  let bottom ctx = D.bottom ctx.c

  let symbols ctx t =
    List.map (in_to_out ctx) (D.symbols ctx.c t)

  let constrain ctx c t =
    D.constrain ctx.c (out_cst_to_in_cst ctx c) t

  let serialize ctx t =
    in_cst_to_out_cst ctx (D.serialize ctx.c t)

  let sat ctx t c =
    D.sat ctx.c t (out_cst_to_in_cst ctx c)

  let join ctx = D.join ctx.c

  let widening ctx = D.widening ctx.c

  let meet ctx = D.meet ctx.c

  let le ctx = D.le ctx.c

  let is_top ctx = D.is_top ctx.c

  let is_bottom ctx = D.is_bottom ctx.c

  let forget ctx syms t =
    D.forget ctx.c (List.map (out_to_in ctx) syms) t

  let rename_symbols ctx rename t =
    let iter f = Rename.iter (fun s1 s2 ->
        let s1 = out_to_in ctx s1 in
        let s2 = out_to_in ctx s2 in
        f s1 s2
      ) rename in
    let mem s = Rename.mem rename (in_to_out ctx s) in
    let get s =
      try
        let s = in_to_out ctx s in
        let r = Rename.get rename s in
        out_to_in ctx r
      with Remap_unmapped ->
        s
    in
    let rename' = Rename.of_iter_mem_get iter mem get in
    D.rename_symbols ctx.c rename' t

  let query ctx t =
    let q = D.query ctx.c t in
    let get_eqs () =
      List.map (fun (a,b) -> (in_to_out ctx a, in_to_out ctx b)) (q.S.get_eqs ())
    in
    let get_eqs_sym a =
      let in_syms = q.S.get_eqs_sym (out_to_in ctx a) in
      List.map (in_to_out ctx) in_syms
    in
    {
      S.get_eqs;
      S.get_eqs_sym;
    }

  let combine ctx q =
    let get_eqs () =
      List.map (fun (a,b) -> (out_to_in ctx a, out_to_in ctx b)) (q.S.get_eqs ())
    in
    let get_eqs_sym a =
      let in_syms = q.S.get_eqs_sym (in_to_out ctx a) in
      List.map (out_to_in ctx) in_syms
    in
    let q' = {
      S.get_eqs;
      S.get_eqs_sym;
    } in
    D.combine ctx.c q'

  let pp_print ctx pp_sym ff t =
    let pp_sym ff s =
      pp_sym ff (in_to_out ctx s)
    in
    D.pp_debug ctx.c pp_sym ff t

  let pp_debug ctx pp_sym ff t =
    let ss = ref DS.Sette.empty in
    
    D.pp_print ctx.c (fun ff i ->
        ss := DS.Sette.add i !ss;
        Format.fprintf ff "%%%d" i
      ) ff t;
    Format.fprintf ff "@,";
    let first = ref true in
    DS.Sette.iter (fun ins ->
        let outs = in_to_out ctx ins in
        if !first then
          first := false
        else
          Format.fprintf ff "; ";
        Format.fprintf ff "%a -> %%%d" pp_sym outs ins
      ) !ss

end
