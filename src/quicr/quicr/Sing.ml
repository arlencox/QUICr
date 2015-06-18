module L = LogicSymbolicSet

module Make(D: Interface.Domain
                  with type sym = int
                   and type cnstr = int L.t
                   and type output = int L.t
                   and type query = int L.q
                 )
  : Interface.Domain
    with type sym = int
     and type cnstr = int L.t
     and type output = int L.t
     and type query = int L.q
= struct

  module SS = Set.Make(struct
      type t = int
      let compare a b = b - a
    end)

  type ctx = D.ctx

  type t =
    {
      d : D.t;
      s : SS.t;
    }

  type sym = int

  type cnstr = int L.t

  type output = int L.t

  type query = int L.q

  let init = D.init

  let top ctx = {
    d = D.top ctx;
    s = SS.empty;
  }

  let bottom ctx = {
    d = D.bottom ctx;
    s = SS.empty;
  }

  let symbols ctx t = D.symbols ctx t.d

  let rec rewrite_e = function
    | L.Empty -> (L.Empty, SS.empty)
    | L.Universe -> (L.Universe, SS.empty)
    | L.DisjUnion(a,b) ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (L.DisjUnion(a,b), SS.union ai bi)
    | L.Union(a,b) ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (L.Union(a,b), SS.union ai bi)
    | L.Inter(a,b) ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (L.Inter(a,b), SS.union ai bi)
    | L.Diff(a,b) ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (L.Diff(a,b), SS.union ai bi)
    | L.Comp(a) ->
      let (a,ai) = rewrite_e a in
      (L.Comp a, ai)
    | L.Var v ->
      (L.Var v, SS.empty)
    | L.Sing v ->
      (L.Var v, SS.singleton v)

  let rec rewrite is_pos t =
    let opt_n e =
      if is_pos then
        e
      else
        L.Not e
    in
    match t, is_pos with
    | L.Eq(a,b), _ ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (opt_n (L.Eq(a,b)), SS.union ai bi)
    | L.SubEq(a,b), _ ->
      let (a,ai) = rewrite_e a in
      let (b,bi) = rewrite_e b in
      (opt_n (L.SubEq(a,b)), SS.union ai bi)
    | L.In (a,b), true ->
      let (b,bi) = rewrite_e b in
      (L.SubEq(L.Var a, b), SS.add a bi)
    | L.In (a,b), false ->
      let (b,bi) = rewrite_e b in
      (L.Eq(L.Inter(L.Var a, b), L.Empty), SS.add a bi)
    | L.And(a,b), _ ->
      let (a,ai) = rewrite true a in
      let (b,bi) = rewrite true b in
      (opt_n (L.And(a,b)), SS.union ai bi)
    | L.Not a, n ->
      rewrite (not n) a
    | L.True, true -> (L.True, SS.empty)
    | L.True, false -> (L.False, SS.empty)
    | L.False, true -> (L.False, SS.empty)
    | L.False, false -> (L.True, SS.empty)

  let rewrite = rewrite true

  let constrain ctx c {d;s} =
    let (c,ci) = rewrite c in
    let d = D.constrain ctx c d in
    let s = SS.union s ci in
    {d;s}

  let serialize ctx {d;s} =
    let o = D.serialize ctx d in
    L.map_sym (fun is_sing id ->
        if is_sing || SS.mem id s then
          L.Sing id
        else
          L.Var id
      ) o

  let sat ctx a c =
    let (c,_ci) = rewrite c in
    D.sat ctx a.d c

  let join ctx a b =
    {
      d = D.join ctx a.d b.d;
      s = SS.union a.s b.s;
    }

  let widening ctx a b =
    {
      d = D.widening ctx a.d b.d;
      s = SS.union a.s b.s;
    }

  let meet ctx a b =
    {
      d = D.meet ctx a.d b.d;
      s = SS.union a.s b.s;
    }

  let le ctx a b =
    D.le ctx a.d b.d

  let is_bottom ctx a =
    D.is_bottom ctx a.d

  let is_top ctx a =
    D.is_top ctx a.d

  let forget ctx syms a = 
    {
      d = D.forget ctx syms a.d;
      s = List.fold_left (fun s e -> SS.remove e s) a.s syms;
    }

  let rename_symbols ctx rename a =
    {
      d = D.rename_symbols ctx rename a.d;
      s = SS.fold (fun e s -> SS.add (Rename.get rename e) s) a.s SS.empty;
    }

  let query ctx a =
    D.query ctx a.d

  let combine ctx q a = 
    {a with d = D.combine ctx q a.d}

  let pp_sym pp_sym t ff s =
    if SS.mem s t.s then
      Format.fprintf ff "{%a}" pp_sym s
    else
      pp_sym ff s

  let pp_print ctx pp ff t =
    D.pp_print ctx (pp_sym pp t) ff t.d

  let pp_debug ctx pp ff t =
    D.pp_print ctx (pp_sym pp t) ff t.d


end
