
module L = LogicSymbolicSet
  
module H = DS.Hashcons


type l =
  | Var of int
  | And of c * c
  | Or of c * c
  | Not of c
  | Exists of int * c
  | ForAll of int * c
  | True
  | False
and c = l H.hash_consed

type h = l H.t

let mvar h v =
  H.hashcons h (Var v)

let mand h a b =
  let e = match a.H.node,b.H.node with
    | True, b -> b
    | a, True -> a
    | False, _
    | _, False -> False
    | _ -> And(a,b)
  in
  H.hashcons h e

let mor h a b =
  let e = match a.H.node,b.H.node with
    | False, b -> b
    | a, False -> a
    | True, _
    | _, True -> True
    | _ -> Or(a,b)
  in
  H.hashcons h e

let mnot h a =
  let e = match a.H.node with
    | False -> True
    | True -> False
    | Not a -> a.H.node
    | _ -> Not a
  in
  H.hashcons h e

let mexists h v a =
  let e = match a.H.node with
    | True -> True
    | False -> False
    | Var v' when v == v' -> True
    | _ -> Exists(v,a)
  in
  H.hashcons h e

let mforall h v a =
  let e = match a.H.node with
    | True -> True
    | False -> False
    | Var v' when v == v' -> False
    | _ -> ForAll(v,a)
  in
  H.hashcons h e

let mtrue h = H.hashcons h True

let mfalse h = H.hashcons h False

let mimply h a b =
  mor h (mnot h a) b

let meq h a b =
  mand h (mimply h a b) (mimply h b a)

type 'a v = {
  mtrue: 'a;
  mfalse: 'a;
  mvar: (int -> 'a);
  mand: ('a -> 'a -> 'a);
  mor: ('a -> 'a -> 'a);
  mnot: ('a -> 'a);
  mexists: (int -> 'a -> 'a);
  mforall: (int -> 'a -> 'a);
}

let rec visit visited v (e: c) =
  let vrec = visit visited v in
  try
    Hashtbl.find visited e.H.tag
  with Not_found ->
    let result = match e.H.node with
      | True -> v.mtrue
      | False -> v.mfalse
      | Var x -> v.mvar x
      | And (a,b) ->
        let a = vrec a in
        let b = vrec b in
        v.mand a b
      | Or (a,b) ->
        let a = vrec a in
        let b = vrec b in
        v.mor a b
      | Not a ->
        let a = vrec a in
        v.mnot a
      | Exists (x,a) ->
        let a = vrec a in
        v.mexists x a
      | ForAll (x,a) ->
        let a = vrec a in
        v.mforall x a
    in
    Hashtbl.replace visited e.H.tag result;
    result

module ISet = Set.Make(struct
    type t = int
    let compare = (-)
  end)

let symbols e =
  visit (Hashtbl.create 8191) {
    mtrue = ISet.empty;
    mfalse = ISet.empty;
    mvar = (fun x -> ISet.singleton x);
    mand = ISet.union;
    mor = ISet.union;
    mnot = (fun a -> a);
    mexists = (fun v a -> ISet.remove v a);
    mforall = (fun v a -> ISet.remove v a);
  } e

module type Solver = sig
  val is_valid : h -> c -> bool
end


module Make(S: Solver) : Interface.Domain
  with type sym = int
   and type cnstr = int L.t
   and type output = int L.t
   and type query = int L.q
= struct

  type sym = int

  type cnstr = int L.t
  type output = int L.t
  type query = int L.q


  type ctx = h

  let init () = H.create 8191

  type t = {
    ctx: ctx;
    e: c;
  }

  let top ctx =
    { ctx; e = mtrue ctx}

  let bottom ctx =
    { ctx; e = mfalse ctx}

  let context t = t.ctx

  let symbols t =
    ISet.elements @@ symbols t.e

  let rec of_expr ctx = function
    | L.Empty ->
      (mfalse ctx, mtrue ctx)
    | L.Universe ->
      (mtrue ctx, mtrue ctx)
    | L.DisjUnion (a,b) -> 
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (* A ^ B = 0
         A /\ B = false
         ~(A /\ B) *)
      (mor ctx a b, (mand ctx (mand ctx ac bc) (mnot ctx (mand ctx a b))))
    | L.Union (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (mor ctx a b, mand ctx ac bc)
    | L.Inter (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (mand ctx a b, mand ctx ac bc)
    | L.Diff (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (mand ctx a (mnot ctx b), mand ctx ac bc)
    | L.Comp a ->
      let (a,ac) = of_expr ctx a in
      (mnot ctx a, ac)
    | L.Var v ->
      (mvar ctx v, mtrue ctx)
    | L.Sing v ->
      (mvar ctx v, mtrue ctx)

  exception Unsupported

  let rec of_cnstr is_pos is_over ctx c =
    let r is_inv = of_cnstr is_inv is_over ctx in
    match c, is_pos with
    | L.Eq (a,b), true ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      mand ctx (meq ctx a b) (mand ctx ac bc)
    | L.Eq _, false ->
      if is_over then
        mtrue ctx
      else
        raise Unsupported
    | L.SubEq (a,b), true ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      mand ctx (mimply ctx a b) (mand ctx ac bc)
    | L.SubEq _, false ->
      if is_over then
        mtrue ctx
      else
        raise Unsupported
    | L.In (av,b), _ ->
      if is_over then
        mtrue ctx
      else
        raise Unsupported
    | L.And (a,b), true ->
      let a = r is_pos a in
      let b = r is_pos b in
      mand ctx a b

    | L.And (a,b), false ->
      let a = r is_pos a in
      let b = r is_pos b in
      if is_over then
        mor ctx a b
      else 
        raise Unsupported
    | L.Not a, _ ->
      r (not is_pos) a
    | L.True, false
    | L.False, true ->
      mfalse ctx
    | L.False, false
    | L.True, true ->
      mtrue ctx

  let constrain cnstr t =
    let c = of_cnstr true true (context t) cnstr in
    {t with e = mand t.ctx c t.e}

  let serialize t =
    failwith "SAT.Domain.serialize unimplemented"

  let join a b = {a with e = mor a.ctx a.e b.e}

  let widening = join

  let meet a b = {a with e = mand a.ctx a.e b.e}

  let sat t cnstr =
    try
      let c = of_cnstr true false (context t) cnstr in
      S.is_valid t.ctx (mimply t.ctx t.e c)
      (*MLBDD.is_true (MLBDD.imply t c)*)
    with Unsupported ->
      false

  let le a b =
    S.is_valid a.ctx (mimply a.ctx a.e b.e)

  let forget syms t =
    {t with e = List.fold_left (fun e v -> mexists t.ctx v e) t.e syms }

  let is_top t =
    S.is_valid t.ctx t.e

  let is_bottom t =
    S.is_valid t.ctx (mnot t.ctx t.e)

  let rec rename_symbols ctx visited rename ignore e =
    let rign = rename_symbols ctx visited rename in
    let rrec = rign ignore in
    try
      Hashtbl.find visited e.H.tag
    with Not_found ->
      let result = match e.H.node with
        | True -> mtrue ctx
        | False -> mfalse ctx
        | Var x ->
          if ISet.mem x ignore then
            mvar ctx x
          else
            mvar ctx (Rename.get rename x)
        | And (a,b) ->
          let a = rrec a in
          let b = rrec b in
          mand ctx a b
        | Or (a,b) ->
          let a = rrec a in
          let b = rrec b in
          mor ctx a b
        | Not a ->
          let a = rrec a in
          mnot ctx a
        | Exists (x,a) ->
          let a = rign (ISet.add x ignore) a in
          mexists ctx x a
        | ForAll (x,a) ->
          let a = rign (ISet.add x ignore) a in
          mforall ctx x a
      in
      Hashtbl.replace visited e.H.tag result;
      result


  let rename_symbols rename t =
    {t with e = rename_symbols t.ctx (Hashtbl.create 8191) rename ISet.empty t.e }

  let query t =
    {
      L.get_eqs = (fun () -> []);
      L.get_eqs_sym = (fun s -> []);
    }

  let combine q t =
    List.fold_left (fun t (s1, s2) ->
        constrain (L.Eq (L.Var s1, L.Var s2)) t
      ) t (q.L.get_eqs ())

  let pp_print pp_sym ff t =
    failwith "unimp"

  let pp_debug = pp_print

end
