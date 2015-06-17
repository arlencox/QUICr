(* Binary decision diagrams *)

module type VAR = sig
  type t
  val compare: t -> t -> int
  val hash: t -> int
  val equal: t -> t -> bool
  val pp: Format.formatter -> t -> unit
end

module Make(Var: VAR) = struct

  type var = Var.t

  type t =
    | False
    | True
    | If of t * Var.t * t * int

  module HashedIf = struct
    type t = int * Var.t * int
    let equal (l1,v1,r1) (l2,v2,r2) =
      l1 = l2 && r1 = r2 && Var.equal v1 v2
    let hash (l,v,r) =
      l * 3 + r * 7 + Var.hash v
  end

  module IfHashCons = Hashtbl.Make(HashedIf)

  type ctx = {
    bdd_hc : t IfHashCons.t;
    bdd_hc_stamp : int ref;
    not_cache : (int,t) Hashtbl.t;
    and_cache : (int * int,t) Hashtbl.t;
    or_cache : (int * int,t) Hashtbl.t;
    xor_cache : (int * int,t) Hashtbl.t;
  }

  let init () = {
    bdd_hc = IfHashCons.create 2047;
    bdd_hc_stamp = ref 2;
    not_cache = Hashtbl.create 511;
    and_cache = Hashtbl.create 511;
    or_cache = Hashtbl.create 511;
    xor_cache = Hashtbl.create 511;
  }

  let clear ctx = 
    IfHashCons.clear ctx.bdd_hc;
    Hashtbl.clear ctx.not_cache;
    Hashtbl.clear ctx.and_cache;
    Hashtbl.clear ctx.or_cache;
    Hashtbl.clear ctx.xor_cache


  let false_ ctx = False
  let true_ ctx = True

  let is_true ctx l =
    match l with
    | True -> true
    | _ -> false

  let is_false ctx l =
    match l with
    | False -> true
    | _ -> false

  let ident ctx = function
    | False -> 0
    | True -> 1
    | If(_,_,_,id) -> id

  let equal ctx f1 f2 =
    ident f1 = ident f2

  let mkif ctx l v r =
    let il = ident ctx l and ir = ident ctx r in
    if il = ir then l else begin
      let key = (il, v, ir) in
      try
        IfHashCons.find ctx.bdd_hc key
      with Not_found ->
        let id = !(ctx.bdd_hc_stamp) in
        ctx.bdd_hc_stamp := id + 1;
        let f = If(l, v, r, id) in
        IfHashCons.add ctx.bdd_hc key f;
        f
    end

  (* Variable *)

  let var_ ctx v = mkif ctx True v False

  (* Negation *)

  let rec not_ ctx f =
    match f with
    | False -> True
    | True -> False
    | If(l, v, r, id) ->
      (*try
        Hashtbl.find ctx.not_cache id
      with Not_found ->*)
        let nf = mkif ctx (not_ ctx l) v (not_ ctx r) in
        Hashtbl.add ctx.not_cache id nf;
        nf

  (* Conjunction *)

  let rec and_ ctx f1 f2 =
    match (f1, f2) with
    | (False, _) -> False
    | (True, _) -> f2
    | (_, False) -> False
    | (_, True) -> f1
    | (If(l1, v1, r1, id1), If(l2, v2, r2, id2)) ->
      if id1 = id2 then f1 else begin
        let key = if id1 < id2 then (id1, id2) else (id2, id1) in
        try
          Hashtbl.find ctx.and_cache key
        with Not_found ->
          let c = Var.compare v1 v2 in
          let f =
            if c = 0 then
              mkif ctx (and_ ctx l1 l2) v1 (and_ ctx r1 r2)
            else if c < 0 then
              mkif ctx (and_ ctx l1 f2) v1 (and_ ctx r1 f2)
            else
              mkif ctx (and_ ctx f1 l2) v2 (and_ ctx f1 r2) in
          Hashtbl.add ctx.and_cache key f;
          f
      end

  (* Disjunction *)

  (*let or_ ctx f1 f2 =
    not_ ctx (and_ ctx (not_ ctx f1) (not_ ctx f2))*)

  let rec or_ ctx f1 f2 =
    match (f1, f2) with
    | (False, _) -> f2
    | (True, _) -> True
    | (_, False) -> f1
    | (_, True) -> True
    | (If(l1, v1, r1, id1), If(l2, v2, r2, id2)) ->
      if id1 = id2 then f1 else begin
        let key = if id1 < id2 then (id1, id2) else (id2, id1) in
        try
          Hashtbl.find ctx.or_cache key
        with Not_found ->
          let c = Var.compare v1 v2 in
          let f =
            if c = 0 then
              mkif ctx (or_ ctx l1 l2) v1 (or_ ctx r1 r2)
            else if c < 0 then
              mkif ctx (or_ ctx l1 f2) v1 (or_ ctx r1 f2)
            else
              mkif ctx (or_ ctx f1 l2) v2 (or_ ctx f1 r2) in
          Hashtbl.add ctx.or_cache key f;
          f
      end

  (* Exclusive or *)

  (*let xor_ ctx f1 f2 =
    or_ ctx
      (and_ ctx f1 (not_ ctx f2))
      (and_ ctx f2 (not_ ctx f1))*)

  let rec xor_ ctx f1 f2 =
    match (f1, f2) with
    | (False, _) -> f2
    | (True, _) -> not_ ctx f2
    | (_, False) -> f1
    | (_, True) -> not_ ctx f1
    | (If(l1, v1, r1, id1), If(l2, v2, r2, id2)) ->
      if id1 = id2 then False else begin
        let key = if id1 < id2 then (id1, id2) else (id2, id1) in
        try
          Hashtbl.find ctx.xor_cache key
        with Not_found ->
          let c = Var.compare v1 v2 in
          let f =
            if c = 0 then
              mkif ctx (xor_ ctx l1 l2) v1 (xor_ ctx r1 r2)
            else if c < 0 then
              mkif ctx (xor_ ctx l1 f2) v1 (xor_ ctx r1 f2)
            else
              mkif ctx (xor_ ctx f1 l2) v2 (xor_ ctx f1 r2) in
          Hashtbl.add ctx.xor_cache key f;
          f
      end

  (* Existential Quantification *)

  let rec exists_ ctx evl f =
    match evl,f with
    | [], f -> f
    | _, False -> False
    | _, True -> True
    | ev::tl, If(l,v,r,_id) ->
      let c = Var.compare v ev in
      if c < 0 then
        mkif ctx (exists_ ctx evl l) v (exists_ ctx evl r)
      else if c = 0 then
        or_ ctx (exists_ ctx tl l) (exists_ ctx tl r)
      else
        exists_ ctx tl f

  let exists_ ctx evl f =
    exists_ ctx (List.sort Var.compare evl) f

  (* Universal Quantification *)

  let forall_ ctx v f =
    not_ ctx (exists_ ctx v (not_ ctx f))

  (*let rec forall_ ctx ev f =
    match f with
      | False -> False
      | True -> True
      | If(l,v,r,_id) ->
        let c = Var.compare v ev in
        if c < 0 then
    	mkif ctx (forall_ ctx ev l) v (forall_ ctx ev r)
        else if c = 0 then
    	and_ ctx l r
        else
    	f
  *)

  (* Implication *)

  let imply_ ctx f1 f2 = or_ ctx (not_ ctx f1) f2

  (* Equivalence *)

  let equiv_ ctx f1 f2 = not_ ctx (xor_ ctx f1 f2)

  (* If-Then-Else *)

  let ite_ ctx c f1 f2 = and_ ctx (imply_ ctx c f1) (imply_ ctx (not_ ctx c) f2)

  (* Convert to expression *)

  (*let log_if c t e =
    Logic.And [Logic.Or [Logic.Not c; t]; Logic.Or [c; e]]*)

  (*let rec to_expr ctx = function
    | True -> Logic.True
    | False -> Logic.False
    | If (l,v,r,_id) ->
      log_if (Logic.Var v) (to_expr ctx l) (to_expr ctx r)*)

  (* Pretty Print *)

  let rec pp ctx ff = function
    | True -> Format.fprintf ff "true"
    | False -> Format.fprintf ff "false"
    | If (l,v,r,_id) -> Format.fprintf ff "ite(%a,%a,%a)" Var.pp v (pp ctx) l (pp ctx) r

  (* Vistor *)

  type 'a visitor = {
    true_ : 'a;
    false_ : 'a;
    if_ : var -> 'a -> 'a -> 'a
  }

  let rec visit visited ctx visitor = function
    | True -> 
      visitor.true_
    | False ->
      visitor.false_
    | If(l,v,r,id) ->
      try Hashtbl.find visited id
      with Not_found ->
        let resl = visit visited ctx visitor l in
        let resr = visit visited ctx visitor r in
        let res = visitor.if_ v resl resr in
        Hashtbl.replace visited id res;
        res

  let visit ctx visitor =
    let visited = Hashtbl.create 1023 in
    visit visited ctx visitor


  let support ctx t =
    let module St = Set.Make(Var) in
    let vs = visit ctx {
        true_ = St.empty;
        false_ = St.empty;
        if_ = (fun v l r -> St.add v @@ St.union l r);
      } t in
    St.elements vs

    

  (*let stats ctx t =
    let module St = Set.Make(Var) in
    let count = ref 0 in
    let (d,vs) = visit ctx {
        true_ = (0,St.empty);
        false_ = (0,St.empty);
        if_ = (fun v (ld,l) (rd,r) ->
            incr count;
            let d = max ld rd in
            let vs = St.add v @@ St.union l r in
            (d,vs)
          );
      } t in
    {
      Logic.nodes = !count;
      Logic.vars = St.cardinal vs;
      Logic.depth = d;
    }*)

end
