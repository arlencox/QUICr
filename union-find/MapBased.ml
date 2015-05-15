let debug = false

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module Make(C: Comparable)
  : UFInterface.UnionFind
    with type elt = C.t
= struct
  module EMap = Map.Make(C)
  module ESet = Set.Make(C)

  type ctx = unit

  type elt = C.t

  type t = {
    e2r: elt EMap.t;
    r2e: ESet.t EMap.t;
  }

  let empty () = {
    e2r = EMap.empty;
    r2e = EMap.empty;
  }

  let init () = ()

  let context t = ()

  let mem e t =
    EMap.mem e t.e2r

  type rem =
    | NoRepresentative
    | SameRepresentative
    | NewRepresentative of elt

  let remove e t =
    try
      let r = EMap.find e t.e2r in
      if C.compare e r = 0 then
        (* removing representative *)
        let es = EMap.find r t.r2e in
        let es = ESet.remove r es in
        if ESet.is_empty es then
          (* removed sole element *)
          ({ r2e = EMap.remove r t.r2e; e2r = EMap.remove e t.e2r}, NoRepresentative)
        else
          let r2e = EMap.remove r t.r2e in
          (* select new representative *)
          let r = ESet.min_elt es in
          let r2e = EMap.add r es r2e in
          (* update set to point to new representative *)
          let e2r = EMap.remove e t.e2r in
          let e2r = ESet.fold (fun e e2r -> EMap.add e r e2r) es e2r in
          ({ r2e; e2r }, NewRepresentative r)
      else
        (* removing non-representative *)
        let es = EMap.find r t.r2e in
        let es = ESet.remove e es in
        let r2e = EMap.add r es t.r2e in
        let e2r = EMap.remove e t.e2r in
        ({ r2e; e2r }, SameRepresentative)
    with Not_found ->
      (t, NoRepresentative)

  let rep e t =
    try EMap.find e t.e2r with Not_found -> e

  let reps t =
    EMap.bindings t.r2e |>
    List.map fst

  let element_set e t =
    try
      EMap.find (rep e t) t.r2e
    with Not_found ->
      ESet.singleton e

  let elements e t =
    element_set e t

  let union e1 e2 t =
    let r1 = rep e1 t in
    let r2 = rep e2 t in
    let (r1,r2) = if C.compare r1 r2 > 0 then (r2,r1) else (r1,r2) in
    (* r1 becomes new representative *)
    let r1s = element_set r1 t in
    let r2s = element_set r2 t in
    let r2e = EMap.remove r2 t.r2e in
    let r2e = EMap.add r1 (ESet.union r1s r2s) r2e in
    let e2r = ESet.fold (fun e e2r -> EMap.add e r1 e2r) r2s t.e2r in
    { e2r; r2e }

  module CEMap = Map.Make(struct
      type t = C.t * C.t
      let compare (a1,a2) (b1,b2) =
        let res = C.compare a1 b1 in
        if res <> 0 then res
        else C.compare a2 b2
    end)

  let compare t1 t2 =
    let res = EMap.compare C.compare t1.e2r t2.e2r in
    if res <> 0 then res
    else EMap.compare ESet.compare t1.r2e t2.r2e

  let equal t1 t2 =
    compare t1 t2 = 0

  let fold f t r =
    EMap.fold f t.e2r r

  (*
     1 2 3 4
     1 1 3 3 (* r1 = [1; 3]    o1 = [3]    [3 -> 2]         *)
     1 2 2 4 (* r2 = [1; 2; 4] o2 = [2; 4] [2 -> 1; 4 -> 3] *)
     1 1 1 1 
  *)

  (* m *)
  (*let merge t1 t2 =
    (* compute representative sets *)
    let r1 = EMap.fold (fun r _ r1 -> ESet.add r r1) t1.r2e ESet.empty in
    let r2 = EMap.fold (fun r _ r2 -> ESet.add r r2) t2.r2e ESet.empty in
    (* identify representatives that are only in t1 and t2 respectively *)
    let o1 = ESet.diff r1 r2 in
    let o2 = ESet.diff r2 r1 in
    (* compute merges to change 1 into result *)
    let (res,d1) = ESet.fold (fun e2 (res,d1) ->
        let r2 = try set e2 t2 with Not_found -> e2 in
        (
          union e2 r2 res,
          (e2,r2)::d1
        )
      ) o1 (t1,[]) in
    (* compute merges to change 2 into result *)
    let d2 = ESet.fold (fun e1 d2 ->
        let r1 = try set e1 t1 with Not_found -> e1 in
        (e1,r1)::d2
      ) o2 [] in
    (* as a check, compute result from 2 *)
    if debug then begin
      let res2 = List.fold_left (fun res2 (e1,r1) -> union e1 r1 res2) t2 d2 in
      assert (equal res res2)
    end;

    (res,d1,d2)*)

  let merge t1 t2 =
    let res = t1 in
    let res = EMap.fold (fun e r res ->
        res |>
        union e r
      ) t2.e2r res in
    res

  let split t1 t2 =
    let h = Hashtbl.create 8191 in
    let res = ref (empty ()) in
    ignore(EMap.merge (fun e _ _ ->
        let r1 = rep e t1 in
        let r2 = rep e t2 in
        begin try
          let r = Hashtbl.find h (r1,r2) in
          res := union e r !res
          with Not_found ->
            Hashtbl.replace h (r1,r2) e;
            res := union e e !res
        end;
        None
      ) t1.e2r t2.e2r);
    !res

  let diff t1 t2 =
    snd (fold (fun e r (t1,tm) ->
        let r' = rep e t1 in
        if C.compare r e <> 0 && C.compare r' e = 0 then
          (union e r t1, (e,r)::tm)
        else
          (t1,tm)
      ) t2 (t1,[]))

  let rename map t =
    fold (fun e r t ->
        let e = map e in
        let r = map r in
        union e r t
      ) t (empty ())



end
