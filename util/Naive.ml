let debug = true

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module Make(C: Comparable)
  : Sets.Interface 
    with type elt = C.t
= struct
  module CMap = Map.Make(C)
  module CSet = Set.Make(C)

  type elt = C.t

  type t = {
    e2r: elt CMap.t;
    r2e: CSet.t CMap.t;
  }

  let empty = {
    e2r = CMap.empty;
    r2e = CMap.empty;
  }

  let mem e t =
    CMap.mem e t.e2r

  type rem =
    | NoRepresentative
    | SameRepresentative
    | NewRepresentative of elt

  let remove e t =
    try
      let r = CMap.find e t.e2r in
      if C.compare e r = 0 then
        (* removing representative *)
        let es = CMap.find r t.r2e in
        let es = CSet.remove r es in
        if CSet.is_empty es then
          (* removed sole element *)
          ({ r2e = CMap.remove r t.r2e; e2r = CMap.remove e t.e2r}, NoRepresentative)
        else
          let r2e = CMap.remove r t.r2e in
          (* select new representative *)
          let r = CSet.min_elt es in
          let r2e = CMap.add r es r2e in
          (* update set to point to new representative *)
          let e2r = CMap.remove e t.e2r in
          let e2r = CSet.fold (fun e e2r -> CMap.add e r e2r) es e2r in
          ({ r2e; e2r }, NewRepresentative r)
      else
        (* removing non-representative *)
        let es = CMap.find r t.r2e in
        let es = CSet.remove e es in
        let r2e = CMap.add r es t.r2e in
        let e2r = CMap.remove e t.e2r in
        ({ r2e; e2r }, SameRepresentative)
    with Not_found ->
      (t, NoRepresentative)

  let rep e t =
    try CMap.find e t.e2r with Not_found -> e

  let reps t =
    CMap.bindings t.r2e |>
    List.map fst

  let element_set e t =
    try
      CMap.find (rep e t) t.r2e
    with Not_found ->
      CSet.singleton e

  let elements e t =
    CSet.elements @@ element_set e t

  let union e1 e2 t =
    let r1 = rep e1 t in
    let r2 = rep e2 t in
    let (r1,r2) = if C.compare r1 r2 > 0 then (r2,r1) else (r1,r2) in
    (* r1 becomes new representative *)
    let r1s = element_set r1 t in
    let r2s = element_set r2 t in
    let r2e = CMap.remove r2 t.r2e in
    let r2e = CMap.add r1 (CSet.union r1s r2s) r2e in
    let e2r = CSet.fold (fun e e2r -> CMap.add e r1 e2r) r2s t.e2r in
    { e2r; r2e }

  module CCMap = Map.Make(struct
      type t = C.t * C.t
      let compare (a1,a2) (b1,b2) =
        let res = C.compare a1 b1 in
        if res <> 0 then res
        else C.compare a2 b2
    end)

  let compare t1 t2 =
    let res = CMap.compare C.compare t1.e2r t2.e2r in
    if res <> 0 then res
    else CMap.compare CSet.compare t1.r2e t2.r2e

  let equal t1 t2 =
    compare t1 t2 = 0

  let fold f t r =
    CMap.fold f t.e2r r

  (*
     1 2 3 4
     1 1 3 3 (* r1 = [1; 3]    o1 = [3]    [3 -> 2]         *)
     1 2 2 4 (* r2 = [1; 2; 4] o2 = [2; 4] [2 -> 1; 4 -> 3] *)
     1 1 1 1 
  *)

  (* m *)
  (*let merge t1 t2 =
    (* compute representative sets *)
    let r1 = CMap.fold (fun r _ r1 -> CSet.add r r1) t1.r2e CSet.empty in
    let r2 = CMap.fold (fun r _ r2 -> CSet.add r r2) t2.r2e CSet.empty in
    (* identify representatives that are only in t1 and t2 respectively *)
    let o1 = CSet.diff r1 r2 in
    let o2 = CSet.diff r2 r1 in
    (* compute merges to change 1 into result *)
    let (res,d1) = CSet.fold (fun e2 (res,d1) ->
        let r2 = try set e2 t2 with Not_found -> e2 in
        (
          union e2 r2 res,
          (e2,r2)::d1
        )
      ) o1 (t1,[]) in
    (* compute merges to change 2 into result *)
    let d2 = CSet.fold (fun e1 d2 ->
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
    let res = CMap.fold (fun e r res ->
        res |>
        union e r
      ) t2.e2r res in
    let build_map t =
      CMap.fold (fun r _ tm ->
          let r' = rep r res in
          if C.compare r r' <> 0 then
            (r,r')::tm
          else
            tm
        ) t.r2e [] 
    in
    let t1m = build_map t1 in
    let t2m = build_map t2 in
    (res,t1m,t2m)





  let partition t1 t2 =
    failwith "partition unimplemented"

  let rename map t =
    let map = List.fold_left (fun map (x,y) -> CMap.add x y map) CMap.empty map in
    let r2e,e2r,r2r = CMap.fold (fun r s (r2e,e2r,r2r) ->
        let s = CSet.fold (fun e s -> CSet.add (try CMap.find e map with Not_found -> e) s) s CSet.empty in
        let r' = CSet.min_elt s in
        let r2e = CMap.add r' s r2e in
        let e2r = CSet.fold (fun e e2r -> CMap.add e r' e2r) s e2r in
        if C.compare r r' = 0 then
          (r2e,e2r,r2r)
        else
          (r2e,e2r,(r,r')::r2r)
      ) t.r2e (CMap.empty,CMap.empty,[]) in
    (
      { r2e; e2r },
      r2r
    )


end
