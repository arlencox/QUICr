module type S = sig
  type t
  type elt
  val empty : t
  val add : elt -> t -> t
  val get_representative : elt -> t -> t * elt
  val union : elt -> elt -> t -> t
  val mem : elt -> t -> bool
  val fmt : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
  val fold : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a
  val normalize : t -> t
  val meet_raw : t -> t -> t * t * t
  val eqs : t -> (elt * elt) list

  type remove_result =
    | Representative of elt
    | NoRepresentative
    | RepresentativeSame

  val remove : elt -> t -> t * remove_result

end

module type Ordered = sig
  type t
  val compare: t -> t -> int
end

module Make(T : Ordered) : S with type elt = T.t =
struct
  module TMap = Map.Make(T)

  type elt = T.t

  type t = elt TMap.t

  let empty = TMap.empty

  let add v uf =
    if TMap.mem v uf then
      uf
    else
      TMap.add v v uf

  let rec get_representative v uf =
    try
      let rep = TMap.find v uf in
      if (T.compare rep v) = 0 then
        (uf,rep)
      else begin
        let (uf, rep) = get_representative rep uf in
        (TMap.add v rep uf, rep)
      end
    with Not_found ->
      (TMap.add v v uf, v)

  let union v1 v2 uf =
    let (uf,v1_rep) = get_representative v1 uf in
    let (uf,v2_rep) = get_representative v2 uf in
    if T.compare v1_rep v2_rep < 0 then
      TMap.add v2_rep v1_rep uf
    else
      TMap.add v1_rep v2_rep uf

  let mem v uf =
    TMap.mem v uf

  let fmt fmt_el ff uf =
    let first = ref true in
    TMap.iter (fun v rep ->
      if !first then
        first := false
      else
        Format.fprintf ff "@ ";
      Format.fprintf ff "%a -> %a" fmt_el v fmt_el rep
    ) uf

  let fold f t r = TMap.fold f t r

  module TTMap = Map.Make(struct
      type t = T.t * T.t

      let compare ((a1,b1) as v1) ((a2,b2) as v2) =
        if v1 == v2 then 0
        else
          let res = T.compare a1 a2 in
          if res == 0 then
            T.compare b1 b2
          else
            res
    end)

  let normalize uf =
    let normalize uf k v =
      let (_uf,v) = get_representative k uf in
      v
    in
    TMap.mapi (normalize uf) uf


  (* from http://www.cs.cmu.edu/~ab/Desktop/15-211%20Archive/res00035/Lecture26MRMS.ppt *)

  let meet_raw uf1 uf2 =
    let uf1 = normalize uf1 in
    let uf2 = normalize uf2 in
    let memo = ref TTMap.empty in
    let res = TMap.merge (fun v r1 r2 ->
        match r1,r2 with
        | Some r1, Some r2 ->
          (*Format.printf "visiting: %a (%a,%a)@." T.pp v T.pp r1 T.pp r2;*)
          if T.compare r1 r2 = 0 then 
            Some r1
          else begin
            try 
              let res = TTMap.find (r1,r2) !memo in
              (*Format.printf "found memo: %a@." T.pp res;*)
              Some res
            with Not_found ->
              memo := TTMap.add (r1,r2) v !memo;
              Some v
          end
        | Some _r1, None ->
          Some v
        | None, Some _r2 ->
          Some v
        | _ -> assert false
      ) uf1 uf2 in
    (uf1,uf2,res)

  let eqs uf =
    TMap.bindings (normalize uf)

  type remove_result =
    | Representative of elt
    | NoRepresentative
    | RepresentativeSame

  let remove a uf =
    let uf = normalize uf in
    try
      let v = TMap.find a uf in
      if T.compare v a = 0 then
        (* removing representative *)
        let (uf,found) = TMap.fold (fun k v (uf,found) ->
            match found with
            | Some rep when T.compare v a = 0 ->
              let uf = TMap.add k rep uf in
              (uf, found)
            | None when T.compare v a = 0 && T.compare k a <> 0 ->
                let uf = TMap.add k k uf in
                (uf, Some k)
            | _ ->
                (uf,found)
          ) uf (uf,None) in
        let uf = TMap.remove a uf in
        begin match found with
          | None ->
            (uf,NoRepresentative)
          | Some v ->
            (uf,Representative v)
        end
      else
        (* removing non-representative *)
        (TMap.remove a uf,RepresentativeSame)

    with Not_found ->
      (uf,RepresentativeSame)
end
