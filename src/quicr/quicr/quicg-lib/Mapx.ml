module type S = sig
  include Map.S

  val forall2 :
    (key -> 'a -> 'b -> bool) ->
    'a t ->
    'b t ->
    bool
      
  val exists2 :
    (key -> 'a -> 'b -> bool) ->
    'a t ->
    'b t ->
    bool
      
  val fmt :
    ?sep:(Format.formatter -> unit) ->
    ?kvsep:(Format.formatter -> unit) ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit

  val foldi :
    (int -> key -> 'a -> 'b -> 'b) ->
    'a t ->
    'b ->
    'b

  val fold2 :
    (key -> 'a option -> 'b option -> 'c -> 'c) ->
    'a t ->
    'b t ->
    'c -> 'c

  val of_assn_list :
    (key * 'a) list ->
    'a t

  val to_assn_list :
    'a t -> (key * 'a) list
end

module Make(Ord: Map.OrderedType) = struct
  include Map.Make(Ord)

  exception Short_circuit

  let forall2 f m1 m2 =
    try
      ignore (merge (fun key v1 v2 ->
        match (v1,v2) with
          | (Some v1, Some v2) ->
              if f key v1 v2 then
                None
              else
                raise Short_circuit
          | (Some _, None) ->
              raise (Invalid_argument "Key missing from second map")
          | (None, Some _) ->
              raise (Invalid_argument "Key missing from first map")
          | _ -> assert false
      ) m1 m2);
      true
    with Short_circuit ->
      false

  let exists2 f m1 m2 =
    try
      ignore (merge (fun key v1 v2 ->
        match (v1,v2) with
          | (Some v1, Some v2) ->
              if f key v1 v2 then
                raise Short_circuit
              else
                None
          | (Some _, None) ->
              raise (Invalid_argument "Key missing from second map")
          | (None, Some _) ->
              raise (Invalid_argument "Key missing from first map")
          | _ -> assert false
      ) m1 m2);
      false
    with Short_circuit ->
      true

  let default_sep ff =
    Format.fprintf ff ", "

  let default_kvsep ff =
    Format.fprintf ff ": "

  let fmt ?sep:(sep=default_sep) ?kvsep:(kvsep=default_kvsep) fmt_key fmt_val ff map =
    let first = ref true in
    iter (fun k v ->
      if !first then
        first := false
      else
        sep ff;
      fmt_key ff k;
      kvsep ff;
      fmt_val ff v
    ) map

  let foldi f m s =
    snd (fold (fun k v (i,s) -> (i+1,f i k v s)) m (0,s))

  let fold2 f a b r =
    let r = ref r in
    ignore(merge (fun key a b ->
      r := f key a b !r;
      None
    ) a b);
    !r
      
  let of_assn_list l =
    List.fold_left (fun m (k,v) ->
      add k v m
    ) empty l

  let to_assn_list m =
    List.rev (fold (fun k v l ->
      (k,v)::l
    ) m [])
      
      
end
