type 'sym t = {
  iter : (('sym -> 'sym -> unit) -> unit);
  mem : ('sym -> bool);
  get : ('sym -> 'sym);
}

module D = DS.Dequeue

type 'sym c = 'sym t DS.Dequeue.t

let singleton a b =
  {
    iter = (fun f -> f a b);
    mem = (fun s -> s = a);
    get = (fun s -> if s = a then b else s);
  }

let of_assoc_list l =
  let h = Hashtbl.create (2*(List.length l)) in
  List.iter (fun (a,b) ->
      Hashtbl.replace h a b
    ) l;
  {
    iter = (fun f -> List.iter (fun (a,b) -> f a b) l);
    mem = (fun s -> Hashtbl.mem h s);
    get = (fun s -> try Hashtbl.find h s with Not_found -> s);
  }

let of_iter_mem_get iter mem get =
  {iter; mem; get}

let of_map m =
  failwith "Rename.of_map unimplemented"

let fold f t r =
  let r = ref r in
  t.iter (fun a b -> r := f a b !r);
  !r

let iter f t =
  t.iter f

let mem t s =
  t.mem s

let get t s =
  t.get s

let to_assoc_list t =
  fold (fun a b l -> (a,b)::l) t []

let empty = D.empty

let is_empty c =
  D.is_empty c

let prepend t c =
  D.push_front t c

let append c t =
  D.push_back t c

let compose c1 c2 =
  D.append c1 c2

let of_composition c =
  let h = Hashtbl.create 1023 in
  D.riter (fun t ->
      t.iter (fun a b ->
          let b = try
              Hashtbl.find h b
            with Not_found -> b in
          Hashtbl.replace h a b
        )
    ) c;
  {
    iter=(fun f -> Hashtbl.iter f h);
    mem=Hashtbl.mem h;
    get=(fun s -> try Hashtbl.find h s with Not_found -> s);
  }

