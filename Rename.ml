type 'sym t = {
  iter : (('sym -> 'sym -> unit) -> unit);
  mem : ('sym -> bool);
  get : ('sym -> 'sym);
}

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


let fold f t r =
  let r = ref r in
  t.iter (fun a b -> r := f a b !r);
  !r

let mem s t =
  t.mem s

let get s t =
  t.get s

let to_assoc_list t =
  fold (fun a b l -> (a,b)::l) t []
