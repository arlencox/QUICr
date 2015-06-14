module type S = sig
  include Set.S
  
  val of_list: elt list -> t

  val map: (elt -> elt) -> t -> t

  val fmt:
    ?sep:(Format.formatter -> unit) ->
    (Format.formatter -> elt -> unit) ->
    Format.formatter ->
    t ->
    unit

  val iter_subsets: (t -> unit) -> t -> unit
  val fold_subsets: (t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make(Ord: Set.OrderedType) = struct
  include Set.Make(Ord)
  
  let rec of_list acc = function
    | [] -> acc
    | h:: tl -> of_list (add h acc) tl
  
  let of_list l =
    of_list empty l

  let default_sep ff =
    Format.fprintf ff ", "

  let fmt ?sep:(sep=default_sep) fmt_el ff set =
    let first = ref true in
    iter (fun el ->
      if !first then
        first := false
      else
        sep ff;
      fmt_el ff el 
    ) set

  let map f s =
    fold (fun el rest ->
      add (f el) rest
    ) s empty 

  let iter_subsets f s =
    f empty;
    ignore(fold (fun x rest ->
      let new_els = List.rev_map (fun ys ->
        let res = add x ys in
        f res;
        res
      ) rest in
      List.rev_append rest new_els
    ) s [empty])

  let fold_subsets f s a =
    let a = ref a in
    iter_subsets (fun s ->
      a := f s !a
    ) s;
    !a
    
end
