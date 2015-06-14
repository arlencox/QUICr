include List

let rec map_filter f = function
  | [] -> []
  | h::t ->
    begin match f h with
      | None -> map_filter f t
      | Some h -> h::(map_filter f t)
    end

let rec rev_map_filter acc f = function
  | [] -> acc
  | h::t ->
    begin match f h with
      | None -> rev_map_filter acc f t
      | Some h -> rev_map_filter (h::acc) f t
    end

let rev_map_filter f l =
  rev_map_filter [] f l

let rec fmt fmt_sep fmt_el ff = function
  | [] -> ()
  | [h] ->
    fmt_el ff h
  | h::t ->
    fmt_el ff h;
    fmt_sep ff;
    fmt fmt_sep fmt_el ff t

let fmt_str_sep sep =
  fmt (fun ff -> Format.fprintf ff "%s" sep)
  
let reduce_left1 f l =
  let h = List.hd l in
  let t = List.tl l in
  fold_left f h t
  

let rec iteri_int count f = function
  | [] -> ()
  | h::t ->
    f count h;
    iteri_int (count+1) f t

let iteri f l =
  iteri_int 0 f l

let fold_lefti f r l =
  let r = ref r in
  iteri (fun i el ->
    r := f i !r el
  ) l;
  !r
    
  
let rec find_first f l =
  match l with
  | [] -> None
  | h::t ->
    begin match f h with
    | Some el -> Some el
    | None -> find_first f t
    end

let rec fold_seq f starti endi result =
  if starti = endi then
    result
  else
    let result' = f starti result in
    if starti < endi then
      fold_seq f (starti+1) endi result'
    else
      fold_seq f (starti-1) endi result'
  

let rec uniq compare prev l =
  match l with
    | [] ->
        []
    | h::t ->
        let t = uniq compare h t in
        if (compare prev h) = 0 then
          t
        else
          h::t

let uniq compare l =
  match l with
    | [] -> []
    | h::t ->
        h::(uniq compare h t)

let rec compare cmp_el l1 l2 =
  match l1,l2 with
    | [],[] -> 0
    | l1,[] -> -1
    | [],l2 -> 1
    | hd1::tl1, hd2::tl2 ->
        let res = cmp_el hd1 hd2 in
        if res = 0 then
          compare cmp_el tl1 tl2
        else
          res

let rec fold_left_pairs f r l =
  match l with
    | [] -> r
    | h::tl ->
        let r = List.fold_left (fun r h2 ->
          f r h h2
        ) r tl in
        fold_left_pairs f r tl
