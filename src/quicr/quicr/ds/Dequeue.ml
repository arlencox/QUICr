type 'a t =
  {
    fwd : 'a list;
    bkw : 'a list;
  }

let empty =
  {
    fwd = [];
    bkw = [];
  }

let singleton e =
  { fwd = [e]; bkw = [] }

let of_list l =
  { fwd = l; bkw = [] }

let push_back e t =
  { t with
    bkw = e::t.bkw }

let push_front e t =
  { t with
    fwd = e::t.fwd }

let is_empty = function
  | {fwd = []; bkw = []} -> true
  | _ -> false

let rec pop_front = function
  | {fwd = h::tl; bkw} -> ({fwd=tl; bkw}, h)
  | {fwd = []; bkw = []} -> raise Not_found
  | {fwd = []; bkw} -> pop_front {fwd=List.rev bkw; bkw=[]}

let rec pop_back = function
  | {bkw = h::tl; fwd} -> ({bkw=tl; fwd}, h)
  | {bkw = []; fwd = []} -> raise Not_found
  | {bkw = []; fwd} -> pop_back {bkw=List.rev fwd; fwd=[]}

let iter f t =
  List.iter f t.fwd;
  List.iter f (List.rev t.bkw)

let riter f t =
  List.iter f t.bkw;
  List.iter f (List.rev t.fwd)

let fold f t r =
  let r = ref r in
  iter (fun e -> r := f e !r) t;
  !r

let rfold f t r =
  let r = ref r in
  riter (fun e -> r := f e !r) t;
  !r

let append t1 t2 =
  let t1 = ref t1 in
  try
    let rec app t2 =
      let (t2,v) = pop_front t2 in
      t1 := push_back v !t1;
      app t2
    in
    app t2
  with Not_found ->
    !t1

