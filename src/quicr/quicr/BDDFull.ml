module L = LogicSymbolicSet

module ISet = Set.Make(struct
    type t = int
    let compare = (-)
  end)

module B = MLBDD.Raw

type ctx = B.man

type t = B.t

type sym = int

type cnstr = sym L.t

type output = cnstr

type query = sym L.q 

let init () = B.init ()

let top ctx = B.dtrue

let bottom ctx = B.dfalse

let symbols ctx t = B.support ctx t |> B.list_of_support

let rec of_expr ctx = function
  | L.Empty ->
    (B.dfalse, B.dtrue)
  | L.Universe ->
    (B.dtrue, B.dtrue)
  | L.DisjUnion (a,b) -> 
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (* A ^ B = 0
       A /\ B = false
       ~(A /\ B) *)
    (B.dor ctx a b, (B.dand ctx (B.dand ctx ac bc) (B.dnot (B.dand ctx a b))))
  | L.Union (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (B.dor ctx a b, B.dand ctx ac bc)
  | L.Inter (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (B.dand ctx a b, B.dand ctx ac bc)
  | L.Diff (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (B.dand ctx a (B.dnot b), B.dand ctx ac bc)
  | L.Comp a ->
    let (a,ac) = of_expr ctx a in
    (B.dnot a, ac)
  | L.Var v ->
    (B.ithvar ctx v, B.dtrue)
  | L.Sing v ->
    (B.ithvar ctx v, B.dtrue)

exception Unsupported

let rec of_cnstr is_pos is_over ctx c =
  let r is_inv = of_cnstr is_inv is_over ctx in
  match c, is_pos with
  | L.Eq (a,b), true ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    B.dand ctx (B.eq ctx a b) (B.dand ctx ac bc)
  | L.Eq _, false ->
    if is_over then
      B.dtrue
    else
      raise Unsupported
  | L.SubEq (a,b), true ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    B.dand ctx (B.imply ctx a b) (B.dand ctx ac bc)
  | L.SubEq _, false ->
    if is_over then
      B.dtrue
    else
      raise Unsupported
  | L.In (av,b), _ ->
    if is_over then
      B.dtrue
    else
      raise Unsupported
  | L.And (a,b), true ->
    let a = r is_pos a in
    let b = r is_pos b in
    B.dand ctx a b

  | L.And (a,b), false ->
    let a = r is_pos a in
    let b = r is_pos b in
    if is_over then
      B.dor ctx a b
    else 
      raise Unsupported
  | L.Not a, _ ->
    r (not is_pos) a
  | L.True, false
  | L.False, true ->
    B.dfalse
  | L.False, false
  | L.True, true ->
    B.dtrue

let constrain ctx cnstr t =
  let c = of_cnstr true true ctx cnstr in
  B.dand ctx c t


let rec bin_of_list emp op acc = function
  | h1::h2::rest -> bin_of_list emp op ((op h1 h2)::acc) rest
  | [h] -> bin_of_list emp op (h::acc) []
  | [] -> match acc with
    | [] -> emp
    | [h] -> h
    | rest -> bin_of_list emp op [] rest 

let bin_of_list emp op l = bin_of_list emp op [] l

let serialize ctx t =
  B.allprime ctx (B.dnot t) |>
  List.map (fun el ->
     let (l,r) =
       el |>
       List.map (fun (b,v) -> (b, L.Var v)) |>
       List.partition fst in
      let l = List.map snd l in
      let l = bin_of_list L.Universe (fun a b -> L.Inter (a,b)) l in
      let r = List.map snd r in
      let r = bin_of_list L.Empty (fun a b -> L.Union (a,b)) r in
      L.SubEq (l,r)
    ) |>
  bin_of_list L.True (fun a b -> L.And (a,b))

let sat ctx t cnstr =
  try
    let c = of_cnstr true false ctx cnstr in
    B.is_true (B.imply ctx t c)
  with Unsupported ->
    false


let join ctx a b = B.dor ctx a b

let widening = join

let meet ctx a b = B.dand ctx a b

let le ctx a b = B.is_true (B.imply ctx a b)

let forget ctx syms t =
  B.exists ctx syms t


let is_bottom ctx t = B.is_false t

let is_top ctx t = B.is_true t



let rename_symbols ctx rename t =
  B.permutef ctx (Rename.get rename) t


module SymSymSet = Set.Make(struct
    type t = int * int
    let compare (a1,b1) (a2,b2) =
      let res = a1 - a2 in
      if res <> 0 then res
      else b1 - b2
  end)

module SymSet = Set.Make(struct
    type t = int
    let compare = (-)
  end)

let query ctx t =
  let rmap = ref None in
  let get_subset_map () =
    let map = B.allprime ctx (B.dnot t) |>
              List.filter (function
                  | [(true, _); (false, _)]
                  | [(false, _); (true, _)] -> true
                  | _ -> false) |>
              List.fold_left (fun map l ->
                  match l with
                  | [(true, v2); (false, v1)]
                  | [(false, v1); (true, v2)] ->
                    SymSymSet.add (v1,v2) map
                  | _ ->
                    assert false
                ) SymSymSet.empty in
    rmap := Some map;
    map
  in
  let get_subset_map () =
    match !rmap with
    | None -> get_subset_map ()
    | Some m -> m
  in
  let get_eqs () =
    let map = get_subset_map () in
    SymSymSet.fold (fun (s,d) eqs ->
        if SymSymSet.mem (d,s) map then
          (s,d)::eqs
        else
          eqs
      ) map []
  in
  let get_eqs_sym sym =
    let map = get_subset_map () in
    SymSymSet.fold (fun (s,d) eqs ->
        if SymSymSet.mem (d,s) map then
          if s = sym then
            SymSet.add d eqs
          else if d = sym then
            SymSet.add s eqs
          else
            eqs
        else
          eqs
      ) map SymSet.empty |>
    SymSet.elements
  in
  {
    L.get_eqs = get_eqs;
    L.get_eqs_sym = get_eqs_sym;
  }

let combine ctx q t =
  List.fold_left (fun t (s1, s2) ->
        constrain ctx (L.Eq (L.Var s1, L.Var s2)) t
    ) t (q.L.get_eqs ())

let pp_print ctx pp_sym ff t =
  let s = serialize ctx t in
  LogicSymbolicSet.pp pp_sym ff s

let pp_debug = pp_print
