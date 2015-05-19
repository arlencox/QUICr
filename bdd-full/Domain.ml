module L = LogicSymbolicSet

module ISet = Set.Make(struct
    type t = int
    let compare = (-)
  end)

type ctx = MLBDD.man

type t = MLBDD.t

type sym = int

type cnstr = sym L.t

type output = cnstr

type query = sym L.q 

let init () = MLBDD.init ()

let top ctx = MLBDD.dtrue ctx

let bottom ctx = MLBDD.dfalse ctx

let context t = MLBDD.manager t

let symbols t = MLBDD.support t |> MLBDD.list_of_support

let rec of_expr ctx = function
  | L.Empty ->
    (MLBDD.dfalse ctx, MLBDD.dtrue ctx)
  | L.Universe ->
    (MLBDD.dtrue ctx, MLBDD.dtrue ctx)
  | L.DisjUnion (a,b) -> 
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (* A ^ B = 0
       A /\ B = false
       ~(A /\ B) *)
    (MLBDD.dor a b, (MLBDD.dand (MLBDD.dand ac bc) (MLBDD.dnot (MLBDD.dand a b))))
  | L.Union (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (MLBDD.dor a b, MLBDD.dand ac bc)
  | L.Inter (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (MLBDD.dand a b, MLBDD.dand ac bc)
  | L.Diff (a,b) ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    (MLBDD.dand a (MLBDD.dnot b), MLBDD.dand ac bc)
  | L.Comp a ->
    let (a,ac) = of_expr ctx a in
    (MLBDD.dnot a, ac)
  | L.Var v ->
    (MLBDD.ithvar ctx v, MLBDD.dtrue ctx)
  | L.Sing v ->
    (MLBDD.ithvar ctx v, MLBDD.dtrue ctx)

exception Unsupported

let rec of_cnstr is_pos is_over ctx c =
  let r is_inv = of_cnstr is_inv is_over ctx in
  match c, is_pos with
  | L.Eq (a,b), true ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    MLBDD.dand (MLBDD.eq a b) (MLBDD.dand ac bc)
  | L.Eq _, false ->
    if is_over then
      MLBDD.dtrue ctx
    else
      raise Unsupported
  | L.SubEq (a,b), true ->
    let (a,ac) = of_expr ctx a in
    let (b,bc) = of_expr ctx b in
    MLBDD.dand (MLBDD.imply a b) (MLBDD.dand ac bc)
  | L.SubEq _, false ->
    if is_over then
      MLBDD.dtrue ctx
    else
      raise Unsupported
  | L.In (av,b), _ ->
    if is_over then
      MLBDD.dtrue ctx
    else
      raise Unsupported
  | L.And (a,b), true ->
    let a = r is_pos a in
    let b = r is_pos b in
    MLBDD.dand a b

  | L.And (a,b), false ->
    let a = r is_pos a in
    let b = r is_pos b in
    if is_over then
      MLBDD.dor a b
    else 
      raise Unsupported
  | L.Not a, _ ->
    r (not is_pos) a
  | L.True, false
  | L.False, true ->
    MLBDD.dfalse ctx
  | L.False, false
  | L.True, true ->
    MLBDD.dtrue ctx

let constrain cnstr t =
  let c = of_cnstr true true (context t) cnstr in
  MLBDD.dand c t


let rec bin_of_list emp op acc = function
  | h1::h2::rest -> bin_of_list emp op ((op h1 h2)::acc) rest
  | [h] -> bin_of_list emp op (h::acc) []
  | [] -> match acc with
    | [] -> emp
    | [h] -> h
    | rest -> bin_of_list emp op [] rest 

let bin_of_list emp op l = bin_of_list emp op [] l

let serialize t =
  MLBDD.allprime (MLBDD.dnot t) |>
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

let sat t cnstr =
  try
    let c = of_cnstr true false (context t) cnstr in
    MLBDD.is_true (MLBDD.imply t c)
  with Unsupported ->
    false


let join a b = MLBDD.dor a b

let widening = join

let meet a b = MLBDD.dand a b

let le a b = MLBDD.is_true (MLBDD.imply a b)

let forget syms t =
  let ctx = context t in
  let supp = syms |>
             List.map (MLBDD.ithvar ctx) |>
             List.fold_left MLBDD.dand (MLBDD.dtrue (context t)) |>
             MLBDD.support in
  MLBDD.exists supp t


let is_bottom t = MLBDD.is_false t



let rename_symbols rename t =
  MLBDD.permutef (Rename.get rename) t


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

let query t =
  let rmap = ref None in
  let get_subset_map () =
    let map = MLBDD.allprime (MLBDD.dnot t) |>
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

let combine q t =
  List.fold_left (fun t (s1, s2) ->
        constrain (L.Eq (L.Var s1, L.Var s2)) t
    ) t (q.L.get_eqs ())

let pp_print pp_sym ff t =
  let s = serialize t in
  LogicSymbolicSet.pp pp_sym ff s

let pp_debug = pp_print
