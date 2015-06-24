module type Config = sig
  val reorder : string option
end

module Make(C: Config) = struct
  module L = LogicSymbolicSet

  module ISet = Set.Make(struct
      type t = int
      let compare = (-)
    end)

  module B = Cudd.Bdd

  type ctx = Cudd.Man.d Cudd.Man.t

  type t = Cudd.Man.d B.t

  type sym = int

  type cnstr = sym L.t

  type output = cnstr

  type query = sym L.q 

  let init () =
    let man = Cudd.Man.make_d () in
    let reorder = match C.reorder with
      | Some s -> Some (String.lowercase s)
      | None -> None
    in
    let reorder = match reorder with
      | Some "same" -> Some Cudd.Man.REORDER_SAME
      | Some "none" -> Some Cudd.Man.REORDER_NONE
      | Some "random" -> Some Cudd.Man.REORDER_RANDOM
      | Some "random_pivot" -> Some Cudd.Man.REORDER_RANDOM_PIVOT
      | Some "sift" -> Some Cudd.Man.REORDER_SIFT
      | Some "sift_converge" -> Some Cudd.Man.REORDER_SIFT_CONVERGE
      | Some "symm_sift" -> Some Cudd.Man.REORDER_SYMM_SIFT
      | Some "symm_sift_conv" -> Some Cudd.Man.REORDER_SYMM_SIFT_CONV
      | Some "window2" -> Some Cudd.Man.REORDER_WINDOW2
      | Some "window3" -> Some Cudd.Man.REORDER_WINDOW3
      | Some "window4" -> Some Cudd.Man.REORDER_WINDOW4
      | Some "window2_conv" -> Some Cudd.Man.REORDER_WINDOW2_CONV
      | Some "window3_conv" -> Some Cudd.Man.REORDER_WINDOW3_CONV
      | Some "window4_conv" -> Some Cudd.Man.REORDER_WINDOW4_CONV
      | Some "group_sift" -> Some Cudd.Man.REORDER_GROUP_SIFT
      | Some "group_sift_conv" -> Some Cudd.Man.REORDER_GROUP_SIFT_CONV
      | Some "annealing" -> Some Cudd.Man.REORDER_ANNEALING
      | Some "genetic" -> Some Cudd.Man.REORDER_GENETIC
      | Some "linear" -> Some Cudd.Man.REORDER_LINEAR
      | Some "linear_converge" -> Some Cudd.Man.REORDER_LINEAR_CONVERGE
      | Some "lazy_sift" -> Some Cudd.Man.REORDER_LAZY_SIFT
      | Some "exact" -> Some Cudd.Man.REORDER_EXACT
      | Some _ -> None
      | None -> None
    in
    begin match reorder with
    | Some reorder -> Cudd.Man.enable_autodyn man reorder
    | None -> ()
    end;
    man

  let top ctx = B.dtrue ctx

  let bottom ctx = B.dfalse ctx

  let symbols ctx t = B.support t |> B.list_of_support

  let rec of_expr ctx = function
    | L.Empty ->
      (B.dfalse ctx, B.dtrue ctx)
    | L.Universe ->
      (B.dtrue ctx, B.dtrue ctx)
    | L.DisjUnion (a,b) -> 
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (* A ^ B = 0
         A /\ B = false
         ~(A /\ B) *)
      (B.dor a b, (B.dand (B.dand ac bc) (B.dnot (B.dand a b))))
    | L.Union (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (B.dor a b, B.dand ac bc)
    | L.Inter (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (B.dand a b, B.dand ac bc)
    | L.Diff (a,b) ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      (B.dand a (B.dnot b), B.dand ac bc)
    | L.Comp a ->
      let (a,ac) = of_expr ctx a in
      (B.dnot a, ac)
    | L.Var v ->
      (B.ithvar ctx v, B.dtrue ctx)
    | L.Sing v ->
      (B.ithvar ctx v, B.dtrue ctx)

  exception Unsupported

  let rec of_cnstr is_pos is_over ctx c =
    let r is_inv = of_cnstr is_inv is_over ctx in
    match c, is_pos with
    | L.Eq (a,b), true ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      B.dand (B.eq a b) (B.dand ac bc)
    | L.Eq _, false ->
      if is_over then
        B.dtrue ctx
      else
        raise Unsupported
    | L.SubEq (a,b), true ->
      let (a,ac) = of_expr ctx a in
      let (b,bc) = of_expr ctx b in
      B.dand (B.dor (B.dnot a) b) (B.dand ac bc)
    | L.SubEq _, false ->
      if is_over then
        B.dtrue ctx
      else
        raise Unsupported
    | L.In (av,b), _ ->
      if is_over then
        B.dtrue ctx
      else
        raise Unsupported
    | L.And (a,b), true ->
      let a = r is_pos a in
      let b = r is_pos b in
      B.dand a b

    | L.And (a,b), false ->
      let a = r is_pos a in
      let b = r is_pos b in
      if is_over then
        B.dor a b
      else 
        raise Unsupported
    | L.Not a, _ ->
      r (not is_pos) a
    | L.True, false
    | L.False, true ->
      B.dfalse ctx
    | L.False, false
    | L.True, true ->
      B.dtrue ctx

  let constrain ctx cnstr t =
    let c = of_cnstr true true ctx cnstr in
    B.dand c t


  let rec bin_of_list emp op acc = function
    | h1::h2::rest -> bin_of_list emp op ((op h1 h2)::acc) rest
    | [h] -> bin_of_list emp op (h::acc) []
    | [] -> match acc with
      | [] -> emp
      | [h] -> h
      | rest -> bin_of_list emp op [] rest 

  let bin_of_list emp op l = bin_of_list emp op [] l

  let serialize ctx t =
    failwith "unimplemented"

  let sat ctx t cnstr =
    try
      let c = of_cnstr true false ctx cnstr in
      B.is_true (B.dor (B.dnot t) c)
    with Unsupported ->
      false


  let join ctx a b = B.dor a b

  let widening = join

  let meet ctx a b = B.dand a b

      (*let le ctx a b = B.is_true (B.dor (B.dnot a) b)*)
  let le ctx a b = B.is_leq a b

  let forget ctx syms t =
    let supp = List.fold_left (fun supp el -> B.dand supp (B.ithvar ctx el)) (B.dtrue ctx) syms in
    B.exist supp t

  let is_bottom ctx t = B.is_false t

  let is_top ctx t = B.is_true t



  let rename_symbols ctx rename t =
    let max_sym = Cudd.Man.get_bddvar_nb ctx in
    let perm = Array.init (max_sym + 1) (fun i -> B.ithvar ctx (Rename.get rename i)) in
    B.vectorcompose perm t

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
    let get_eqs () = [] in
    let get_eqs_sym sym = [] in
    {
      L.get_eqs = get_eqs;
      L.get_eqs_sym = get_eqs_sym;
    }

  let combine ctx q t =
    List.fold_left (fun t (s1, s2) ->
        constrain ctx (L.Eq (L.Var s1, L.Var s2)) t
      ) t (q.L.get_eqs ())

  let pp_print ctx pp_sym ff t =
    B.print pp_sym ff t

  let pp_debug = pp_print
end
