let en_reductions = ref true
let en_relations = ref true
let args = [
  ("-disable-reductions", Arg.Clear en_reductions, "Disable reductions with base domain");
  ("-disable-relations", Arg.Clear en_relations, "Disable relations between set variables")
]

module Make (ND: QGInterface.ScalarDomain) : QGInterface.SetDomain =
struct

  module G = SetConstraintGraph.Make(ND)

  type ctx = G.ctx

  type t = G.t * ND.t


  let args = G.args 

  let init = ND.init

  let top ctx ndim _sdim =
    (G.empty ctx (BaseSym.of_int ndim), ND.top ctx ndim)

  let is_top ctx (s,ns) =
    (G.is_empty ctx s) && (ND.is_top ctx ns)

  let fmt ?escape:(escape=FormatHelp.Text) ctx fmt_scalar_var fmt_set_var ff (ss,sn) =
    let prints = not(G.is_empty ctx ss) in
    let printn = not(ND.is_top ctx sn) in
    if prints then
      G.pp_print ~escape:escape ctx fmt_scalar_var fmt_set_var ff ss;
    if prints && printn then
      Format.fprintf ff " %a%a" (FormatHelp.fmt_conj escape) () (G.pp_conj_gap escape) ();
    if printn || ((not prints) && (not printn)) then
      ND.fmt ~escape:escape ctx fmt_scalar_var ff sn

  let fmt_debug ctx ff st =
    G.pp_print ~escape:FormatHelp.Text ctx
      (fun ff b -> Format.fprintf ff "b%d" b)
      (fun ff s -> Format.fprintf ff "s%d" s)
      ff st

  let fmt_vtx_list sep ff vl =
    Listx.fmt_str_sep sep G.V.pp_print ff vl

  let fmt_edge ff (srcs,dsts) =
    Format.fprintf ff "%a <= %a"
      (fmt_vtx_list " ^ ") srcs
      (fmt_vtx_list " U ") dsts

  let fmt_base ctx ff nd =
    ND.fmt ctx (fun ff b -> Format.fprintf ff "b%d" b) ff nd
      


  let is_bottom ctx (s,ns) =
    false

  (*let do_reduction_cnstr ctx pos t tn cnstr =
    let t' = G.copy_atoms ctx t in
    let t' = G.fold_constraints ctx (fun srcs dsts lbl t' ->
      let lbl' = ND.constrain ctx lbl cnstr in
      G.add_constraint ctx t' srcs dsts lbl'
    ) t t' in
    (t', ND.constrain ctx tn cnstr)*)

  let do_reduction_cnstr ctx pos t tn cnstr =
    let t' = G.map_edges ctx (fun lbl -> ND.constrain ctx lbl cnstr) t in
    (t', ND.constrain ctx tn cnstr)

  (*let do_reduction_trans ctx pos t tn cmd =
    let t' = G.copy_atoms ctx t in
    let t' = G.fold_constraints ctx (fun srcs dsts lbl t' ->
      let lbl' = ND.transition ctx lbl cmd in
      G.add_constraint ctx t' srcs dsts lbl'
    ) t t' in
    (t', ND.transition ctx tn cmd)*)
  let do_reduction_trans ctx pos t tn cmd =
    let t' = G.map_edges ctx (fun lbl -> ND.transition ctx lbl cmd) t in
    (t', ND.transition ctx tn cmd)

  let set_to_base_reduction ctx t tn =
    (*Format.printf "before_simp: %a@." (fmt_debug ctx) t;*)
    let t = G.simplify ctx t in
    (*Format.printf "after_simp: %a@." (fmt_debug ctx) t;*)
    let c = G.fold_atoms ctx (fun v c ->
      match v with
        | G.Singleton _ ->
            let (_t, res) = G.get_constraint ctx t [v] [v] in
            begin match res with
              | None ->
                  c
              | Some cnstr ->
                  let cnstr = ND.rem_dim ctx cnstr in
                  ND.meet ctx c cnstr
            end
        | _ ->
            c
    ) t (ND.rem_dim ctx (G.base_top ctx t)) in
    let tn = ND.meet ctx tn c in
    let c_ext = ND.add_dim ctx c in
    let t' = G.map_edges ctx (fun lbl -> ND.meet ctx lbl c_ext) t in
    (t',tn)

  let base_to_set_reduction ctx t tn =
    let eqs = ND.get_eq ctx tn in
    let eqs = Listx.rev_map_filter (fun lst ->
      let lst = List.filter (fun v -> G.mem_atom ctx t (G.Singleton (BaseSym.of_int v))) lst in
      if lst = [] then
        None
      else
        Some lst
    ) eqs in
    let t = List.fold_left (fun t eq ->
      let refv = BaseSym.of_int (List.hd eq) in
      let (t,cnstr) = G.get_constraint ctx t [G.Singleton refv] [G.Singleton refv] in
      let refv_lbl = match cnstr with
        | None -> G.base_top ctx t
        | Some l -> l
      in
      let rest = List.tl eq in
      List.fold_left (fun t v ->
        let v = BaseSym.of_int v in
        let (t,cnstr) = G.get_constraint ctx t [G.Singleton v] [G.Singleton v] in
        let v_lbl = match cnstr with
          | None -> G.base_top ctx t
          | Some l -> l
        in
        let lbl = ND.meet ctx refv_lbl v_lbl in
        let t = G.add_constraint ctx t [G.Singleton v] [G.Singleton refv] lbl in
        let t = G.add_constraint ctx t [G.Singleton refv] [G.Singleton v] lbl in
        t
      ) t rest
    ) t eqs in
    (t, tn)

  let reductions ctx t tn =
    let (t,tn) = base_to_set_reduction ctx t tn in
    set_to_base_reduction ctx t tn
    

  let post_process ctx t tn =
    let (t,tn) = if not(!en_relations) then
        (G.delete_relations ctx t, tn)
      else
        (t,tn)
    in
    let (t,tn) = if not(!en_reductions) then
        (G.delete_reductions ctx t, tn)
      else
        (t,tn)

    in
    (t,tn)
    
        

  exception LE_Short of G.t

      
  let memoizing_le ctx a b =
    try
      (*Format.printf "a: %a@." (fmt_debug ctx) a;
      Format.printf "b: %a@." (fmt_debug ctx) b;*)
      let a = G.fold_atoms ctx (fun atoma a ->
        Timeout.check_timeout ();
        (* check if atom from a is in b *)
        if G.mem_atom ctx b atoma then
          (* if so, leave it *)
          a
        else
          (* otherwise, get rid of it *)
          G.project_atom ctx a atoma
      ) a a in
      let a = G.simplify ctx a in
      Timeout.check_timeout ();
      let a = G.fold_constraints ctx (fun srcb dstb lblb a ->
        (* check if edge from b is in a *)
        let (a, result) = G.get_constraint ctx a srcb dstb in
        match result with
          | None ->
              raise (LE_Short a)
          | Some lbla ->
              (* TODO: handle labels correctly *)
              a
      ) b a in
      (a, true)
    with LE_Short a ->
      (a, false)

  let le ctx (a,na) (b,nb) =
    let (a,na) = reductions ctx a na in
    let (b,nb) = reductions ctx b nb in
    let (_sa,res) = memoizing_le ctx a b in
    let res = res && (ND.le ctx na nb) in
    (*if res then
      Format.printf "LE@."
    else
      Format.printf "Not LE@.";*)
    res

  let narrow _ctx _a b = b

  module CSet = Setx.Make(struct
    type t = G.vtx_t list * G.vtx_t list

    let compare (a1,a2) (b1,b2) =
      let res = Listx.compare G.V.compare a1 b1 in
      if res = 0 then
        Listx.compare G.V.compare a2 b2
      else
        res
  end)


  let generic_join ctx base_join a b =
    (*Format.printf "a: %a@." (fmt_debug ctx) a;
    Format.printf "b: %a@." (fmt_debug ctx) b;*)
    Timeout.check_timeout ();
    let a = G.simplify ctx a in
    Timeout.check_timeout ();
    let b = G.simplify ctx b in
    Timeout.check_timeout ();
    (*Format.printf "a: %a@." (fmt_debug ctx) a;
    Format.printf "b: %a@." (fmt_debug ctx) b;*)
    (* result is all common vertexes *)
    let a = G.fold_atoms ctx (fun atoma a ->
      (* check if atom from a is in b *)
      if G.mem_atom ctx b atoma then
        (* if so, leave it *)
        a
      else
        (* otherwise, get rid of it *)
        G.project_atom ctx a atoma
    ) a a in
    let b = G.fold_atoms ctx (fun atomb b ->
      (* check if atom from b is in a *)
      if G.mem_atom ctx a atomb then
        (* if so, leave it *)
        b
      else
        (* otherwise, get rid of it *)
        G.project_atom ctx b atomb
    ) b b in
    Timeout.check_timeout ();
    (*Format.printf "a: %a@." (fmt_debug ctx) a;
    Format.printf "b: %a@." (fmt_debug ctx) b;*)
    (* generate candidate edges *)
    let candidates = CSet.empty in
    let candidates = G.fold_constraints ctx (fun src dst _lbl candidates ->
      CSet.add (src,dst) candidates
    ) a candidates in
    let candidates = G.fold_constraints ctx (fun src dst _lbl candidates ->
      CSet.add (src,dst) candidates
    ) b candidates in
    let result = G.copy_atoms ctx a in

    (* iterate over candidate edges *)
    let (a,b,result) = CSet.fold (fun (srcs,dsts) (a,b,result) ->
      Timeout.check_timeout ();
      (*Format.printf "Checking edge: %a@." fmt_edge (srcs,dsts);*)
      (*if srcs = [G.Variable (SetSym.of_int (-1))] && dsts = [G.Variable (SetSym.of_int 1)] then begin
        Format.printf "a: %a@." (fmt_debug ctx) a;
        Format.printf "b: %a@." (fmt_debug ctx) b;
      end;*)
      let (a,ac) = G.get_constraint ctx a srcs dsts in
      let result = match ac with
        | None ->
            (a,b,result)
        | Some albl ->
            let (b,bc) = G.get_constraint ctx b srcs dsts in
            begin match bc with
              | None ->
                  (a,b,result)
              | Some blbl ->
                  let lbl' = base_join ctx albl blbl in
                  (*Format.printf "  adding: %a@." (fmt_base ctx) lbl';*)
                  let result = G.add_constraint ctx result srcs dsts lbl' in
                  (a,b,result)
            end
      in
      (*if srcs = [G.Variable (SetSym.of_int (-1))] && dsts = [G.Variable (SetSym.of_int 1)] then begin
        Format.printf "Done checking edge: %a@." fmt_edge (srcs,dsts);
        Format.printf "a: %a@." (fmt_debug ctx) a;
        Format.printf "b: %a@." (fmt_debug ctx) b;
      end;*)
      result
    ) candidates (a,b,result) in
    assert(snd (memoizing_le ctx a result));
    assert(snd (memoizing_le ctx b result));
    result

  let join ctx (a,na) (b,nb) =
    let (a,na) = reductions ctx a na in
    let (b,nb) = reductions ctx b nb in
    (*Format.printf "Dimension a: %d@." (ND.get_dim ctx na);
    Format.printf "Dimension b: %d@." (ND.get_dim ctx nb);*)
    (generic_join ctx ND.join a b, ND.join ctx na nb)

  let widen ctx (a,na) (b,nb) =
    let (a,na) = reductions ctx a na in
    let (b,nb) = reductions ctx b nb in
    (generic_join ctx ND.widen a b, ND.widen ctx na nb)

  let meet ctx (a,na) (b,nb) =
    let a = G.fold_atoms ctx (fun atom a ->
      if G.mem_atom ctx a atom then
        a
      else
        G.add_atom ctx a atom
    ) b a in
    let result = G.fold_constraints ctx (fun src dst lbl a ->
      G.add_constraint ctx a src dst lbl
    ) b a in
    (result, ND.meet ctx na nb)

      
  let constrain_subseteq ctx s sn s1 s2 =
    G.add_constraint ctx s [G.Variable s1] [G.Variable s2] sn

  let constrain_eq ctx s sn s1 s2 =
    let s = constrain_subseteq ctx s sn s1 s2 in
    constrain_subseteq ctx s sn s2 s1

  let constrain_base ctx s sn (guard, pos, args) =
    do_reduction_cnstr ctx pos s sn (guard, pos, args)
    

  let constrain ctx (s,sn) (guard, pos, oargs) =
    let sne = ND.add_dim ctx sn in
    let args = List.map SetSym.of_int oargs in
    match (guard, args) with
      | (Commands.SubSetEqGuard, [s1; s2]) ->
          (constrain_subseteq ctx s sne s1 s2, sn)
      | (Commands.SetEqGuard, [s1; s2]) ->
          (constrain_eq ctx s sne s1 s2, sn)

      | (Commands.LessThanGuard, _)
      | (Commands.LessEqGuard, _)
      | (Commands.EqualGuard, _)
      | (Commands.NotEqualGuard, _) ->
        constrain_base ctx s sn (guard, pos, oargs)

            
      | (Commands.SubSetEqGuard, _) ->
          assert false
      | (Commands.SetEqGuard, _) ->
          assert false

  let constrain_qc ctx (s,sn) a b =
    let a = List.map SetSym.of_int a in
    let b = List.map SetSym.of_int b in
    let a = List.map (fun v -> G.Variable v) a in
    let b = List.map (fun v -> G.Variable v) b in
    (G.add_constraint ctx s a b sn, sn)

  let rename_set ctx (s,sn) a b =
    (G.replace_atom ctx s (G.Variable (SetSym.of_int a)) (G.Variable (SetSym.of_int b)), sn)

  let forget_set ctx (s,sn) a =
    (G.project_atom ctx s (G.Variable (SetSym.of_int a)), sn)


  let fv_count = ref (SetSym.of_int (-1))

  let fresh_var t =
    let res = !fv_count in
    fv_count := SetSym.pred !fv_count;
    res

  let update_var ctx t v to_rename =
    if G.mem_atom ctx t (G.Variable v) then 
      let vnn = fresh_var t in
      let vn = G.Variable vnn in
      let t = G.replace_atom ctx t (G.Variable v) vn in
      (t,List.map (fun v_ren -> if v_ren = v then vnn else v_ren) to_rename)
    else
      (t,to_rename)



  let transition_sub_set ctx pos t tn result set value =
    (* post(r = s - v, t) = t[r'/r] /\ r <= s U v /\ s <= r U v /\ r ^ v <= {} *)
    let (t,renamed) = update_var ctx t result [set; value] in
    let (set,value) = match renamed with
      | [set; value] -> set, value
      | _ -> assert false
    in
    let t = G.add_variable ctx t result in
    let t = G.add_constraint ctx t [G.Variable result] [G.Variable set; G.Variable value] tn in
    let t = G.add_constraint ctx t [G.Variable set] [G.Variable result; G.Variable value] tn in
    let t = G.add_constraint ctx t [G.Variable result; G.Variable value] [G.Empty] tn in
    t

  let transition_union_set ctx t tn result args =
    (* post(r = a1 U ... U an, t) = t[r'/r] /\ r <= a1 U ... U an /\ a1  <= r /\ ... /\ an <= r *)
    let (t,args) = update_var ctx t result args in
    let args_vars = List.map (fun a -> G.Variable a) args in
    let t = G.add_constraint ctx t [G.Variable result] args_vars tn in
    let t = List.fold_left (fun t arg_var ->
      G.add_constraint ctx t [arg_var] [G.Variable result] tn
    ) t args_vars in
    t

    

  let transition_inter_set ctx t tn result args =
    (* post(r = a1 ^ ... ^ an, t) = t[r'/r] /\ a1 ^ ... ^ an <= r /\ r <= a1 /\ ... /\ r <= an *)
    let (t,args) = update_var ctx t result args in
    let args_vars = List.map (fun a -> G.Variable a) args in
    let t = G.add_constraint ctx t args_vars [G.Variable result] tn in
    let t = List.fold_left (fun t arg_var ->
      G.add_constraint ctx t [G.Variable result] [arg_var] tn
    ) t args_vars in
    t

  let transition_choose_fallback ctx t tne tn result set =
    (* fallback *)
    let t = if G.mem_atom ctx t (G.Singleton result) then
        G.project_atom ctx t (G.Singleton result)
      else t in
    let tn = ND.transition ctx tn (Commands.KillNumCommand, Common.null_position, [BaseSym.to_int result]) in
    (* add a new result *)
    let t = G.add_singleton ctx t result in
    let tne = ND.constrain ctx tne (Commands.EqualGuard, Common.null_position, [BaseSym.to_int result; BaseSym.to_int t.G.based]) in
    let t = G.add_constraint ctx t [G.Singleton result] [G.Variable set] tne in
    t,tn

  let transition_choose_explicit ctx t tn result rhs set lbl =
    (*Format.printf "choose_explicit: {%a} %a@." (fmt_vtx_list ", ") (G.G.VSet.elements set) (fmt_base ctx) lbl;*)
    (* project out a result singleton if it exists *)
    let t = if G.mem_atom ctx t (G.Singleton result) then
        G.project_atom ctx t (G.Singleton result)
      else t in

    (* add necessary constraint to label *)
    let lbl = ND.constrain ctx lbl (Commands.EqualGuard, Common.null_position, [BaseSym.to_int result; BaseSym.to_int t.G.based]) in
    let lbl = ND.transition ctx lbl (Commands.KillNumCommand, Common.null_position, [BaseSym.to_int t.G.based]) in

    (*Format.printf "lbl: %a@." (fmt_base ctx) lbl;*)

    (* update base domain *)
    let first_el = match G.G.VSet.choose set with
      | G.Singleton bv ->
          ND.transition ctx tn (Commands.AssignNumCommand, Common.null_position, [BaseSym.to_int result; BaseSym.to_int bv])
      | _ -> assert false
    in
    let tn' = G.G.VSet.fold (fun v tn' ->
      match v with
        | G.Singleton bv ->
            let new_state = ND.transition ctx tn (Commands.AssignNumCommand, Common.null_position, [BaseSym.to_int result; BaseSym.to_int bv]) in
            ND.join ctx new_state tn'
        | _ -> assert false
    ) set first_el in
    let tn' = ND.meet ctx (ND.rem_dim ctx lbl) tn' in

    (* update each edge in set *)
    let t' = G.copy_atoms ctx t in
    let t' = G.fold_constraints ctx (fun srcs dsts edge_lbl t' ->
      let edge_lbl' = G.G.VSet.fold (fun v edge_lbl' ->
        match v with
          | G.Singleton bv ->
              let new_state = ND.transition ctx edge_lbl (Commands.AssignNumCommand, Common.null_position, [BaseSym.to_int result; BaseSym.to_int bv]) in
              ND.join ctx new_state edge_lbl'
          | _ -> assert false
      ) set (G.base_bottom ctx t) in
      let edge_lbl' = ND.meet ctx lbl edge_lbl' in
      G.add_constraint ctx t' srcs dsts edge_lbl'
    ) t t' in

    (* add result constraint to set domain *)
    let tne' = ND.add_dim ctx tn' in
    let tne' = ND.constrain ctx tne' (Commands.EqualGuard, Common.null_position, [BaseSym.to_int result; BaseSym.to_int t.G.based]) in
    let t' = G.add_singleton ctx t' result in
    let t' = G.add_constraint ctx t' [G.Singleton result] [G.Variable rhs] tne' in
    (*Format.printf "set: %a@." (fmt_debug ctx) t';
    Format.printf "base: %a@." (fmt_base ctx) tn';*)
    (t', tn')


  let transition_choose ctx t tne tn result rhs =
    (* post(r = choose(s), t) =
       t[r'/r] /\ {r} <= s /\ if s <= {a} U ... U {b} then [[r:= a]] | [[r := ...]] | [[r := b]] else kill r
    *)
    (*Format.printf "choose: %a@." (fmt_debug ctx) t;*)
    let in_explicit = G.contained_in_explicit ctx t (G.Variable rhs) in
    let (t,tn) = match in_explicit with
      | Some (set, lbl) ->
          transition_choose_explicit ctx t tn result rhs set lbl
      | None ->
          transition_choose_fallback ctx t tne tn result rhs
    in
    set_to_base_reduction ctx t tn




  let transition_singleton_set ctx t tn result base =
    let (t,_) = update_var ctx t result [] in
    let t = if G.mem_atom ctx t (G.Singleton base) then t
      else G.add_singleton ctx t base in
    let t = G.add_variable ctx t result in
    let tn = ND.constrain ctx tn (Commands.EqualGuard, Common.null_position, [BaseSym.to_int base; BaseSym.to_int t.G.based]) in
    let t = G.add_constraint ctx t [G.Singleton base] [G.Variable result] tn in
    let t = G.add_constraint ctx t [G.Variable result] [G.Singleton base] tn in
    t

  let transition_empty_set ctx t result =
    let (t,_) = update_var ctx t result [] in
    let t = G.add_variable ctx t result in
    let t = G.add_constraint ctx t [G.Empty] [G.Variable result] (G.base_bottom ctx t) in
    let t = G.add_constraint ctx t [G.Variable result] [G.Empty]  (G.base_bottom ctx t) in
    t

  let transition_assign_set ctx t tn result set =
    let (t,renamed) = update_var ctx t result [set] in
    let set = match renamed with | [set] -> set | _ -> assert false in
    let t = G.add_constraint ctx t [G.Variable set] [G.Variable result] tn in
    let t = G.add_constraint ctx t [G.Variable result] [G.Variable set]  tn in
    t


  let transition_kill_set ctx t args =
    List.fold_left (fun t v ->
      let (t,_) = update_var ctx t v [] in
      t
    ) t args

  let transition_base ctx t tn (cmd, pos, (args : BaseSym.t list) ) =

    (* project out of set domain *)
    let t = match args with
      | result::_rest ->
          (* result is always first argument *)
          if G.mem_atom ctx t (G.Singleton result) then
            G.project_atom ctx t (G.Singleton result)
          else t
      | _ -> assert false
    in
    do_reduction_trans ctx pos t tn (cmd, pos, List.map BaseSym.to_int args)

    

  (* take the transition from state t, introducing new variables using var_gen
     return the post state *)
  let transition ctx (t,tn) (cmd, pos, args) =
    (*Format.printf "Dimension (pre): %a %d@." BaseSym.fmt t.G.based (ND.get_dim ctx tn);*)
    let tne = ND.add_dim ctx tn in
    (*Format.printf "Dimension (post): %d@." (ND.get_dim ctx tne);*)
    let (t,tn) = match (cmd, args) with
      | (Commands.SubSetCommand, [result; set; value]) ->
          (transition_sub_set ctx pos t tne (SetSym.of_int result) (SetSym.of_int set) (SetSym.of_int value), tn)
      | (Commands.UnionSetCommand, result::args) ->
          let args = List.map SetSym.of_int args in
          (transition_union_set ctx t tne (SetSym.of_int result) args, tn)
      | (Commands.InterSetCommand, result::args) ->
          let args = List.map SetSym.of_int args in
          (transition_inter_set ctx t tne (SetSym.of_int result) args, tn)
      | (Commands.AssignSetCommand, [result; set]) ->
          (transition_assign_set ctx t tne (SetSym.of_int result) (SetSym.of_int set), tn)
      | (Commands.EmptySetCommand, [result]) ->
          (transition_empty_set ctx t (SetSym.of_int result), tn)
      | (Commands.KillSetCommand, args) ->
          let args = List.map SetSym.of_int args in
          (transition_kill_set ctx t args, tn)

            
      | (Commands.ChooseCommand, [result; set]) ->
          transition_choose ctx t tne tn (BaseSym.of_int result) (SetSym.of_int set)
      | (Commands.SingletonSetCommand, [result; base]) ->
          (transition_singleton_set ctx t tne (SetSym.of_int result) (BaseSym.of_int base), tn)

      | (Commands.AddCommand, _)
      | (Commands.NegCommand, _)
      | (Commands.AssignNumCommand, _)
      | (Commands.ConstCommand _, _) ->
          transition_base ctx t tn (cmd, pos, List.map BaseSym.of_int args)
      | (Commands.KillNumCommand, _) ->
          List.fold_left (fun (t,tn) arg ->
            transition_base ctx t tn (cmd, pos, [BaseSym.of_int arg])
          ) (t,tn) args

      | (Commands.SubSetCommand, _)
      | (Commands.UnionSetCommand, _)
      | (Commands.InterSetCommand, _)
      | (Commands.SingletonSetCommand, _)
      | (Commands.EmptySetCommand, _)
      | (Commands.ChooseCommand, _)
      | (Commands.AssignSetCommand, _) ->
          assert false
    in
    post_process ctx t tn
      
end
