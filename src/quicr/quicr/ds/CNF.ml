type lit = bool * int

type clause = lit list

type clauses = clause list

let mk_true = []

let mk_false = [[]]

let mk_and a b = a @ b

let mk_var v = [[(true,v)]]

let rec iter_distrib f acc clauses =
  match clauses with
  | [] -> f acc
  | h::t ->
    List.iter (fun l -> iter_distrib f (l::acc) t) h

let iter_distrib f clauses = iter_distrib f [] clauses

module LSet = Set.Make(struct
    type t = lit
    let compare = compare
  end)


let mk_not a =
  let clauses = ref [] in
  iter_distrib (fun nclause ->
      let clause = List.fold_left (fun s (b,v) -> LSet.add (not b, v) s) LSet.empty nclause in

      let is_true = LSet.for_all (fun (b,v) -> not (LSet.mem (not b, v) clause)) clause in
      if is_true then
        clauses := (LSet.elements clause) :: !clauses
      else
        ()
    ) a;
  
  !clauses


let mk_or a b =
  List.fold_left (fun clauses clausea ->
      List.fold_left (fun clauses clauseb ->
          (List.rev_append clausea clauseb)::clauses
        ) clauses b
    ) [] a
  (*mk_not (mk_and (mk_not a) (mk_not b))*)

(* ~~(~a \/ b) = ~(a /\ ~b) *)
let mk_imply a b =
  mk_or (mk_not a) b

(*
   a -> b
/\ b -> a

   ~(a /\ ~b)
/\ ~(b /\ ~a)
*)
let mk_eq a b =
  mk_and (mk_imply a b) (mk_imply b a)

exception Clause_true

let cofactor is_pos v a =
  List.fold_left (fun clauses clause ->
      try
        let clause = List.fold_left (fun clause (pos, l) ->
            if l = v then
              if pos = is_pos then
                raise Clause_true
              else
                clause
            else
              (pos, l)::clause
          ) [] clause in
        clause::clauses
      with Clause_true ->
        clauses
    ) [] a

let mk_forall v a =
  mk_and (cofactor true v a) (cofactor false v a)

let mk_exists v a =
  mk_or (cofactor true v a) (cofactor false v a)

module Int = struct
  type t = int
  let compare = compare
end

module ISet = Set.Make(Int)

let symbols (clauses:clauses) : int list =
  let ss = List.fold_left (List.fold_left (fun vars (_,v) -> ISet.add v vars)) ISet.empty clauses in
  ISet.elements ss
  

let pp pp_sym ff clauses =
  let first = ref true in
  List.iter (fun clause ->
      if !first then
        first := false
      else
        Format.pp_print_string ff " /\\ ";
      let inner_first = ref true in
      List.iter (fun (b,a) ->
          if !inner_first then
            inner_first := false
          else
            Format.pp_print_string ff " \\/ ";
          if not b then
            Format.pp_print_string ff "~";
          pp_sym ff a
        ) clause
    ) clauses

let pp_int = pp Format.pp_print_int

(*let _ =
  let a = mk_var 1 in
  let b = mk_var 2 in
  let c = mk_var 3 in
  let d = mk_var 4 in
  Format.printf "%a@." pp_int (mk_eq
                                 (mk_and a d)
                                 (mk_or b c))*)

