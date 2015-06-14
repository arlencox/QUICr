module F = QBF_Formula

exception DepQBF_unexpected_result of string

let fail res cnf =
  let fname = Printf.sprintf "/tmp/depqbf_%d.cnf" (Unix.getpid ()) in
  let fout = open_out fname in
  F.pp_qdimacs fout cnf;
  close_out fout;
  let code = match res with
    | Unix.WEXITED id -> Printf.sprintf "exit(%d)" id
    | Unix.WSIGNALED id -> Printf.sprintf "signaled(%d)" id
    | Unix.WSTOPPED id -> Printf.sprintf "stopped(%d)" id
  in

  raise (DepQBF_unexpected_result (Printf.sprintf "%s %s" code fname))

let is_valid h f =
  let cnf = F.prenexcnf h (F.mnot h f) in
  let (fin,fout) = Unix.open_process "depqbf" in
  F.pp_qdimacs fout cnf;
  close_out fout;
  let res = Unix.close_process (fin,fout) in
  match res with
  | Unix.WEXITED id ->
    if id == 10 then false
    else if id == 20 then true
    else fail res cnf
  | _ ->
    fail res cnf

let counter = ref 0


let is_valid h f =
  (*Format.printf "Checking %a@." (F.pp_formula Format.pp_print_int) f;*)
  (*Format.printf "Checking %a@." F.pp_smtlib (F.mnot h f);*)
  let cnf = F.prenexcnf h (F.mnot h f) in
  let fname = Printf.sprintf "/tmp/depqbf_%d_%d.cnf" (Unix.getpid ()) !counter in
  incr counter;
  (*Format.printf "Running %s@." fname;*)
  let fout = open_out fname in
  F.pp_qdimacs fout cnf;
  close_out fout;
  let id = Sys.command (Printf.sprintf "depqbf %s > /dev/null" fname) in
  match id with
  | 10 -> false
  | 20 -> true
  | _ -> fail (Unix.WEXITED id) cnf
