let print_environment = ref false

let args = [
  ("-print-env", Arg.Set print_environment, "Print out the environment at various points in the execution")
]
module Make(D: QGInterface.SetNumDomain) : QGInterface.EnvDomain = struct
  open Common

  type ctx = D.ctx

  type env = {
    scalar_count: int;
    set_count: int;
    map: (Types.t * int) SMap.t
  }

  type t = env * D.t

  let args = D.args

  let print_env e = 
    if !print_environment then begin
      Format.printf "Environment:@.";
      SMap.iter (fun name (typ, num) ->
                   match typ with
                     | Types.Set ->
                         Format.printf "  %s s%d@." name num
                     | Types.Number ->
                         Format.printf "  %s b%d@." name num
                     | Types.Bool ->
                         assert false
      ) e.map
    end


  let env_equal a b =
    ((SMap.compare compare a.map b.map) = 0) &&
      (a.scalar_count = b.scalar_count) &&
      (a.set_count = b.set_count)
      
  let init = D.init

  let meet ctx (ea,a) (eb,b) =
    assert(env_equal ea eb);
    (ea, D.meet ctx a b)

  let join ctx (ea,a) (eb,b) =
    assert(env_equal ea eb);
    print_env ea;
    (ea, D.join ctx a b)

  let widen ctx (ea,a) (eb,b) =
    assert(env_equal ea eb);
    (ea, D.widen ctx a b)

  let narrow ctx (ea,a) (eb,b) =
    assert(env_equal ea eb);
    (ea, D.narrow ctx a b)

  let is_bottom ctx (e,a) =
    D.is_bottom ctx a

  let is_top ctx (e,a) =
    D.is_top ctx a

  let le ctx (ea,a) (eb,b) =
    assert(env_equal ea eb);
    D.le ctx a b

  let empty_env ctx =
    {
      scalar_count = 0;
      set_count = 0;
      map = SMap.empty;
    }

  let add_variable ctx e n t =
    let env = match t with
      | Types.Number ->
          let i = e.scalar_count in
          { e with
            scalar_count = e.scalar_count + 1;
            map = SMap.add n (t, i) e.map;
          }
      | Types.Set ->
          let i = e.set_count in
          { e with
            set_count = e.set_count + 1;
            map = SMap.add n (t, i) e.map;
          }
      | Types.Bool -> assert false
    in
    print_env env;
    env

  let env ctx (e,_) = e

  let top ctx e =
    (e, D.top ctx e.scalar_count e.set_count)

  let bottom ctx e =
    (e, D.bottom ctx e.scalar_count e.set_count)

  let transition ctx (e, t) (cmd,pos,args) =
    (e, D.transition ctx t (cmd,pos,List.map (fun arg ->
      (*print_endline ("env_transition: " ^ arg);*)
      let (typ,i) = SMap.find arg e.map in
      i
     ) args))
      

  let constrain ctx (e, t) (guard,pos,args) =
    (e, D.constrain ctx t (guard,pos,List.map (fun arg -> let (typ,i) = SMap.find arg e.map in i) args))

  let fmt ?escape:(escape=FormatHelp.Text) ctx ff (e,t) =
    let (nmap, smap) = SMap.fold (fun name (typ, i) (nmap,smap) ->
      match typ with
      | Types.Number ->
        (IMap.add i name nmap, smap)
      | Types.Set ->
        (nmap, IMap.add i name smap)
      | _ -> assert false
    ) e.map (IMap.empty,IMap.empty) in
    let fmt_scalar ff id =
      try
        Format.fprintf ff "%s" (IMap.find id nmap)
      with Not_found ->
        Format.fprintf ff "$i%d" id
    in
    let fmt_set ff id =
      try
        Format.fprintf ff "%s" (IMap.find id smap)
      with Not_found ->
        Format.fprintf ff "$v%d'" (-id)
    in
    D.fmt ~escape:escape ctx fmt_scalar fmt_set ff t
      
      
    
end
