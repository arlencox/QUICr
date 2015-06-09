module type Comparable = sig
  type t
  val compare : t -> t -> int
end
  
module Make(Task: Comparable) = struct
  module TMap = Mapx.Make(Task)
  module IMap = Mapx.Make(struct
    type t = int
    let compare = compare
  end)

  type t = {
    in_queue: int TMap.t;
    queue: Task.t IMap.t;
  }

  exception Complete

  let empty = {
    in_queue = TMap.empty;
    queue = IMap.empty;
  }

  let add wl t =
    if TMap.mem t wl.in_queue then
      wl
    else begin
      let index =
        try
          let prev_ind = fst (IMap.max_binding wl.queue) in
          assert(prev_ind < max_int);
          prev_ind + 1
        with Not_found ->
          min_int
      in
      assert(not (IMap.mem index wl.queue));
      { in_queue = TMap.add t index wl.in_queue;
        queue = IMap.add index t wl.queue }
    end

  let pop wl =
    try
      let (index,task) = IMap.min_binding wl.queue in
      let wl = {
        in_queue = TMap.remove task wl.in_queue;
        queue = IMap.remove index wl.queue;
      } in
      (wl, task)
    with Not_found ->
      raise Complete
    

  let rec run f wl =
    let wltask = try Some (pop wl) with Complete -> None in
    match wltask with
      | Some (wl,task) ->
          let wl = f wl task in
          run f wl
      | None ->
          ()

  let run_fold f wl result =
    let result = ref result in
    run (fun wl t ->
      let (wl, res) = f wl t !result in
      result := res;
      wl
    ) wl;
    !result
      
end
