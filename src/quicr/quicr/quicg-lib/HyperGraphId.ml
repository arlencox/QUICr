(* vertex and edge ids *)
type vtx_id = int
type edge_id = int

(* instantiate the functor hypergraph *)
module HG = HyperGraph.Make
  (struct
    type t = vtx_id
    let compare = compare
    let pp_print ff id =
      Format.fprintf ff "v%d" id
   end)
  (struct
    type t = unit
    let compare = compare
    let pp_print ff () = ()
   end)
  (struct
    type t = unit
    let compare = compare
    let pp_print ff () = ()
   end)
  (struct
    type t = edge_id
    let compare a b = 0
    let pp_print ff id =
      Format.fprintf ff "e%d" id
   end)

module IMap = Mapx.Make(struct
  type t = int
  let compare = compare
end)

(* hypergraph type *)
type ('vl, 'el) t = {
  v_id2lbl: 'vl IMap.t;
  e_id2lbl: 'el IMap.t;
  graph: HG.t;
  max_vertex: vtx_id;
  max_edge: edge_id;
}

let empty = {
  v_id2lbl = IMap.empty;
  e_id2lbl = IMap.empty;
  graph = HG.empty;
  max_vertex = 0;
  max_edge = (-1);
}

let edge_create srcl dstl eid =
  HG.E.create (List.map (fun el -> el,()) srcl) (List.map (fun el -> el,()) dstl) eid

let add_edge g srcl dstl lbl =
  let eid = g.max_edge in
  let edge = edge_create srcl dstl eid in
  let graph = HG.add_edge g.graph edge in
  ({ g with
    e_id2lbl = IMap.add eid lbl g.e_id2lbl;
    max_edge = g.max_edge - 1;
    graph = graph;
  }, eid)


let edge_id g srcl dstl =
  let tmp_edge = edge_create srcl dstl (-1) in
  let e = HG.get_edge g.graph tmp_edge in
  HG.E.get_label e



let mem_edge g srcl dstl =
  try
    ignore(edge_id g srcl dstl);
    true
  with Not_found ->
    false

let edge_label g eid =
  IMap.find eid g.e_id2lbl

let remove_edge g srcl dstl =
  let tmp_edge = edge_create srcl dstl (-1) in
  let e = HG.get_edge g.graph tmp_edge in
  let eid = HG.E.get_label e in
  let graph = HG.remove_edge g.graph e in
  { g with
    e_id2lbl = IMap.remove eid g.e_id2lbl;
    graph;
  }

let add_vertex g vtx =
  let vid = g.max_vertex in
  let graph = HG.add_vtx g.graph vid in
  ({ g with
    v_id2lbl = IMap.add vid vtx g.v_id2lbl;
    max_vertex = vid + 1;
    graph;
  },vid)

let vertex_label g vid =
  IMap.find vid g.v_id2lbl

let mem_vertex g vid =
  try
    ignore (vertex_label g vid);
    true
  with Not_found ->
    false

let remove_vertex g vid =
  let v_id2lbl = IMap.remove vid g.v_id2lbl in
  let graph = HG.remove_vtx g.graph vid in
  { g with
    v_id2lbl;
    graph;
  }

let edge_count g =
  IMap.cardinal g.e_id2lbl

let vertex_count g =
  IMap.cardinal g.v_id2lbl


let iter_succ f g vid =
  HG.iter_succ (fun edge ->
    f (HG.E.get_src_vtxs edge) (HG.E.get_dst_vtxs edge) (HG.E.get_label edge)
  ) g.graph vid

let fold_succ f g vid st =
  let st = ref st in
  iter_succ (fun srcl dstl eid ->
    st := f srcl dstl eid !st
  ) g vid;
  !st

let iter_pred f g vid =
  HG.iter_pred (fun edge ->
    f (HG.E.get_src_vtxs edge) (HG.E.get_dst_vtxs edge) (HG.E.get_label edge)
  ) g.graph vid

let fold_pred f g vid st =
  let st = ref st in
  iter_pred (fun srcl dstl eid ->
    st := f srcl dstl eid !st
  ) g vid;
  !st

let iter_edge f g =
  HG.iter_edges (fun edge ->
    f (HG.E.get_src_vtxs edge) (HG.E.get_dst_vtxs edge) (HG.E.get_label edge)
  ) g.graph

let fold_edge f g vid st =
  let st = ref st in
  iter_edge (fun srcl dstl eid ->
    st := f srcl dstl eid !st
  ) g;
  !st

let iter_vertex f g =
  HG.iter_vtxs f g.graph

let fold_vertex f g st =
  HG.fold_vtxs f g.graph st

let pp_dot fmt_vtx fmt_edge ff g =
  let id_start = ref (g.max_vertex + 1) in
  Format.fprintf ff "@[<v 0>@[<v 2>digraph g {@,";
  iter_vertex (fun vid ->
    Format.fprintf ff "v%d [label=\"%a\"];@," vid fmt_vtx (vertex_label g vid)
  ) g;
  iter_edge (fun srcs dsts eid ->
    match srcs,dsts with
      | [src],[dst] ->
          Format.fprintf ff "v%d -> v%d [label=\"%a\"];@," src dst fmt_edge (edge_label g eid)
      | srcs,dsts ->
          let this_id = !id_start in
          incr id_start;
          Format.fprintf ff "v%d [label=\"%a\"];@," this_id fmt_edge (edge_label g this_id);
          List.iter (fun src ->
            Format.fprintf ff "v%d -> v%d;@," src this_id
          ) srcs;
          List.iter (fun dst ->
            Format.fprintf ff "v%d -> v%d;@," this_id dst
          ) dsts;
  ) g;
  Format.fprintf ff "@]@,}@]"
