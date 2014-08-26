module Int = struct
  type t = int
  let compare = (-)
end

module ISet = Set.Make(Int)

type t = {
  nextid : int;
  freeid : ISet.t;
}

let empty = {
  nextid = 0;
  freeid = ISet.empty;
}

let fresh t =
  try
    let res = ISet.min_elt t.freeid in
    let freeid = ISet.remove res t.freeid in
    ({t with freeid}, res)
  with Not_found ->
    let res = t.nextid in
    let nextid = res + 1 in
    ({t with nextid}, res)
