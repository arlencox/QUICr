module Make(C: Interface.Ordered) = struct

  module S = Set.Make(C)

  type t = S.t * S.t (* definitely includes, definitely does not include *)

  let top = (S.empty, S.empty)

  let is_bottom (i, e) =
    not (S.is_empty (S.inter i e))

  let const s = (S.singleton s, S.empty)

  let union (ai,ae) (bi,be) =
    (S.union ai bi, S.inter ae be)

  let intersection (ai,ae) (bi,be) =
    (S.inter ai bi, S.union ae be)

  let complement (ai,ae) =
    (ae,ai)

  let join (ai,ae) (bi,be) =
    (S.inter ai bi, S.inter ae be)

  let widening = join

  let meet ((ai,ae): t) ((bi,be): t) : t =
    (S.union ai bi, S.union ae be)

  let le (ai,ae) (bi,be) =
    S.subset bi ai && S.subset ae be

  let cardinal (ai,ae) = (S.cardinal ai, S.cardinal ae)

end
