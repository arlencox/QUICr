module type SetDom = Interface.Domain
  with type sym = int
   and type cnstr = int LogicSymbolicSet.t
   and type output = int LogicSymbolicSet.t
   and type query = int LogicSymbolicSet.q

type t =
  | Bool of bool
  | String of string
  | Int of int
  | SetDomain of (module SetDom)

exception Build_error of string

let build_domain name args =
  match String.lowercase name, args with

(************************************************)
(**             Abstract Domains               **)
(************************************************)
#ifdef PKG_MLBDD
  | "bdd-full", [] ->
    SetDomain (module BDDFull.Domain)
  | "bdd-full", _ ->
    raise (Build_error "bdd-full domain does not accept any arguments")
  | "bdd-opt", [] ->
    SetDomain (module Sing.Domain.Make(Eq.Domain.Make(Packer.Domain.Make(BDDFull.Domain))))
  | "bdd-opt", _ ->
    raise (Build_error "bdd-full domain does not accept any arguments")
#else
  | "bdd-full", _
  | "bdd-opt", _ ->
    raise (Build_error "BDD-based domains were disabled at compile time")
#endif
  | "lin", [] ->
    SetDomain (module Lin.Domain)
  | "lin", _ ->
    raise (Build_error "lin domain does not accept any arguments")
#ifdef PKG_Z3
  | "smt", [] ->
    SetDomain (module SMT.Domain)
  | "smt", _ ->
    raise (Build_error "smt domain does not accept any arguments")
  | "quic", [] ->
    SetDomain (module QUICG.Domain)
  | "quic", _ ->
    raise (Build_error "quic domain does not accept any arguments")
#else
  | "smt", _
  | "quic", _ ->
    raise (Build_error "SMT-based domains smt and quic were disabled at compile time")
#endif
#ifdef PKG_QBF
  | "qbf", l ->
    let arg = match l with
      | [] -> "depqbf"
      | [String s] -> s
      | _ ->
        raise (Build_error "qbf domain accepts an optional string argument specifying the solver")
    in
    let s = match arg with
      | "depqbf" -> (module QBF.SolverDepQBF : QBF.Domain.Solver)
      | s -> (Build_error ("QBF solver '"^s^"' is not supported"))
    in
    SetDomain (module QBF.Domain.Make((val s)))
#else
  | "qbf", _ ->
    raise (Build_error "QBF-based domain was disabled at compile time")
#endif


(************************************************)
(**            Domain Combinators              **)
(************************************************)

  | "logger", l ->
    let fname, d = match l with
      | [String fname; SetDomain d] -> (fname, d)
      | [SetDomain d] -> ("domain.log", d)
      | _ ->
        raise (Build_error "logger takes an optional file name and a set domain as arguments")
    in
    let module L = (struct
      let file = fname
    end) in
    let module D = (val d) in
    let module Log = Logger.Domain.Make(L)(D) in
    SetDomain (module Log)

  | "pack", [SetDomain d] ->
    SetDomain (module Packer.Domain.Make((val d)))
  | "pack", _ ->
    raise (Build_error "pack takes a set domain as an argument")

  | "eq", [SetDomain d] ->
    SetDomain (module Eq.Domain.Make((val d)))
  | "eq", _ ->
    raise (Build_error "eq takes a set domain as an argument")

  | "debug", [SetDomain d] ->
    SetDomain (module Debugger.Domain.Make((val d)))
  | "debug", _ ->
    raise (Build_error "debug takes a set domain as an argument")

  | "trace", l ->
    let default_name = "trace.strace" in
    let default_check = false in
    let c, s, d = match l with
      | [Bool c; String s; SetDomain d] -> (c, s, d)
      | [String s; SetDomain d] -> (default_check, s, d)
      | [Bool c; SetDomain d] -> (c, default_name, d)
      | [SetDomain d] -> (default_check, default_name, d)
      | _ ->
        raise (Build_error "trace takes an optional boolean, an optional file name, and a set domain")
    in
    let module L = (struct
      let file = s
      let check = c
    end) in
    SetDomain (module Tracer.Domain.Make(L)((val d)))

  | "sing", [SetDomain d] ->
    SetDomain (module Sing.Domain.Make((val d)))
  | "sing", _ ->
    raise (Build_error "sing takes a set domain as an argument")

  | "stats", [SetDomain d] ->
    SetDomain (module Stats.Domain.Make((val d)))
  | "stats", _ ->
    raise (Build_error "stats takes a set domain as an argument")

(************************************************)
(**                Error Case                  **)
(************************************************)

  | s, _ ->
    raise (Build_error ("unrecognized domain '"^s^"'"))

let help_string =
"  The domain construction language has the following grammar:

  d ::= lin                  - A linear abstraction for disjoint unions
      | bdd-full             - A BDD-based abstraction for sets
      | bdd-opt              - A pre-built optimized combination of domains
      | smt                  - An SMT-based abstraction for sets
      | quic                 - A QUIC-graphs-based abstraction for sets
      | qbf [ <'solver'> ]   - A QBF-based abstraction for sets.
                               An optional solver may be provided.

      | logger< ['file',] d> - Log domain operations to file.
                               Default file is domain.log.
      | pack<d>              - Use a separate abstract state for each group of
                               related variables.
      | eq<d>                - Track equality externally.
      | debug<d>             - Turn on debug printer for domain.
      | trace< [true,] ['file',] d >
                             - Generate strace file for domain operations.
                               If true is provided as first argument, produce a
                               sat check of each constraint after each
                               constrainquery.  Optional file name for trace
                               defaults to trace.strace.
      | sing<d>              - Syntactic handling of singleton sets.
      | stat<d>              - Collect domain statistics"

let get_help () =
  Format.printf "%s@." help_string;
  exit 2
