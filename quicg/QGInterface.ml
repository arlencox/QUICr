module type CommonDomain = sig
  type ctx

  type t

  val args : (Arg.key * Arg.spec * Arg.doc) list

  val init : unit -> ctx

  val meet : ctx -> t -> t -> t

  val join : ctx -> t -> t -> t

  val widen : ctx -> t -> t -> t

  val narrow : ctx -> t -> t -> t

  val is_bottom: ctx -> t -> bool

  val is_top: ctx -> t -> bool

  val le: ctx -> t -> t -> bool

end

module type EnvDomain = sig
  include CommonDomain

  type env


  val empty_env: ctx -> env

  val add_variable: ctx -> env -> string -> Types.t -> env

  val env: ctx -> t -> env

  val top : ctx -> env -> t

  val bottom : ctx -> env -> t

  val transition: ctx -> t -> Commands.command_t -> t

  val constrain: ctx -> t -> Commands.guard_t -> t

  val fmt: ?escape:FormatHelp.escape_t -> ctx -> Format.formatter -> t -> unit

end

module type NonEnvDomain = sig
  include CommonDomain

  val transition: ctx -> t -> Commands.command0_t -> t

  val constrain: ctx -> t -> Commands.guard0_t -> t

  (* [constrain_qc ctx t a b] adds constraint
     (intersect a) <= (union b) *)
  val constrain_qc: ctx -> t -> int list -> int list -> t

  val rename_set: ctx -> t -> int -> int -> t

  val forget_set: ctx -> t -> int -> t
end


module type SetNumDomain = sig
  include NonEnvDomain

  val top : ctx -> int -> int -> t

  val bottom : ctx -> int -> int -> t

  val fmt: ?escape:FormatHelp.escape_t -> ctx -> (Format.formatter -> int -> unit) -> (Format.formatter -> int -> unit) -> Format.formatter -> t -> unit

end


module type ScalarDomain = sig
  include NonEnvDomain

  val top : ctx -> int -> t

  val bottom : ctx -> int -> t

  val add_dim: ctx -> t -> t
  val rem_dim: ctx -> t -> t
  val get_dim: ctx -> t -> int
  val get_eq: ctx -> t -> int list list

  val fmt: ?escape:FormatHelp.escape_t -> ctx -> (Format.formatter -> int -> unit) -> Format.formatter -> t -> unit
end

module type SetDomain = sig
  include NonEnvDomain

  val top : ctx -> int -> int -> t

  val fmt: ?escape:FormatHelp.escape_t -> ctx -> (Format.formatter -> int -> unit) -> (Format.formatter -> int -> unit) -> Format.formatter -> t -> unit

end

module type SetBottomDomain = sig
  include SetDomain

  val bottom : ctx -> int -> int -> t
    
end
    


