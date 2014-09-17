
module type DomainSimp = sig
  type ctx   (** the context (mutable state) for managing the domain *)
  type sym   (** the type of symbols used in the domain *)
  type cnstr (** the type of constraints used in the domain *)
  type t     (** the type of the domain itself *)

  (** [init ()] creates a new context *)
  val init : unit -> ctx

  (** [top ctx syms] creates a new top element from a context [ctx] and a set
      of symbols [syms] in the domain *)
  val top : ctx -> sym list -> t

  (** [bottom ctx syms] creates a new bottom element from a context [ctx] and a
      set of symbols [syms] in the domain *)
  val bottom : ctx -> sym list -> t

  (** [context t] returns the context used to construct [t] *)
  val context : t -> ctx

  (** [symbols t] returns the symbols used to construct [t] *)
  val symbols : t -> sym list

  (** [join mapping a b] overapproximates the abstraction [a] or the
      abstraction [b].  The [mapping] is used to map symbols in [a] to symbols
      in [b] and the result.

      Each element in mapping [(syma, symb, symc)] cause [syma] to be matched
      with [symb] producing [symc].
  *)
  val join : (sym * sym * sym) list -> t -> t -> t

  (** [widening mapping a b] overapproximates the abstraction [a] or the
      abstraction [b], guaranteeing stabilization..  The [mapping] is used to
      map symbols in [a] to symbols in [b] and the result *)
  val widening : (sym * sym * sym) list -> t -> t -> t

  (** [meet mapping a b] overapproximates the abstraction [a] and the
      abstraction [b].  The [mapping] is used to map symbols in [a] to symbols
      in [b] and the result *)
  val meet : (sym * sym * sym) list -> t -> t -> t

  (** [le mapping a b] determines if [a] is contained in [b] ([a] is less than
      or equal to [b]) under mapping [mapping]. *)
  val le : (sym * sym) list -> t -> t -> bool

  (** [constrain c t] abstracts constraint [c] and computes meet of the result
      with [t] *)
  val constrain : cnstr -> t -> t

  (** [constrain_eq s1 s2 a] adds an equality constraint between symbol [s1]
      and symbol [s2] to [t] *)
  val constrain_eq : sym -> sym -> t -> t

  (** [sat t c] checks if the abstract state [t] satisfies the constraints [c].
      If [c] is unrepresentable by the domain this will always be false.
  *)
  val sat : t -> cnstr -> bool

  (** [add_symbols ?level:l syms t] adds symbols [syms] to [t] domain at level
      [l].  If domain is parameterized by a base domain, [l] determines which
      domain is modified.  A level of 0 (default) is this domain.  Symbols that
      are already in [t] are ignored *)
  val add_symbols : ?level:int -> sym list -> t -> t

  (** [remove_symbols ?level:l syms t] removes symbols [syms] from [t] domain
      at level [l].  If domain is parameterized by a base domain, [l]
      determines which domain is modified.  A level of 0 (default) is this
      domain.  Symbols that are already not in [t] are ignored *)
  val remove_symbols : ?level:int -> sym list -> t -> t

  (** [forget syms t] forgets meaning of the symbols [syms] in the domain [t]
      at level [l].  If domain is parameterized by a base domain, [l]
      determines which domain is modified.  A level of 0 (default) is this
      domain.  In effect, this removes the symbols and readds them as fresh. *)
  val forget : ?level:int -> sym list -> t -> t

  (** [rename_symbols f t] renames each symbol in [t] at level [l] to the
    corresponding symbol returned by looking up the symbol in the association
    list [f].  If domain is parameterized by a base domain, [l] determines
    which domain is modified.  A level of 0 (default) is this domain.
    Relationships are maintained, assuming the mapping does not reuse any
    symbols *)
  val rename_symbols : ?level:int -> (sym * sym) list -> t -> t

  (** [to_constraint t] returns the constraint that represents [t] *)
  val to_constraint : t -> cnstr

  (** [equalities t] returns the set of equalities implied by [t] *)
  val equalities : t -> (sym * sym) list
end


(** Domain with convenience functions *)
module type Domain = sig
  include DomainSimp

  (** [top d] creates a new top element from an existing domain element [d],
      symbols and context are copied *)
  val topd : t -> t

  (** [bottom d] creates a new bottom element from an existing domain element
      [d], symbols and context are copied *)
  val bottomd : t -> t

  (** [abstract ctx syms c] creates a new element that overapproximates [cnstr]
      from a context [ctx] and a set of symbols [syms] *)
  val abstract : ctx -> sym list -> cnstr -> t

  (** [abstractd d c] creates a new element that overapproximates [cnstr] from
    an existing domain element [d], symbols and context are copied from [d] *)
  val abstractd : t -> cnstr -> t
end

module type Ordered = sig
  type t
  val compare: t -> t -> int
end

module type Sym = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
end

module type Constant = sig
  type sym
  type cnstr
  type t

  (** returns a list of var = const pairs or none if false *)
  val constrain: cnstr -> (sym * t) list option

  (** returns a list of var = const pairs or none if a constraint cannot be converted *)
  val sat: cnstr -> (sym * t) list option

  val to_constraint: (sym * t) list -> cnstr

  val compare: t -> t -> int
end
