(** [Domain] is a basic domain description.  It supports all basic domain
    operations including join, widening, inclusion check *)
module type Domain = sig
  type ctx    (** the context (mutable state) for managing the domain *)
  type t      (** the type of the domain itself *)
  type sym    (** the type of symbols used in the domain *)
  type cnstr  (** the type of constraints used in the domain *)
  type output (** the type returned when serializing the abstract state *)
  type query  (** the type of query and constrain commands suitable for this domain *)

  (** [init ()] creates a new context *)
  val init : unit -> ctx

  (** [top ctx] creates a new top element from a context [ctx]. *)
  val top : ctx -> t

  (** [bottom ctx] creates a new bottom element from a context [ctx]. *)
  val bottom : ctx -> t

  (** [context t] returns the context used to construct [t] *)
  val context : t -> ctx

  (** [symbols t] returns the symbols constrained in [t] *)
  val symbols : t -> sym list

  (** {2 Constraint Interfaces} *)

  (** [constrain c t] abstracts constraint [c] and computes meet of the result
      with [t] *)
  val constrain : cnstr -> t -> t

  (** [serialize t] returns the constraint that represents [t] *)
  val serialize : t -> output

  (** [sat t c] checks if the abstract state [t] satisfies the constraints [c].
      If [c] is unrepresentable by the domain this will always be false.
  *)
  val sat : t -> cnstr -> bool

  (** {2 Binary Domain Operations} *)

  (** [join a b] overapproximates the abstraction [a] or the
      abstraction [b].  *)
  val join : t -> t -> t

  (** [widening a b] overapproximates the abstraction [a] or the
      abstraction [b], guaranteeing stabilization. *)
  val widening : t -> t -> t

  (** [meet a b] overapproximates the abstraction [a] and the
      abstraction [b]. *)
  val meet : t -> t -> t

  (** [le a b] determines if [a] is contained in [b] ([a] is less than
      or equal to [b]). *)
  val le : t -> t -> bool

  (** [is_bottom t] returns true if [t] is the bottom element *)
  val is_bottom: t -> bool

  (** {2 Symbol Manipulation} *)

  (** [forget syms t] forgets meaning of (projects out) the symbols [syms] in
      the domain [t]. *)
  val forget : sym list -> t -> t

  (** [rename_symbols f t] renames each symbol in [t] to the corresponding
      symbol returned by looking up the symbol in the association list [f].
      Relationships are maintained, assuming the mapping does not reuse any
      symbols *)
  val rename_symbols : (sym * sym) list -> t -> t

  (** {2 Query interface} *)

  (** [query t] returns a query datatype that is bound to the abstract state
      [t] *)
  val query: t -> query

  (** [combine q t] use a query from another abstract state (in a different
      domain) to strengthen this domain *)
  val combine: query -> t -> t

end

module type DomainRenamable = sig
  include Domain

  (** [join map a b] overapproximates the abstraction [a] or the abstraction
      [b].  The [map] is used to map symbols in [a] to symbols in [b] and the
      result.

      Each element in mapping [(syma, symb, symc)] cause [syma] to be matched
      with [symb] producing [symc].  The same symbol should not occur more than
      once. *)
  val join_map : (sym * sym * sym) list -> t -> t -> t

  (** [widening map a b] overapproximates the abstraction [a] or the
      abstraction [b], guaranteeing stabilization..  The [map] is used to map
      symbols in [a] to symbols in [b] and the result *)
  val widening_map : (sym * sym * sym) list -> t -> t -> t

  (** [meet map a b] overapproximates the abstraction [a] and the abstraction
      [b].  The [map] is used to map symbols in [a] to symbols in [b] and the
      result *)
  val meet_map : (sym * sym * sym) list -> t -> t -> t

  (** [le map a b] determines if [a] is contained in [b] ([a] is less than or
      equal to [b]) under mapping [map]. *)
  val le_map : (sym * sym) list -> t -> t -> bool
end

(** [Sym] describes a modules for representing symbols.  Typical candidates
    would be integers and strings. *)
module type Sym = sig
  type t

  (** [compare a b] creates a total ordering on symbols.  Should return a
      negative number if [a] < [b], a positive number if [a] > [b], and 0 if
      [a] = [b] *)
  val compare: t -> t -> int

  (** [to_string t] converts a symbol to a string *)
  val to_string: t -> string

  (** [hash t] returns a hash code for a symbol, to be used in hash combinators
    or hash functions *)
  val hash: t -> int

  (** [fresh ()] returns a fresh symbol that is guaranteed to not conflict with
      any user-specified symbol or previously created fresh variable *)
  val fresh: unit -> t
end
