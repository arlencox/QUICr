# QUICr Sets Abstract Domain Library

QUICr is a collection of abstract domains for sets with a common interface.

Currently, it is focused on symbolic sets with singletons.  Therefore, it does
not (currently) support the content reasoning originally offered by QUICr.  The
original version of QUICr that offers content reasoning is available at
http://pl.cs.colorado.edu/projects/quicgraphs/


## Abstract Domains

QUICr is a common domain interface for multiple abstractions.  It is designed
to support many underlying abstractions and reductions between them under a
single common interface.  Thus any abstract interpreter that utilizes the
interface can swap in and out any of the included set abstractions.

The following abstractions intend to be explored (though they many not all be
included at this time).

### BDD-based Set Abstractions

These abstractions use binary decision diagrams as the underlying data
strucutre.  This means that they have a normal form that may not be optimal,
but they are quite general and precise.  In this space, we plan to implement
the following abstractions:

#### BDD-Full: Simple BDD-based Set Abstraction

This directly implements a set abstraction using a BDDs and, or, and not
operations to represent set intersection, union, and complement respectively.
As a result, this does not necessarily offer a compact representation for
operations like disjoint union, which requires encoding in the BDD the
pair-wise disjointness constraints.

#### Disjoint-less BDD-based Set Abstraction

This abstraction does not encode disjointness constraints.  They could be added
externally through a domain combinator, but are not supported in the BDD.  This
attempts to reduce the overhead of keeping track of these disjointness
constraints from the BDD.  Consequently all disjoint union operations are
overapproximated as union operations, dropping the disjointness constraint.

#### Functional BDD-based Set Abstraction

This abstraction is a BDD-based set abstraction without the global memoization
table.  This is a persistent version of the simple BDD-based set abstraction.

### Theorem Prover-based Set Abstractions

These are set abstractions that are based on theorem provers or
theorem-prover-like data structures.  Because these are designed around search,
they do not maintain normal forms, but they can, in theory be precise and
flexible for representing set constraints.


#### SAT-based Set Abstraction

Use a SAT solver to solve set constraints and for the inclusion check proofs.
Essentially this abstraction accumulates formulas representing the constraints.
When an inclusion check is started, the formula is converted into a SAT problem
and solved.  Join and widening are implemented as disjunction.

#### Sequent Calculus Representation Abstraction

Set constraints can be represented in a sequent calculus and can thus use
sequent-calculus-theorem-prover techniques to solve inclusion check.  As
opposed to the SAT based techniques, these could provide on-the-fly formula
simplifications and perform more incremental inclusion checks, possibly saving
significant work.

The original QUIC graphs is a variant of a sequent calculus representation.
However, it is not utilizing proof search techniques such as focusing.  It uses
a simple cut-based technique, combined with a heuristic join and widening
algorithm.

#### Espresso Logic Minimizer-based Abstraction

Use the Espresso logic minimizer to perform on-the-fly simplifications of the
current representation.  This efficiently reduces sequent-calculus-based
representations and may improve performance of possible join algorithms.

#### Model Checking-based Union Abstraction

A hardware model checking engine can be used to infer search for optimal
union-only constraints in a QUIC-graph-like structure.  It can be use for
directly implementing join and may serve as a good mechanism for pairing with
other theorem-prover-based techniques, which typically only optimize for
inclusion check.

### Fixed Predicate Techniques

These techniques are more targeted at specific applications.  They are designed
for use for solving certain kinds of problems and thus they can have data
structures designed for the application-specific predicates.

#### Memcad Linear Set Abstraction

This is a set abstraction built around the idea of linear set constraints such
as:

```
X = {a, b, c} ⊎ Y ⊎ Z
```

It is restricted to support only disjoint union and limited use of subset
constraints.  Join and inclusion check operations are heuristic and tuned for
the intended application.

#### Linear and Difference Set Abstraction

This is an abstraction that extends linear set constraints with difference
constraints to eliminate case splits that may occur when materializing elements
from a set.

#### FixBag (and adaptations)

The FixBag abstraction uses saturation based inclusion check and join based on
a limited set of propagation an simplification rules.  It uses a syntactic
representation of logical formulas as its data structure.

#### Algebraic Set Abstraction

TBD

## Test Language

This package will also define the SDSL, a domain specific language for
manipulating symbolic sets.  The language has the following syntax:

```
k ::= x = e
    | k; k                       // sequencing two commands
    | if ( c ) { k }             // conditional execution
    | if ( c ) { k } else { k }
    | while ( c ) { k }          // looping

e ::= v                          // set variable
    | { v }                      // singleton set
    | e U e                      // union
    | e U+ e                     // disjoint union
    | e \ e                      // set difference
    | e ^ e                      // intersection
    | ~e                         // set complement

c ::= e = e                      // set equality
    | e <= e                     // subset or equality
    | v in e                     // element containment
```
