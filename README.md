# QUICr Sets Abstract Domain Library

QUICr is a collection of abstract domains for sets with a common interface.

Currently, it is focused on symbolic sets with singletons.  Therefore, it does
not (currently) support the content reasoning originally offered by QUICr.  The
original version of QUICr that offers content reasoning is available at
http://pl.cs.colorado.edu/projects/quicgraphs/


## Abstract domains

QUICr is a common domain interface for multiple abstractions.  It is designed
to support many underlying abstractions and reductions between them under a
single common interface.  Thus any abstract interpreter that utilizes the
interface can swap in and out any of the included set abstractions.

The following abstractions intend to be explored (though they many not all be
included at this time).

### BDD-based set abstractions

These abstractions use binary decision diagrams as the underlying data
structure.  This means that they have a normal form that may not be optimal,
but they are quite general and precise.  In this space, we plan to implement
the following abstractions:

#### BDD-full: simple BDD-based set abstraction

This directly implements a set abstraction using a BDDs and, or, and not
operations to represent set intersection, union, and complement respectively.
As a result, this does not necessarily offer a compact representation for
operations like disjoint union, which requires encoding in the BDD the
pair-wise disjointness constraints.

#### Disjoint-less BDD-based set abstraction

This abstraction does not encode disjointness constraints.  They could be added
externally through a domain combinator, but are not supported in the BDD.  This
attempts to reduce the overhead of keeping track of these disjointness
constraints from the BDD.  Consequently all disjoint union operations are
over-approximated as union operations, dropping the disjointness constraint.

#### Functional BDD-based set abstraction

This abstraction is a BDD-based set abstraction without the global memoization
table.  This is a persistent version of the simple BDD-based set abstraction.

### Theorem prover-based set abstractions

These are set abstractions that are based on theorem provers or
theorem-prover-like data structures.  Because these are designed around search,
they do not maintain normal forms, but they can, in theory be precise and
flexible for representing set constraints.


#### SAT-based set abstraction

Use a SAT solver to solve set constraints and for the inclusion check proofs.
Essentially this abstraction accumulates formulas representing the constraints.
When an inclusion check is started, the formula is converted into a SAT problem
and solved.  Join and widening are implemented as disjunction.

#### Sequent calculus representation abstraction

Set constraints can be represented in a sequent calculus and can thus use
sequent-calculus-theorem-prover techniques to solve inclusion check.  As
opposed to the SAT based techniques, these could provide on-the-fly formula
simplifications and perform more incremental inclusion checks, possibly saving
significant work.

The original QUIC graphs is a variant of a sequent calculus representation.
However, it is not utilizing proof search techniques such as focusing.  It uses
a simple cut-based technique, combined with a heuristic join and widening
algorithm.

#### Espresso logic minimizer-based abstraction

Use the Espresso logic minimizer to perform on-the-fly simplifications of the
current representation.  This efficiently reduces sequent-calculus-based
representations and may improve performance of possible join algorithms.

#### Model checking-based union abstraction

A hardware model checking engine can be used to infer search for optimal
union-only constraints in a QUIC-graph-like structure.  It can be use for
directly implementing join and may serve as a good mechanism for pairing with
other theorem-prover-based techniques, which typically only optimize for
inclusion check.

### Fixed predicate techniques

These techniques are more targeted at specific applications.  They are designed
for use for solving certain kinds of problems and thus they can have data
structures designed for the application-specific predicates.

#### Memcad linear set abstraction

This is a set abstraction built around the idea of linear set constraints such
as:

```
X = {a, b, c} ⊎ Y ⊎ Z
```

It is restricted to support only disjoint union and limited use of subset
constraints.  Join and inclusion check operations are heuristic and tuned for
the intended application.

#### Linear and difference set abstraction

This is an abstraction that extends linear set constraints with difference
constraints to eliminate case splits that may occur when materializing elements
from a set.

#### FixBag (and adaptations)

The FixBag abstraction uses saturation based inclusion check and join based on
a limited set of propagation an simplification rules.  It uses a syntactic
representation of logical formulas as its data structure.

#### Algebraic set abstraction

TBD

## Test language

This package will also define the SDSL, a domain specific language for
manipulating symbolic sets.  The language has the following syntax:

```
k ::= x = e
    | k; k                       // sequencing two commands
    | if ( c ) { k }             // conditional execution
    | if ( c ) { k } else { k }
    | while ( c ) { k }          // looping
    | kill x                     // project out variable
    | assume ( c )               // make an assumption about state
    | assert ( c )               // check property

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

Examples of this language are contained in the `tests` directory.  These are
sample programs that exercise the built-in abstract interpreter.

## SDSL abstract interpreter usage

The included SDSL abstract interpreter is contained in the `sdsl` directory and
the corresponding `SDSL` module.  The static analyzer provides a number of
printing and usability options.

- `-step`  Prints each step of the program as it is interpreted and prints the
  abstract state after each step of the interpretation.
- `-final` Prints only the final abstract state
- `-brace` Prints braces around the abstract states (useful if abstract states
  require multiple to represent)
- `-color <color>` Shows abstract states in color to ease reading.  The
  `<color>` argument supports the following colors `black`, `red`, `green`,
  `yellow`, `blue`, `magenta`, `cyan`, and `white`.  Additionally each color can
  be converted to it's "bright" version by appending it with `!`.  For example,
  `red!` would be bright red.

Options as above can be set on each command line.  However if they should
persist, they can be assigned through an environment variable.  Use the
variable `SDSLPARAMS` as an additional command-line place.  For example to make
step printing and bright red abstract states standard, execute the following
bash command:

```
$ export SDSLPARAMS='-step -color red!'
```

The command line implements a stack-based declarative language for selecting
and combining abstract domains.  By default the stack is empty, so running the
tool without specifying a domain will give an error:

```
$ ./Main.d.byte 
Error: Domain stack is empty, please specify an abstract domain
```

There are two kinds of abstract domains that can be specified: base domains and
domain combinators.  A _base domain_ implements a set abstraction without any
external assistance.  A _domain combinator_ implements a set abstraction based
on one or more other abstractions.  For example, the `-bdd-full` domain is a
base domain that is implemented using binary decision diagrams, whereas the
`-logger` domains is a domain combinator that logs all domain interaction to a
file.

The way the stack is used is through the order of arguments on the command
line.  For example, consider the following command line:

```
$ ./Main.d.byte -bdd-full -bdd-full -logger output.log tests/test1.sdsl
```

The first argument pushes the BDD domain onto the stack; the stack looks like
this:

| Stack  |
|--------|
| BDD(1) |

The second argument pushes a second BDD domain onto the stack; the stack looks
like this:

| Stack  |
|--------|
| BDD(2) |
| BDD(1) |

The third argument pushes the logger combinator onto the stack.  This removes
the top element and adds a logger wrapper to it.  As a result, the stack now
looks like this:

| Stack                     |
|---------------------------|
| Log("output.txt", BDD(2)) |
| BDD(1)                    |

At this point the analyzer would be run with the topmost domain
Log("output.txt", BDD(2)) and would raise a warning for not using all of the
domains on the stack; the domain BDD(1) would remain unused.

## Adding new abstract domains and combinators

The SDSL abstract interpreter (and QUICr) is designed to ease the addition of
new abstract domains and domain combinators.  To add a new domain or
combinator, the code should implement the interface defined in
`Interface.Domain` with integer symbols, `LogicSymbolicSet.t` constraints and
output, and `LogicSymbolicSet.q` queries.  The code should be placed in a new
directory that starts with a lower-case letter (to be properly supported by the
build system).  By convention, the main file for the domain should be called
`Domain.ml`.  A combinator should also be contained in `Domain.ml`, but should
define the functor `Make`.

A `.mlpack` file should be created for the domain.  This should have an
upper-case first letter and will represent the name of the domain or combinator
inside the main file.  Similarly, the _tags file should be updated to both
indicate any dependencies for the new domain as well as adding an appropriate
`for-pack()` line.

Finally, the `Main.ml` file should be modified to allow using the domain.  For
base domains, this involves adding a new line in the `domains` variable.  For
combinators, a line should be added to `combinators`.

For example, if a new domain were called test-domain, the following changes
would be required:

Add the directory `test-domain`.

Add the file `Domain.ml` to `test-domain`.

Add the file `TestDomain.mlpack` to `.` with the following contents:

```
test-domain/Domain
```

Modify the `_tags` file to add the following line:

```
<test-domain/*>: for-pack(TestDomain)
```

Modify the `Main.ml` file to add the following line to the `domains` list:

```
("-test-domain", Arg.Unit (fun () -> push (module TestDomain.Domain)), " Test set domain");
```

Note that there is a blank space before the description.  The first word in the
description documents the argument for the domain (if there is one).
