Durito
======

**Durito** is a work-in-progress toy purely-functional language
that implements [Ahead-of-Time `eval`][], which in rough terms
means that

*   anything that *can* be evaluated ahead-of-time, *is*;
*   the language supports quoted forms and `eval`; and
*   `eval` can be evaluated ahead-of-time too.

The combination of these three features replaces most uses
of hygienic macros, with some additional benefits.
See the [Ahead-of-Time `eval`][] article for more information.

### Quick Start

Install Haskell, clone this repository, `cd` into its folder,
run `build.sh`, then run `test.sh` to run the test suite.

See [doc/Tests.md](doc/Tests.md) for the content of the test
suite.  The test cases should serve to illustrate most of the
points of the language.  Example Durito programs can also
be found in the [eg/](eg/) folder.

### Theory of Operation

To accomplish AoT-eval, Durito defines an operation, _residuate_,
which takes a program structure (that is, a parsed program text) and
returns a program structure where every part of the program that
can be evaluated ahead-of-time (that is, without knowing the input
to the program) has been evaluated and replaced in the program
with the value that it evaluated to.  (cf. [constant folding][],
except this is not just a compiler optimization -- it is mandated
by the language.)

The result of applying the residuate operation on a program structure
P is called the _residual_ of P.  The residual of P has semantics
equivlaent to P itself.  That is, evaluating the residual of P has
the same observable result as evaluating P itself.

#### Termination

Since Durito is Turing-complete, it is not possible to determine in
all cases whether the evaluation of a part of a Durito program will
terminate or not.  This is true regardless of whether the evaluation
happens ahead-of-time or during runtime.

The Ahead-of-Time `eval` concept is not prescriptive on this point.  It
leaves it up to the implementation to deal with this issue as it sees fit.

Durito deals with the issue by not dealing with it, so to speak:
if evaluating some part of the program ahead of time fails to
terminate, then it fails to terminate ahead of time, that's all.
Durito regards this situation as no worse than the same code failing to
terminate at runtime.

#### Hygiene

Functions that are evaluated ahead of time are morally equivalent
to macros.  Or rather, macros are functions _from syntax to syntax_,
that are evaluated ahead of time.

Hygiene is violated when the identifiers in a macro (or in an application
of a macro) are bound to values that the macro writer (or macro user)
didn't expect them to be bound to.

Durito's `eval` always evaluates the given quoted program form in
an environment that contains only builtins and globals (top-level
identifiers).  _However_, when _creating_ a quoted form in the usual
way, it _includes_ any local bindings as a `let` block inside the
quoted form.  So when `eval`ing a quoted form, the local bindings
are in effect, which is what one usually expects.

If one wished to _change_ this, however, to subvert the
base hygiene expectations, to either have those local bindings
be unavailable, or to inject other bindings entirely, during `eval` --
the idiomatic approach is to _modify the quoted form_ before
passing it to `eval`.

To this end, Durito should eventually provide some tools for
manipulating quoted forms.  It doesn't, at present, because at
present it is still a proof-of-concept.

#### Expressed and Denoted Values

A note about modifying quoted forms that wasn't apparent to me when
I started out on this: it really works best when the
set of _denoted values_ of the language closely matches the set of
_expressed values_.  That is, it's best if, for every kind of value
the program works with at runtime, there is a corresponding
syntactic form that represents it exactly.

There are often some denoted values that don't have literal syntax
for them.  A notable one is function values that have closed over
some variables in their environment.  (Often called "closures").
The literal function syntax doesn't include the enclosing environment
that the closure has captured (as it is inferred from the execution
context when the function value is created at runtime.)

The general strategy is to concoct an expression which will, when
evaluated, evaluate to the denoted value.  So, when placing a closure
into a quoted form, we do this: we generate a `let` block around the
function literal that provides the captured bindings of the closure.

### TODO

*   arguments to `main()` (otherwise the whole exercise is kind of trivial -- every program can be residuated to a value!)
*   `@macro` sugar that looks like Julia macros, where arguments are automatically quoted
*   "ancillary syntax" that doesn't mean anything by itself but can be used for macros
    (e.g. `with a = e1, b = e2 in e3` -- as a generic structure that evaluates to a
    pair of a list of (name, expr) pairs and an expression -- to be used in `let` or
    other macros)

[Ahead-of-Time `eval`]: https://github.com/cpressey/Ahead-of-Time-eval
[constant folding]: https://en.wikipedia.org/wiki/Constant_folding
