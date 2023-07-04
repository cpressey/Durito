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
to the program) has been evaluated, and that part of the program
has been replaced with the value that it evaluated to.
(cf. [constant folding][], except this is not just a compiler
optimization -- it is mandated by the language.)

The result of applying the residuate operation on a program structure
P is called the _residual_ of P.  The residual of P has semantics
equivlaent to P itself.  That is, evaluating the residual of P has
the same observable result as evaluating P itself.

#### Termination

Since Durito is Turing-complete, it is not possible to determine in
all cases whether the evaluation of a part of a Durito program will
terminate or not.  This is true regardless of whether the evaluation
happens ahead-of-time or during runtime.  Ahead-of-Time `eval` leaves
it up to the implementation to deal with this issue as it sees fit.

Durito deals with the issue by not dealing with it, so to speak:
if evaluating some part of the program ahead of time fails to
terminate, then it fails to terminate ahead of time, that's all.
Durito regards this situation as no worse than the same code failing to
terminate at runtime.

#### Hygiene

Functions that are evaluated ahead of time are morally equivalent
to macros.

Hygiene is violated when the identifiers in a macro are bound to
values that the macro writer didn't expect them to be bound to.

Durito's `eval` always evaluates the given quoted program form in
an environment that contains only builtins and globals (top-level
identifiers).  So, for example,

    def double = fun(n) -> mul(2, n)
    def eight = fun() -> eval <<double(mul(2, 2))>>

residuates to

    def eight = 8

while

    def double = fun(n) -> eval <<mul(2, n)>>

will produce a runtime error when `eval` attempts to evaluate the
symbol `n`, which has no binding.

To accomplish in Durito what the second version of `double` is
presumably intending to accomplish -- that is, to subvert the
base hygiene expectations and allow other bindings during `eval` --
the idiomatic approach is to _modify the quoted form_ before
passing it to `eval`.

To this end, Durito provides a language construct called `subst`,
which looks a little like `let`.  Unlike `let`, it operates
directly on quoted forms.  In a give quoted form, it replaces
all occurrences of given names with given values.

There is no good reason this needs to be a built-in language
construct; if Durito provided more general facilities for
manipulating quoted forms, it could be built from them instead.
But having it as its own `let`-like language construct is
convenient and allows us to demonstrate how to subvert hygiene
with it.  Namely,

    def double = fun(n) ->
        eval subst n -> n in <<mul(2, n)>>

[Ahead-of-Time `eval`]: https://github.com/cpressey/Ahead-of-Time-eval
[constant folding]: https://en.wikipedia.org/wiki/Constant_folding
