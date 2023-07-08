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
to macros.

Hygiene is violated when the identifiers in a macro (or in an application
of a macro) are bound to values that the macro writer (or macro user)
didn't expect them to be bound to.

Durito's `eval` always evaluates the given quoted program form in
an environment that contains only builtins and globals (top-level
identifiers).  So, for example,

    def double = fun(n) -> mul(2, n)
    def main = fun() -> eval(<<double(mul(2, 2))>>)

evaluates to 8, while

    def double = fun(n) -> eval(<<mul(2, n)>>)
    def main = fun() -> double(mul(2,2))

will produce a runtime error when `eval` attempts to evaluate the
symbol `n`, which has no binding.

To accomplish in Durito what the second version of `double` is
presumably intending to accomplish -- that is, to subvert the
base hygiene expectations and allow other bindings during `eval` --
the idiomatic approach is to _modify the quoted form_ before
passing it to `eval`.  To this end, Durito provides a builtin
called `subst`, which substitutes parts of a quoted form.  In this
case, we can use it to replace the quoted occurrences of `n`
with the value of `n` in the evaluating environment:

    def double = fun(n) ->
        eval(
            subst(
                [[<<n>>, n]],
                <<mul(2, n)>>
            )
        )
    def main = fun() -> double(mul(2,2))

#### Substitution

A note about substitution (that wasn't apparent to me when I started
out on this): this method of substitution really works best when the
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

So trying to place a closure value back into a quoted form
with `subst` raises the question of how to handle it.

There are two reasonable options: error out, i.e. only allow denoted
values which do have exactly corresponding expressed values to be
put into quoted forms; or concoct an expression which will, when
evaluated, evaluate to the denoted value.  In the case of function
closures, this might involve generating a `let` block around the
function literal (or something similar).

How Durito will handle this is still work-in-progress.

### TODO

*   arguments to `main()` (otherwise the whole exercise is kind of trivial -- every program can be residuated to a value!)
*   tests for inserting lists, function closures, etc. into quoted forms
*   `@macro` sugar that looks like Julia macros, arguments are automatically quoted
    (and locals are automatically subst'ed in?)
*   "ancillary syntax" that doesn't mean anything by itself but can be used for macros
    (e.g. `with a = e1, b = e2 in e3` -- as a generic structure that evaluates to a
    pair of a list of (name, expr) pairs and an expression -- to be used in `let` or
    `subst` or other macros)

[Ahead-of-Time `eval`]: https://github.com/cpressey/Ahead-of-Time-eval
[constant folding]: https://en.wikipedia.org/wiki/Constant_folding
