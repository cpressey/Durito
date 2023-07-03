Durito Tests
============

Eval
----

    -> Functionality "Evaluate Durito Program" is implemented by
    -> shell command "bin/durito eval %(test-body-file)"

    -> Tests for functionality "Evaluate Durito Program"

Applying functions

    def double = fun(n) -> mul(2, n)
    def perim = fun(w, h) -> double(add(w, h))
    def main = fun() -> perim(12,34)
    ===> 92

The language does not use dynamic scope

    def main = fun() -> aaaa(1, 2)
    def aaaa = fun(p, q) -> bbbb(99)
    def bbbb = fun(r) -> add(p, q)
    ???> undefined name p

The language does use lexical scope

    def main = fun() -> aaaa(1)
    def make = fun(p) -> fun(q) -> add(p, q)
    def aaaa = fun(x) -> (make(x))(99)
    ===> 100

The language implements eval (for now)

    def double = fun(n) -> mul(2, n)
    def perim = fun(w, h) -> eval <<double(add(w, h))>>
    def main = fun() -> perim(12,34)
    ===> 92

This makes it more obvious when symbols in quoted forms are bound

    def double = fun(n) -> mul(2, n)
    def quoted = fun() -> <<double(add(w, h))>>
    def perim = fun(w, h) -> eval quoted()
    def main = fun() -> perim(12,34)
    ===> 92

A program may evaluate to a function value.

    def main = fun() -> fun(r) -> add(1, r)
    ===> fun(r) -> add(1, r)

Functions can be passed to functions.

    def yark = fun(x, double) -> double(x)
    def main = fun() -> yark(53, fun(z) -> mul(z, 2))
    ===> 106

Residuation
-----------

    -> Functionality "Residuate Durito Program `main` Function" is implemented by
    -> shell command "bin/durito residuate-main %(test-body-file)"

    -> Tests for functionality "Residuate Durito Program `main` Function"

Literals.

    def main = fun() -> 123
    ===> 123

    def main = fun() -> <<add(123, r)>>
    ===> <<add(123, r)>>

    def main = fun() -> fun(r) -> add(1, r)
    ===> fun(r) -> add(1, r)

Known names.

    def num = 123
    def main = fun() -> num
    ===> 123

Builtins.

    def main = fun() -> add(123, 7)
    ===> 130

Can't residuate an application if any of the actuals are not known.

    def main = fun(r) -> add(123, r)
    ===> add(123, r)

Application of named global functions.  If a function was defined globally,
then it must be evaluatable ahead-of-time, for it cannot close over any values
which might be unknown.

    def double = fun(x) -> mul(x, 2)
    def main = fun() -> double(53)
    ===> 106

Can't residuate an application if the function is not known to be closing
over no values.  Currently we assume that if the function was defined
in any environment at all, it might close over some values.  This is somewhat
pessimistic, but will serve us to start.

    def yark = fun(x, double) -> double(x)
    def main = fun(r) -> yark(53, fun(z) -> mul(z, 2))
    ===> yark(53, fun(z) -> mul(z, 2))

Residuate over `eval`.

    def main = fun() -> eval <<add(123, 456)>>
    ===> 579
