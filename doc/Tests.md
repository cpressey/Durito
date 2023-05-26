Diodorus Tests
==============

Eval
----

    -> Functionality "Evaluate Diodorus Program" is implemented by
    -> shell command "bin/diodorus eval %(test-body-file)"

    -> Tests for functionality "Evaluate Diodorus Program"

Applying functions

    def double = fun(n) -> mul(2, n)
    def perim = fun(w, h) -> double(add(w, h))
    def main = fun() -> perim(12,34)
    ===> Int 92

The language does not use dynamic scope

    def main = fun() -> aaaa(1, 2)
    def aaaa = fun(p, q) -> bbbb(99)
    def bbbb = fun(r) -> add(p, q)
    ???> undefined name p

The language does use lexical scope

    def main = fun() -> aaaa(1)
    def make = fun(p) -> fun(q) -> add(p, q)
    def aaaa = fun(x) -> (make(x))(99)
    ===> Int 100

The language implements eval (for now)

    def double = fun(n) -> mul(2, n)
    def perim = fun(w, h) -> eval <<double(add(w, h))>>
    def main = fun() -> perim(12,34)
    ===> Int 92

This makes it more obvious when symbols in quoted forms are bound

    def double = fun(n) -> mul(2, n)
    def quoted = fun() -> <<double(add(w, h))>>
    def perim = fun(w, h) -> eval quoted()
    def main = fun() -> perim(12,34)
    ===> Int 92

Residuation
-----------

    -> Functionality "Residuate Diodorus Program `main` Function" is implemented by
    -> shell command "bin/diodorus residuate-main %(test-body-file)"

    -> Tests for functionality "Residuate Diodorus Program `main` Function"

Literals.

    def main = fun() -> 123
    ===> 123

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
