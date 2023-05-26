Diodorus Tests
==============

    -> Functionality "Evaluate Diodorus Program" is implemented by
    -> shell command "bin/diodorus eval %(test-body-file)"

    -> Tests for functionality "Evaluate Diodorus Program"

Applying functions

    def double = fun(n) -> mul(2, n)
    def perim = fun(w, h) -> double(add(w, h))
    def main = fun() -> perim(12,34)
    ===> Int 92

Dynamic scope (FIXME: should fail)

    def main = fun() -> aaaa(1, 2)
    def aaaa = fun(p, q) -> bbbb(99)
    def bbbb = fun(r) -> add(p, q)
    ===> Int 3

Lexical scope

    def main = fun() -> aaaa(1)
    def aaaa = fun(p) -> (fun(q) -> add(p, q))(99)
    ===> Int 100
