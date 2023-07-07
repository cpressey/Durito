Durito Tests
============

Evaluation of Durito programs
-----------------------------

    -> Functionality "Evaluate Durito Program" is implemented by
    -> shell command "bin/durito eval %(test-body-file)"

    -> Tests for functionality "Evaluate Durito Program"

Applying functions.

    def double = fun(n) -> mul(2, n)
    def perim = fun(w, h) -> double(add(w, h))
    def main = fun() -> perim(12,34)
    ===> 92

The language does not use dynamic scope.

    def main = fun() -> aaaa(1, 2)
    def aaaa = fun(p, q) -> bbbb(99)
    def bbbb = fun(r) -> add(p, q)
    ???> undefined name p

The language does use lexical scope.

    def main = fun() -> aaaa(1)
    def make = fun(p) -> fun(q) -> add(p, q)
    def aaaa = fun(x) -> (make(x))(99)
    ===> 100

A program may evaluate to a function value.

    def main = fun() -> fun(r) -> add(1, r)
    ===> fun(r) -> add(1, r)

Functions can be passed to functions.

    def yark = fun(x, double) -> double(x)
    def main = fun() -> yark(53, fun(z) -> mul(z, 2))
    ===> 106

Creating some lists of values.

    def main = fun() -> cons(1, cons(2, nil))
    ===> [1, 2]

    def main = fun() -> [add(1, 4), mul(4, 4)]
    ===> [5, 16]

    def main = fun() -> cons(cons(<<a>>, <<123>>), nil)
    ===> [[<<a>> | <<123>>]]

    def main = fun() -> [<<a>> => <<123>>, <<b>> => <<456>>]
    ===> [[<<a>>, <<123>>], [<<b>>, <<456>>]]

### Evaluation of `eval`

The language implements `eval` at runtime (at least, for now).

    def double = fun(n) -> mul(2, n)
    def main = fun() -> eval(<<double(add(12, 34))>>)
    ===> 92

The argument to `eval` must be a quoted form.

    def double = fun(n) -> mul(2, n)
    def scram = fun(x) -> double(eval(x))
    def main = fun() -> scram(99)
    ???> type

The environment in which the `eval` takes place consists
only of the names of builtins and of global (top-level)
identifiers.  Any other names, such as local variables
or formal arguments, will not be bound.

    def double = fun(n) -> mul(2, n)
    def quoted = fun() -> <<double(add(w, h))>>
    def perim = fun(w, h) -> eval(quoted())
    def main = fun() -> perim(12,34)
    ???> undefined name

There is, at present, no way to provide an `eval` with an environment
other than described above.  To introduce specific bindings, the idiom
in Durito is to perform syntactic replacement on the quoted form
before calling `eval` on it.  To this end, Durito could (and arguably
should) provide a sophisticated set of tools for constructing and
manipulating quoted forms.  Alas, at present, it does not.  It provides
only a `subst` builtin, which substitutes names in a quoted form with
values.  However, this suffices for a lot of cases.

    def yarf = fun() -> subst([<<a>> => <<123>>], <<add(a, 99)>>)
    def main = fun() -> eval(yarf())
    ===> 222

Using this, we may introduce local variables explicitly during `eval`.
Compare this to the "undefined name" example above.

    def double = fun(n) -> mul(2, n)
    def quoted = fun() -> <<double(add(w, h))>>
    def perim = fun(w, h) ->
        eval(subst([<<w>> => w, <<h>> => h], quoted()))
    def main = fun() -> perim(12,34)
    ===> 92

Residuation
-----------

See the README for the definition of the residuate operation.  Roughly
speaking, it evaluates all the parts of the program that can be evaluated
without knowing the precise input to the program.

    -> Functionality "Residuate Durito Program" is implemented by
    -> shell command "bin/durito residuate %(test-body-file)"

    -> Tests for functionality "Residuate Durito Program"

Literals residuate to themselves.

    def main = fun() -> 123
    ===> def main = fun() -> 123

    def main = fun() -> <<add(123, r)>>
    ===> def main = fun() -> <<add(123, r)>>

    def main = fun() -> fun(r) -> add(1, r)
    ===> def main = fun() -> fun(r) -> add(1, r)

Known names residuate to the value they are known to be bound to.

    def num = 123
    def main = fun() -> num
    ===> def num = 123
    ===> def main = fun() -> 123

Applications of builting residuate by evaluation when all of their arguments are known.

    def main = fun() -> add(123, 7)
    ===> def main = fun() -> 130

Can't residuate an application if any of the arguments are not known.

    def main = fun(r) -> add(123, r)
    ===> def main = fun(r) -> add(123, r)

Application of named global functions.  If a function was defined globally,
then it must be evaluatable ahead-of-time, for it cannot close over any values
which might be unknown.

    def double = fun(x) -> mul(x, 2)
    def main = fun() -> double(53)
    ===> def double = fun(x) -> mul(x, 2)
    ===> def main = fun() -> 106

We can residuate a literal function if it closes over no variables.

    def yark = fun(x, double) -> double(x)
    def main = fun(r) -> yark(53, fun(z) -> mul(z, 2))
    ===> def yark = fun(x, double) -> double(x)
    ===> def main = fun(r) -> 106

Residuate over `eval`.

    def main = fun() -> eval(<<add(123, 456)>>)
    ===> def main = fun() -> 579

Some miscellaneous old test cases.

    def pi = 3
    def main = fun() -> add(2, mul(pi, 5))
    ===> def pi = 3
    ===> def main = fun() -> 17

    def main = fun() -> (fun(x) -> mul(x, x))(4)
    ===> def main = fun() -> 16

    def pi = 3
    def main = fun() -> (fun(x) -> mul(x, add(2, pi)))(4)
    ===> def pi = 3
    ===> def main = fun() -> 20

    def main = fun() -> <<a(b(c), d)>>
    ===> def main = fun() -> <<a(b(c), d)>>

    def main = fun() -> eval(<<add(3, 5)>>)
    ===> def main = fun() -> 8

Partial residuation.

    def pi = 3
    def main = fun(x) -> mul(x, add(2, pi))
    ===> def pi = 3
    ===> def main = fun(x) -> mul(x, 5)

Partial residuation inside literal functions.

    def pi = 3
    def main = fun(y) -> (fun(x) -> mul(x, add(2, pi)))(y)
    ===> def pi = 3
    ===> def main = fun(y) -> (fun(x) -> mul(x, 5))(y)

Partial residuation inside `eval`.

    def id = fun(x) -> x
    def main = fun(y) -> eval(add(y, id(<<add(2, 3)>>)))
    ===> def id = fun(x) -> x
    ===> def main = fun(y) -> eval(add(y, <<add(2, 3)>>))

Partial residuation inside `subst` (both body and bindings).

    def id = fun(x) -> x
    def main = fun(y) -> subst([<<y>> => add(y, id(2))], <<add(y, id(2))>>)
    ===> def id = fun(x) -> x
    ===> def main = fun(y) -> subst(cons(cons(<<y>>, cons(add(y, 2), [])), []), <<add(y, id(2))>>)

We (currently) residuate functions out of existence *only* if they
close over no variables.

    def main = fun() -> (fun(x) -> add(mul(x, 2), 1))(5)
    ===> def main = fun() -> 11

We do not residuate functions out of existence when all of their
arguments are known but they close over values (which might not be known).

This can be checked by checking if the function has any free variables,
and if so, it is considered unknown.

    def main = fun(y) -> (fun(x) -> add(mul(x, 2), y))(5)
    ===> def main = fun(y) -> (fun(x) -> add(mul(x, 2), y))(5)

TODO: test shadowing edge case
