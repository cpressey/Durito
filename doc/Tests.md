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

### Lists

Creating some lists of values.

    def main = fun() -> cons(1, cons(2, nil))
    ===> [1, 2]

Some sugar for creating lists.

    def main = fun() -> [add(1, 4), mul(4, 4)]
    ===> [5, 16]

You can, alas, create improper lists.  But there's no sugar for
doing that presently, thankfully.

    def main = fun() -> cons(cons(<<a>>, <<123>>), nil)
    ===> [[<<a>> | <<123>>]]

A weird bit of sugar where, inside a list,
a name then a `=>` then an expression desugars into a
two-element list, with the name quoted.

    def main = fun() -> [a => <<123>>, b => <<456>>]
    ===> [[<<a>>, <<123>>], [<<b>>, <<456>>]]

### `let`

There are `let` blocks.

    def main = fun() -> let r = 1 in add(r, r)
    ===> 2

These work like `let*` in Scheme: they can see previous bindings.

    def main = fun() -> let r = 7, s = mul(r, 2) in add(r, s)
    ===> 21

### Quoted forms

You can use the `<<...>>` construct to create a quoted form.
This is a kind of value that represents an expression in a
Durito program text.

    def main = fun() -> <<add(2, 3)>>
    ===> <<add(2, 3)>>

Name bindings aren't expanded in a quoted form.

    def main = fun() -> <<fun() -> let r = 2 in add(r, r)>>
    ===> <<fun() -> let r = 2 in add(r, r)>>

However, local name bindings are _captured_ when making a
quoted form.  There is nothing special internally representing
these captured bindings; rather, they are represented by
adding a `let` around the quoted form.

    def main = fun() -> let r = 2 in <<fun() -> add(r, r)>>
    ===> <<let r = 2 in fun() -> add(r, r)>>

This applies to formal parameter capture too.

    def main = fun() -> aaaa(5, 6)
    def aaaa = fun(x, y) -> <<add(x, y)>>
    ===> <<let x = 5, y = 6 in add(x, y)>>

In order for this to work coherently, we need to make sure that
every denoted value can be denoted by some expression, and we need
to be able to generate that expression for any given denoted value.

So, for example, lists are denoted values, but they aren't
directly expressed as literals.  So we must express them (as
a series of applications of builtins that create them) when
putting them in a quoted form:

    def main = fun() -> aaaa([1, 2, 3])
    def aaaa = fun(xs) -> <<add(xs, 1)>>
    ===> <<let xs = cons(1, cons(2, cons(3, []))) in add(xs, 1)>>

This is true too for function values, and it is true even when
those functions have closed over local names.  This closure is
expressed as a `let` block around the function value.

    def main = fun() -> aaaa(73)
    def aaaa = fun(g) -> bbbb(fun(x) -> add(g, x))
    def bbbb = fun(f) -> <<add(f, 1)>>
    ===> <<let f = let g = 73 in fun(x) -> add(g, x) in add(f, 1)>>

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

However, whenever a quoted form is created from a quoted
expression in the program, the current environment in effect
is closed over by it, by wrapping the quoted form in a `let` block.
See the [Quoted forms](#quoted-forms) section above for details.

    def yarf = fun(a) -> <<add(a, 99)>>
    def main = fun() -> yarf(123)
    ===> <<let a = 123 in add(a, 99)>>

The effect is for the enclosing lexical bindings to be in place
when the quoted form is evaluated.

    def yarf = fun(a) -> <<add(a, 99)>>
    def main = fun() -> eval(yarf(123))
    ===> 222

Using this, we may introduce local variables during `eval`,
but only where they are not overridden (shadowed) by local
variables that were set up when creating the quoted form
being `eval`ed.

    def double = fun(n) -> mul(2, n)
    def quoted = fun(w, h) -> <<double(add(w, h))>>
    def perim = fun(w, h) ->
        eval(quoted(double(w), double(h)))
    def main = fun() -> perim(6,17)
    ===> 92

If we wanted to remove even these bindings, we could do so
simply by manipulating the quoted form and removing the
enclosing `let` block.  Durito currently provides no facilities
to do this, but it easily could.  (Watch this space, I guess.)

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

Applications of builtins residuate by evaluation when all of their arguments are known.

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
    def main = fun(y) -> subst([y => add(y, id(2))], <<add(y, id(2))>>)
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
