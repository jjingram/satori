![Japanese character for *satori*](https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Satori.svg/105px-Satori.svg.png)

---

# Features

## Pattern matching
Satori features pattern matching on arbitrary structures. The most basic type of
pattern are symbol patterns; that is a symbol evaluates to itself. Some examples
of symbols are `nil`, `t`, and `foo`, with `nil` and `t` being special symbols
in that `nil` matches against nothing but itself and `t` matches against
anything. A simple example of this is the following in which we compute the
length of a list:

```lisp
(define (length lst)
  (case (car lst)
    (nil 0)
    (t (add 1 (length (cdr lst))))))
```

An example that uses symbols as tags for further processing of other components
of the list:

```lisp
(define (eval x)
  (case (first lst)
    (lambda (let ((params (second lst))
                  (body (rest (rest lst))))
              (eval-lambda params body)))
    (t (let ((f (first lst))
             (args (rest lst)))
         (eval-app f args)))))))
```

The next sort of pattern are constant patterns. These match against a quoted
literal such as integers or strings. The following computes the nth factorial
using pattern matching:

```lisp
(define (fact n)
  (case n
    (0 1)
    (t (mul n (fact (sub n 1))))))
```

Now we get to the powerful part of pattern matching, matching against
structures. In Satori two different types of structures can be matched against:
cons and lists. Matching against structures is special because it allows you to
bind variables to different parts of the structure, introduced with the `,`
operator. The following is an example of adding the `car` and `cdr` of a cons
cell:

```lisp
(define (add-cons x)
  (case x
    ((,lhs . ,rhs) (add lhs rhs))))
```

As previously mentioned we can also match against lists. The following is an
example of that:

```lisp
(define (add-list x)
  (case x
    ((,lhs ,rhs) (add lhs rhs))))
```

A powerful pattern is to match against the rest of a list which can be
demonstrated by refactoring our `eval` example with what we've learned so far:

```lisp
(define (eval x)
  (case x
    ((lambda ,params ,@body) (eval-lambda params body))
    ((,f ,@args) (eval-app f args))))
```

As you can see, variables must be unquoted because quoted literals evaluate to
themselves. In this example we've left the symbol `lambda` quoted because we
wanted to match against the symbol `lambda` at the start of the list.

Finally we have the wildcard pattern which matches against anything but doesn't bind any variables. We could introduce a projection function using the wildcard pattern:

```lisp
(define (second x)
  (case x
    (`(_ . (snd . _)) snd)))
```

In general we have the following constructs for pattern matching:

* Special symbols 
* Quoted patterns like `0`, `foo` and `(1 . 2)`
* Quasiquoted patterns where literals evaluate to themself and variable bindings
  can be introduced with `,` and `,@` (`,` matches a single item while `,@`
  matches the rest of a list)

## Symbols
> In the most trivial implementation, they are essentially named integers (e.g.
> the enumerated type in C).
