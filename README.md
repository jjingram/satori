![Japanese character for *satori*](https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Satori.svg/105px-Satori.svg.png)

---

# Structures
A structure can be viewed as either the product or sum of its types. That is,
`'(a . (b . c))` can be viewed as the product `a * b * c` or the sum `a + b + c`
depending on if it's being accessed with projection, `car` and `cdr`, or
analyzed using a `case` expression. This duality comes about as a result of
"cons", or `.`, creating heterogeneous structures.

# Symbols
> In the most trivial implementation, they are essentially named integers (e.g.
> the enumerated type in C).

# Examples
```lisp
(define (length lst)
  (case (x (car lst))
    (nil 0)
    (t (add 1 (length (cdr lst))))))
```
