![Japanese character for *satori*](https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Satori.svg/105px-Satori.svg.png)

---

# Notes

## Types
* Symbols: nullary constructor; `nil`, `t`, `foo`.
* Primitives: unary constructor, can hold information; `0`, `"string"`, pointers.
* Composite: binary constructor, structure, product and sum type; `cons`, `if`.
  
## Special forms
* Functions: `lambda`
* Conditionals: `if`
* Data: `quote`, `quasiquote`
* Projection: `car`, `cdr`

## Is case analysis necessary?
In a language with sum types, is it absolutely necessary to have case analysis,
or could one just have predicates on types?

## Examples
```lisp
(define (length lst)
  (if (null lst)
      0
      (add 1 (length (cdr lst)))))
```
