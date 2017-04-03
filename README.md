![Japanese character for *satori*](https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Satori.svg/105px-Satori.svg.png)

---

# Examples
```lisp
(define (length lst)
  (case lst
    (nil 0)
    (cons (add 1 (length (cdr lst))))))
```
