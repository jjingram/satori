import Test.HUnit

import qualified Parser as P
import Syntax

main :: IO Counts
main = do
  runTestTT expressions
  runTestTT definitions

expressions =
  TestList
    [ symbol
    , integer
    , pair
    , list
    , improperList
    , qqSymbol
    , qqPair
    , qqList
    , qqImproperList
    , variable
    , call
    ]

symbol =
  TestCase
    (assertEqual
       "symbol"
       (Right (Quote (Atom (Symbol "foo"))))
       (P.parseExpression "'foo"))

integer =
  TestCase
    (assertEqual
       "integer"
       (Right (Quote (Atom (Integer 1))))
       (P.parseExpression "1"))

pair =
  TestCase
    (assertEqual
       "pair"
       (Right (Quote (Cons (Atom (Symbol "a")) (Atom (Symbol "b")))))
       (P.parseExpression "'(a . b)"))

list =
  TestCase
    (assertEqual
       "list"
       (Right
          (Quote
             (Cons
                (Atom (Symbol "a"))
                (Cons (Atom (Symbol "b")) (Cons (Atom (Symbol "c")) (Atom Nil))))))
       (P.parseExpression "'(a b c)"))

improperList =
  TestCase
    (assertEqual
       "improper list"
       (Right
          (Quote
             (Cons
                (Atom (Symbol "a"))
                (Cons (Atom (Symbol "b")) (Atom (Symbol "c"))))))
       (P.parseExpression "'(a b . c)"))

qqSymbol =
  TestCase
    (assertEqual
       "quasiquote symbol"
       (Right (Quasiquote (QuasiAtom (Symbol "foo"))))
       (P.parseExpression "`foo"))

qqPair =
  TestCase
    (assertEqual
       "quasiquote pair"
       (Right
          (Quasiquote
             (QuasiCons (QuasiAtom (Symbol "a")) (QuasiAtom (Symbol "b")))))
       (P.parseExpression "`(a . b)"))

qqList =
  TestCase
    (assertEqual
       "quasiquote list"
       (Right
          (Quasiquote
             (QuasiCons
                (QuasiAtom (Symbol "a"))
                (QuasiCons
                   (QuasiAtom (Symbol "b"))
                   (QuasiCons (QuasiAtom (Symbol "c")) (QuasiAtom Nil))))))
       (P.parseExpression "`(a b c)"))

qqImproperList =
  TestCase
    (assertEqual
       "quasiquote improper list"
       (Right
          (Quasiquote
             (QuasiCons
                (QuasiAtom (Symbol "a"))
                (QuasiCons (QuasiAtom (Symbol "b")) (QuasiAtom (Symbol "c"))))))
       (P.parseExpression "`(a b . c)"))

variable =
  TestCase
    (assertEqual "variable" (Right (Variable "x")) (P.parseExpression "x"))

call =
  TestCase
    (assertEqual
       "call"
       (Right (Call (Variable "x") [Quote (Atom (Integer 1))]))
       (P.parseExpression "(x 1)"))

definitions = TestList [define, declare, program]

define =
  TestCase
    (assertEqual
       "define"
       (Right [Define "foo" ["x"] (Variable "x")])
       (P.parseModule "<stdin>" "(define (foo x) x)"))

declare =
  TestCase
    (assertEqual
       "declare"
       (Right [Declare "foo" ["x"]])
       (P.parseModule "<stdin>" "(declare (foo x))"))

program =
  TestCase
    (assertEqual
       "program"
       (Right
          [ Define "foo" ["x"] (Variable "x")
          , Command (Call (Variable "foo") [Quote (Atom (Integer 1))])
          ])
       (P.parseModule "<stdin>" "(define (foo x) x) (foo 1)"))
