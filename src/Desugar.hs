module Desugar where

import Syntax

desugar :: Program Name -> Program Name
desugar [] = []
desugar (Define name formals expr:rest) =
  Define name [] (Lambda formals expr) : desugar rest
desugar (top:rest) = top : desugar rest
