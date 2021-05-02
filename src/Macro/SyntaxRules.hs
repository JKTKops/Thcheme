module Macro.SyntaxRules (macros) where

import Macro.Transformer (compileSyntaxRules)
import Val
import EvaluationMonad

macros :: [(String, Macro)]
macros = [("define-case-macro", defineCaseMacro)]

defineCaseMacro :: Macro
defineCaseMacro = Macro (AtLeast 2) $ const $
  \(Symbol keyword : rules) -> do
    transformer <- compileSyntaxRules rules
    defineVar keyword $ MacroTransformer keyword transformer