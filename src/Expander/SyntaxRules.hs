module Expander.SyntaxRules (macros) where

import Expander.Transformer (CompileConfig(..), compileSyntaxRules)
import Primitives.Unwrappers (unwrapSymbol)
import Val
import EvaluationMonad

import qualified Data.Set as S

macros :: [(String, Macro)]
macros = [ ("define-syntax", defineSyntax)
         , ("define-syntax-rule", defineSyntaxRule)
         ]

defineSyntax :: Macro
defineSyntax = Macro (Exactly 2) $ const $
  \[Symbol keyword, rules] -> do
    transformer <- syntaxRules rules
    defineVar keyword $ MacroTransformer (Just keyword) transformer

defineSyntaxRule :: Macro
defineSyntaxRule = Macro (Exactly 2) $ const $
  \[pat, template] -> do
    subpats <- getListOrError pat
    keyword <- case subpats of
      [] -> throwError $ TypeMismatch "pattern" pat
      (Symbol name:_) -> pure name
      (notSym:_) -> throwError $ TypeMismatch "symbol" notSym
    transformer <- compileSyntaxRules 
      CompileConfig{ellipsis = "...", literals = S.empty}
      [makeImmutableList [pat, template]]
    defineVar keyword $ MacroTransformer (Just keyword) transformer

syntaxRules :: Val -> EM (Val -> EM Val)
syntaxRules form = do
  mFormList <- getList form
  list <- maybe notSyntaxRules pure mFormList
  case list of
    (Symbol "syntax-rules":_) -> pure ()
    _ -> notSyntaxRules
  (first:rest0) <- case tail list of
    [] -> throwError $ NumArgs (AtLeast 1) []
    xs -> pure xs
  mlits <- getList first
  (dots, vlits, rules) <- case mlits of
    Nothing -> ellipsisForm first rest0
    Just lits -> pure ("...", lits, rest0)
  lits <- mapM unwrapSymbol vlits
  compileSyntaxRules 
    CompileConfig{ellipsis = dots, literals = S.fromList lits}
    rules

  where
    notSyntaxRules = throwError $ TypeMismatch "syntax-rules" form

    ellipsisForm _ [] = throwError $ BadSpecialForm form
    ellipsisForm dots (litsS:rules) =
      (,,) <$> getDots dots <*> getListOrError litsS <*> pure rules

    getDots (Symbol dots) = pure dots
    getDots v = throwError $ TypeMismatch "symbol or list" v
