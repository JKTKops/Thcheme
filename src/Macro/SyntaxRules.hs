module Macro.SyntaxRules (macros) where

import Macro.Transformer (CompileConfig(..), compileSyntaxRules)
import Primitives.Unwrappers (unwrapSymbol)
import Val
import EvaluationMonad

import qualified Data.Set as S

macros :: [(String, Macro)]
macros = [("define-syntax", defineSyntax)]

defineSyntax :: Macro
defineSyntax = Macro (Exactly 2) $ const $
  \[Symbol keyword, rules] -> do
    transformer <- syntaxRules rules
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
