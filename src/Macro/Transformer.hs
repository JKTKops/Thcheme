{-# LANGUAGE LambdaCase, TupleSections, DuplicateRecordFields #-}
-- I make no guarantees about the efficiency of this implementation.
-- I can't find any papers about implementing these algorithms anywhere,
-- so mainly I just want something that works for now.
module Macro.Transformer 
  ( CompileConfig(..) 
  , compileSyntaxRules
  )
  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Functor (($>))
import Data.List (foldl', transpose)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M

import Val
import Primitives.Vector
import Primitives.Comparison (equalSSH)
import Primitives.String (stringSH)
import EvaluationMonad

data Pattern
  = IdentPat String
  | ConstPat Val
  | WildPat
  | ListPat [Pattern] (Maybe Pattern)
  | EllipsisListPat 
      [Pattern] -- patterns before ellipsis
      Pattern   -- pattern attached to ellipsis
      [Pattern] -- patterns after ellipsis
      (Maybe Pattern) -- optional dot
  | VectorPat [Pattern]
  | EllipsisVectorPat
      [Pattern]
      Pattern
      [Pattern]
  deriving Show

data CompileConfig = CompileConfig { ellipsis :: String, literals :: S.Set String }
data TemCompileConfig = TemCompileConfig { ellipsis :: String, patVars :: S.Set PatternVar }

compilePattern :: CompileConfig -> Val -> EM (Pattern, S.Set PatternVar)
compilePattern CompileConfig{ellipsis = dots, literals = lits} v
  = runStateT (compile v) S.empty
  where
    compile datum = case datum of
      Symbol s
        | s == dots -> lift $ throwError $ 
            Default "unexpected ellipsis in pattern"
        | s == "_" -> pure WildPat
        | s `S.member` lits -> pure $ ConstPat (Symbol s)
        | otherwise -> do
            test <- gets (S.member s)
            if test
              then lift $ throwError $ Default $ 
                "identifier " ++ s ++ " appears multiple times in pattern"
              else modify (S.insert s) $> IdentPat s
        -- todo: if s is in the list of literals, make a ConstPat instead
      datum
        | isConstant datum -> pure $ ConstPat datum
        | stringSH datum   -> pure $ ConstPat datum
        | vectorSH datum   -> compileVectorPat datum
        | pairSH datum     -> compileListPat datum
      badDatum -> throwError $ TypeMismatch "pattern" badDatum

    compileVectorPat vec = vectorElemsPH vec >>= compileElemList (pure . VectorPat)
      where compileElemList k [] = k []
            compileElemList k (v : Symbol maybeDots : rest)
              | maybeDots == dots = do
                  pat <- compile v
                  compileElemList (newK pat) rest
              where newK pat tl = k [] >>= \case
                      VectorPat ps -> pure $ EllipsisVectorPat ps pat tl
                      _ -> lift $ throwError $ 
                        Default "multiple ellipses in vector pattern"
            compileElemList k (v : rest) = do
              vp <- compile v
              compileElemList (k . (vp:)) rest
    
    compileListPat lst = do
        fl <- lift (freezeList lst)
        case fl of
          FList vals -> compileElemList vals ([], Nothing, [], Nothing)
          FDottedList vals dot -> compileElemList vals ([], Nothing, [], Just dot)
          FNotList datum ->
            panic $ "compilePattern.compileListPat: not list " ++ show datum
      where compileElemList [] ~(bf, el, af, dot) = do
              dotp <- case dot of
                Nothing -> pure Nothing
                Just d  -> Just <$> compile d
              return $ case el of
                Nothing  -> ListPat (reverse bf) dotp
                -- when we counter an ellipses, we swap the lists so that we
                -- don't have to write separate loops that do the same thing
                -- so now we have to swap them back.
                Just elp -> EllipsisListPat (reverse af) elp (reverse bf) dotp
            compileElemList (v : Symbol maybeDots : rest) ~(bf, el, _af, dot)
              | maybeDots == dots = case el of
                  Nothing -> do
                    pat <- compile v
                    compileElemList rest ([], Just pat, bf, dot)
                  Just{} -> lift $ throwError $ 
                    Default "multiple ellipses in list pattern"
            compileElemList (v : rest) ~(bf, el, af, dot) = do
              vp <- compile v
              compileElemList rest (vp:bf, el, af, dot)

type PatternVar = String
data Match a = Here a | Deeper [Match a] deriving Show
type Matching = M.HashMap PatternVar (Match Val)

type Matcher     = Val -> EM (Maybe Matching)
type Transcriber = Matching -> EM Val
type Transformer = Val -> EM Val

match :: Pattern -> Matcher
match p v = runMaybeT $ match' p v
  where    
    match' (IdentPat var) val = pure $ M.singleton var (Here val)
    match' (ConstPat v1) v2 = do
      b <- lift $ equalSSH v1 v2
      if b then return M.empty else mzero
    match' WildPat _ = pure M.empty

    match' (ListPat ps Nothing) v = do
      vs <-  MaybeT $ getList v
      guard $ length ps == length vs -- potential performance problem
      matchings <- zipWithM match' ps vs
      return $ M.unions matchings
    match' (ListPat [] (Just pdot)) v = match' pdot v
    match' (ListPat (p:ps) dot) v
      | pairSH v = do
        hd <- carPS v
        tl <- cdrPS v
        M.union <$> match' p hd <*> match' (ListPat ps dot) tl
      | otherwise = mzero
    
    -- See r7rs-small spec page 23, the dotted case here is rather
    -- different than above because of specification that the
    -- /final/ cdr matches P_x.
    match' (EllipsisListPat before e after dot) v = do
      fvs <- lift $ freezeList v
      (vs, dotMatching) <- case (fvs, dot) of
        (FList vs, Nothing) -> pure (vs, M.empty)
        (FList vs, Just pd) -> (vs,) <$> match' pd Nil
        (FDottedList vs vd, Just pd) -> (vs,) <$> match' pd vd
        _ -> guard False $> ([], M.empty) -- the RHS is never used, because
                                          -- the guard always fails; we just
                                          -- need it because of guard's type.
      let n = length vs
      mainMatching <- matchEllipsisPatterns before e after vs n
      return $ mainMatching `M.union` dotMatching
    
    match' (VectorPat ps) v = do
      guard $ vectorSH v
      guard $ length ps == vectorLengthPH v
      vs <- vectorElemsPH v
      matchings <- zipWithM match' ps vs
      return $ M.unions matchings
    
    match' (EllipsisVectorPat b e a) v = do
      guard $ vectorSH v
      vs <- vectorElemsPH v
      let n = vectorLengthPH v
      matchEllipsisPatterns b e a vs n
    
    matchEllipsisPatterns
      :: [Pattern] -- before the ellipsis
      -> Pattern   -- attached to the ellipsis
      -> [Pattern] -- after the ellipsis
      -> [Val] -- (haskell) list of values to match
      -> Int   -- length of vs because we can calculate it faster for vectors
      -> MaybeT EM Matching
    matchEllipsisPatterns b e a vs n = do
      let k = length b
          m = length a
      guard $ n >= k + m
      let (bvs, vs') = splitAt k vs
          (evs, avs) = splitAt (n-k-m) vs'
      matchingBefore <- zipWithM match' b bvs
      matchingEs     <- zipWithM match' (repeat e) evs
      matchingAfter  <- zipWithM match' a avs
      let matchingESingletons = fmap (fmap (:[])) matchingEs
          matchingELists = foldl' (M.unionWith (++)) M.empty matchingESingletons
          finalEMatching = Deeper <$> matchingELists
      return $ M.unions $ finalEMatching : matchingBefore ++ matchingAfter

data Template
  = IdentTemplate String
  | ConstantTemplate Val
  | ListTemplate [Element] (Maybe Template)
  | VectorTemplate [Element]
  deriving (Show)

data Element 
  = Plain Template 
  | WithEllipsis (S.Set PatternVar) Template
    -- set of pattern vars that appear in the template
    -- used to work out if we'll be able to transcribe
    -- as specified.
  deriving Show

compileTemplate :: TemCompileConfig -> Val -> EM Template
compileTemplate TemCompileConfig{ellipsis = dots, patVars = patVars} 
  = fmap fst . runWriterT . compile
  where
    compile :: Val -> WriterT (S.Set PatternVar) EM Template
    compile (Symbol s)
      | s == dots = throwError $ Default "unexpected ellipsis in template"
      | otherwise = do
        tell $ S.intersection (S.singleton s) patVars
        return $ IdentTemplate s
    compile datum
      | isConstant datum = pure (ConstantTemplate datum)
      | stringSH datum   = pure (ConstantTemplate datum)
      | pairSH datum     = compileListTemplate datum
      | vectorSH datum   = compileVectorTemplate datum
    compile badDatum = throwError $ TypeMismatch "template" badDatum

    compileListTemplate p = do
      fl <- lift $ freezeList p
      case fl of
        FList [Symbol a, Symbol b]
          -- TODO: this isn't right: if a is dots, then we should compile b
          -- as though dots had no special meaning. This works only for
          -- the most common case, (... ...) => ...
          | a == dots && b == dots -> pure $ IdentTemplate dots
        FList es         -> ListTemplate <$> compileElemList es <*> pure Nothing
        FDottedList es d -> ListTemplate <$> compileElemList es <*> (Just <$> compile d)
        FNotList _       -> panic "compileListTemplate: not a pair!"
    
    compileVectorTemplate v = do
      vs <- vectorElemsPH v
      elems <- compileElemList vs
      return $ VectorTemplate elems

    compileElemList [] = pure []
    compileElemList (t : Symbol d : rest)
      | d == dots = do
        (template, vars) <- listen $ compile t
        let elem = WithEllipsis vars template
        restElems <- compileElemList rest
        return $ elem : restElems
    compileElemList (t : rest) = do
      elem <- Plain <$> compile t
      restElems <- compileElemList rest
      return $ elem : restElems

{- HLINT ignore "Use section" -}
transcribe :: Template -> Transcriber
transcribe t outerMatching = template t outerMatching
  where
    template :: Template -> Matching -> EM Val
    template (IdentTemplate sym) m = case M.lookup sym m of
      Nothing -> pure $ Symbol sym
      Just (Here v) -> pure v
      Just (Deeper _) -> throwError $ Default $ 
        "symbol " ++ sym ++ " badly nested in macro template"
    template (ConstantTemplate v) _ = pure v
    template (ListTemplate es dt) m = do
      vs <- concat <$> mapM (flip element m) es
      dot <- case dt of
        Nothing -> pure Nil
        Just t  -> template t m
      return $ makeImproperImmutableList vs dot
    template (VectorTemplate es) m = do
      vs <- concat <$> mapM (flip element m) es
      return $ makeVector vs

    element :: Element -> Matching -> EM [Val]
    element (Plain t) m = (:[]) <$> template t m
    element (WithEllipsis varsUsed t) m = do
      let m' = M.filterWithKey (\k _ -> k `S.member` varsUsed) m
          m'' = fmap unwrapDeeper m'
          len = deeperLength $ head $ filter isDeeper $ M.elems m'
          unwrapDeeper (Deeper lst) = lst
          unwrapDeeper (Here v) = replicate len (Here v)
          
          isDeeper Deeper{} = True
          isDeeper Here{} = False
          deeperLength (Deeper xs) = length xs
          deeperLength Here{} = panic "transcribe: impossible case"
      guardAnyDeeper m'
      guardSameLength m''
      let mlst = M.toList m''
          pushDownKeys xs = [ [(i,y) | y <- ys] | (i,ys) <- xs ]
          lstm = map M.fromList $ transpose $ pushDownKeys mlst
      mapM (template t) lstm

    guardAnyDeeper :: Matching -> EM ()
    guardAnyDeeper m
      | good = pure ()
      | otherwise = throwError $ 
        Default "badly nested pattern variable in template"
      where good = any isDeeper m
            isDeeper Deeper{} = True
            isDeeper Here{}   = False

    guardSameLength :: M.HashMap PatternVar [Match Val] -> EM ()
    guardSameLength m
      | good = pure ()
      | otherwise = throwError $
        Default "Different number of matches for a zip-template"
      where vals = M.elems m
            lengths = map length vals
            good = case lengths of
              [] -> True
              ls -> all (== head ls) (tail ls)

{- | Compile one case of a macro transformer.
The syntax of a syntax rule is:
(<pattern> <template>)
where <pattern> is a list whose first element is
the macro keyword or a wildcard.
-}
{- Note: [keywords in patterns]
The first element of that list pattern
is not used in matching. It syntactically matches the keyword either as 
wild or as a literal, but the spec says to ignore it entirely. Therefore,
we remove it from the value-pattern before compiling it. We also remove
the keyword from syntax forms before matching them.

Note that when we apply a transformer, we will apply it to the whole syntax
form and not just to the arguments. This way, if we need to throw a
BadSpecialForm error, we will still see the keyword.
-}
compileSyntaxRule :: CompileConfig -> Val -> EM (Matcher, Transcriber)
compileSyntaxRule config@CompileConfig{ellipsis = dots} v = do
  (vpat, vtemp) <- getSyntaxRule v
  guardPair vpat
  vpat' <- cdrPS vpat -- remove the first element of the pattern
  (pat, patVars) <- compilePattern config vpat'
  template <- compileTemplate (TemCompileConfig dots patVars) vtemp
  return (dropKeywordThenMatch pat, transcribe template)
  where
    getSyntaxRule v = do
      lst <- getListOrError v
      case lst of
        [x,y] -> pure (x,y)
        _ -> throwError $ TypeMismatch "syntax rule" v
    
    guardPair v
      | pairSH v = pure ()
      | otherwise = throwError $ TypeMismatch "non-empty pattern" v
    
    -- if this function is being called, then the value to match is
    -- necessarily a syntax form, hence a nonempty list. We want
    -- to drop the keyword before matching the pattern.
    dropKeywordThenMatch pat = cdrPS >=> match pat

compileSyntaxRules :: CompileConfig -> [Val] -> EM Transformer
compileSyntaxRules config rules = do
  compiledRules <- mapM (compileSyntaxRule config) rules
  return $ makeTransformer compiledRules

makeTransformer :: [(Matcher, Transcriber)] -> Val -> EM Val
makeTransformer [] v = throwError $ BadSpecialForm v
makeTransformer ((m,t):rules) v = do
  mmatch <- m v
  case mmatch of
    Nothing -> makeTransformer rules v
    Just match -> t match