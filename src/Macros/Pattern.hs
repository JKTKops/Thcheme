{-# LANGUAGE LambdaCase, TupleSections #-}
-- I make no guarantees about the efficiency of this implementation.
-- I can't find any papers about implementing these algorithms anywhere,
-- so mainly I just want something that works for now.
module Macros.Pattern where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Functor (($>))
import Data.List (foldl', transpose)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M

import Val
import Primitives.Vector
import Primitives.Comparison (equalSSH)
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

data CompileConfig = CompileConfig
  { ellipsis :: String }

compilePattern :: CompileConfig -> Val -> EM Pattern
compilePattern CompileConfig{ellipsis = dots} v = evalStateT (compile v) S.empty
  where
    compile datum = case datum of
      Symbol s
        | s == dots -> lift $ throwError $ 
            Default "unexpected ellipsis in pattern"
        | s == "_" -> pure WildPat
        | otherwise -> do
            test <- gets (S.member s)
            if test
              then lift $ throwError $ Default $ 
                "identifier " ++ s ++ " appears multiple times in pattern"
              else modify (S.insert s) $> IdentPat s
        -- todo: if s is in the list of literals, make a ConstPat instead
      datum
        | isConstant datum -> pure $ ConstPat datum
        | vectorSH datum   -> compileVectorPat datum
        | pairSH datum     -> compileListPat datum
      badDatum -> throwError $ TypeMismatch "pattern" badDatum

    compileVectorPat vec = vectorElemsPH vec >>= go (pure . VectorPat)
      where go k [] = k []
            go k (v : Symbol maybeDots : rest)
              | maybeDots == dots = do
                  pat <- compile v
                  go (newK pat) rest
              where newK pat tl = k [] >>= \case
                      VectorPat ps -> pure $ EllipsisVectorPat ps pat tl
                      _ -> lift $ throwError $ 
                        Default "multiple ellipses in vector pattern"
            go k (v : rest) = do
              vp <- compile v
              go (k . (vp:)) rest
    
    compileListPat lst = do
        fl <- lift (freezeList lst)
        case fl of
          FList vals -> go vals ([], Nothing, [], Nothing)
          FDottedList vals dot -> go vals ([], Nothing, [], Just dot)
          FNotList datum ->
            panic $ "compilePattern.compileListPat: not list " ++ show datum
      where go [] ~(bf, el, af, dot) = do
              dotp <- case dot of
                Nothing -> pure Nothing
                Just d  -> Just <$> compile d
              return $ case el of
                Nothing  -> ListPat (reverse bf) dotp
                -- when we counter an ellipses, we swap the lists so that we
                -- don't have to write separate loops that do the same thing
                -- so now we have to swap them back.
                Just elp -> EllipsisListPat (reverse af) elp (reverse bf) dotp
            go (v : Symbol maybeDots : rest) ~(bf, el, _af, dot)
              | maybeDots == dots = case el of
                  Nothing -> do
                    pat <- compile v
                    go rest ([], Just pat, bf, dot)
                  Just{} -> lift $ throwError $ 
                    Default "multiple ellipses in list pattern"
            go (v : rest) ~(bf, el, af, dot) = do
              vp <- compile v
              go rest (vp:bf, el, af, dot)

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
          k = length before
          m = length after
      guard $ n >= k + m
      let (bvs, vs') = splitAt k vs
          (evs, avs) = splitAt (n-k-m) vs'
      matchingBefore <- zipWithM match' before bvs
      matchingEs     <- zipWithM match' (repeat e) evs
      matchingAfter  <- zipWithM match' after avs
      let matchingESingletons = fmap (fmap (:[])) matchingEs
          matchingELists = foldl' (M.unionWith (++)) M.empty matchingESingletons
          finalEMatching = Deeper <$> matchingELists
      return $ M.unions $ dotMatching
                        : finalEMatching
                        : matchingBefore ++ matchingAfter
    
    match' (VectorPat ps) v = do
      guard $ vectorSH v
      guard $ length ps == vectorLengthPH v
      vs <- vectorElemsPH v
      matchings <- zipWithM match' ps vs
      return $ M.unions matchings
    
    match' (EllipsisVectorPat{}) v = panic "ellipses vector pattern matching not implemented"

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
  deriving Show

{- HLINT ignore "Use section" -}
transcribe :: Template -> Matching -> EM Val
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
          unwrapDeeper (Deeper lst) = lst
          unwrapDeeper Here{} = panic "transcribe: impossible case"
      guardAllDeeper m'
      guardSameLength m''
      let mlst = M.toList m''
          pushDownKeys xs = [ [(i,y) | y <- ys] | (i,ys) <- xs ]
          lstM = map M.fromList $ transpose $ pushDownKeys mlst
      mapM (template t) lstM
    
    guardAllDeeper :: Matching -> EM ()
    guardAllDeeper m = pure ()

    guardSameLength :: M.HashMap PatternVar [Match Val] -> EM ()
    guardSameLength m = pure ()
