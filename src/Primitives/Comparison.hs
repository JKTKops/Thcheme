{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns, MultiWayIf #-}
-- | Scheme comparison primitives, namely
-- eq? eqv? and equal?.
-- The implementation of equal? is adapted from
-- Michael D. Adams and R. Kent Dybvig.
--
-- Also defines the whole family of =, <, ...
-- char=?, char<?, ... and string=?, ...
-- primitives.
module Primitives.Comparison 
  ( primitives
  
    -- * Haskell-level equality tests in IO or EM
  , eqSSH, eqvSSH, equalSSH
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Val
import EvaluationMonad (EM, panic)
import Primitives.Bool (boolBinop)
import Primitives.Vector hiding (primitives)
import Primitives.String hiding (primitives)
import Types.Unwrappers

-- Used to implement 'equals?'
import Data.Functor ((<&>))
import Data.String (fromString)
import Data.UnionFind.IO (fresh, repr, union, Point)
import qualified Data.HashTable.IO as H
import System.Mem.StableName (StableName, makeStableName)
import System.Random (randomRIO)

-- | Scheme primitive exports.
primitives :: [Primitive]
primitives = [eqP, eqvP, equalP] ++ typeSpecific

eqP, eqvP, equalP :: Primitive
eqP    = Prim "eq?" (Exactly 2) eqB
eqvP   = Prim "eqv?" (Exactly 2) eqvB
equalP = Prim "equal?" (Exactly 2) equalB

eqB, eqvB, equalB :: Builtin
equalityBuiltin :: (Val -> Val -> EM Bool) -> Builtin
equalityBuiltin e [x1,x2] = Bool <$> e x1 x2
equalityBuiltin _ _ = panic "equalityBuiltin arity"

eqB    = equalityBuiltin eqSSH
eqvB   = equalityBuiltin eqvSSH
equalB = equalityBuiltin equalSSH

-- TODO: we should really just move these into the files for their own
-- types and define them individually. We maybe end up copying some code
-- (the duplication that's eliminated by OrdBuilder) but in exchange we end
-- up _removing_ the duplication of the logic of string=? and char=? etc. by
-- being able to import the appropriate module of primitives and call out to, 
-- say, stringEqPH directly.
typeSpecific :: [Primitive]
typeSpecific = [ primGen builtin
               | (OrdBuilder primGen) <- primBuilders, builtin <- builtinComparisons
               ] ++
               [ makePrim "" True numBoolBinop eqPair
               , makePrim "string" False strBoolBinop eqPair
               , makePrim "char" False charBoolBinop eqPair
               ]
  where eqPair :: Eq a => (String, a -> a -> Bool)
        -- the monomorphism restriction applies here if type signature is omitted.
        eqPair = ("=", (==))

builtinComparisons :: Ord a => [(String, a -> a -> Bool)]
builtinComparisons = [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     ]

-- | A wrapper over a function that converts a (Haskell) builtin comparison
-- function into a Primitive function. We use this to convert the Haskell
-- builtins into appropriate comparison functions for all comparable Scheme
-- types.
data OrdBuilder = forall a. Ord a =>
                      OrdBuilder ((String, a -> a -> Bool) -> Primitive)

primBuilders :: [OrdBuilder]
primBuilders = [ OrdBuilder makeNumPrim
               , OrdBuilder makeStrPrim
               , OrdBuilder makeCharPrim
               ]

makePrim :: String -- type name
   -> Bool   -- appends '?' if False, nothing if True
   -> (Symbol -> (a -> a -> Bool) -> Primitive)
   -> (String, a -> a -> Bool)
   -> Primitive
makePrim tyname isNum primGen (opName, op) = primGen (fromString primName) op
  where primName = tyname ++ opName ++ suffix
        suffix | not isNum = "?"
               | otherwise = ""

makeNumPrim :: (String, RealNumber -> RealNumber -> Bool) -> Primitive
makeNumPrim = makePrim "" True realBoolBinop
makeStrPrim :: (String, Text -> Text -> Bool) -> Primitive
makeStrPrim = makePrim "string" False strBoolBinop
makeCharPrim :: (String, Char -> Char -> Bool) -> Primitive
makeCharPrim = makePrim "char" False charBoolBinop

-- boolBinop :: String -> (Val -> EM a) -> (a -> a -> Bool) -> Primitive

-- TODO: none of these satisfy r7rs, which says the predicate should be
-- satisfied pairwise along a whole list of elements; we should not use
-- 'boolBinop'.
numBoolBinop :: Symbol -> (Number -> Number -> Bool) -> Primitive
numBoolBinop name = boolBinop name unwrapNum
realBoolBinop :: Symbol -> (RealNumber -> RealNumber -> Bool) -> Primitive
realBoolBinop name = boolBinop name unwrapRealNum
strBoolBinop :: Symbol -> (Text -> Text -> Bool) -> Primitive
strBoolBinop name = boolBinop name unwrapStr
charBoolBinop :: Symbol -> (Char -> Char -> Bool) -> Primitive
charBoolBinop name = boolBinop name unwrapChar

-- EQUIVALENCE FUNCTIONS
-- TODO some notion of function equivalence?
-- the above should be satisfied by eqSSH /I think/.

-- TODO [r7rs]
-- write tests
-- Number equality is currently defined in the sense of =,
-- but exact and inexact numbers should never compare equal.

-- | Scheme eqv? primitive. Delegates to eq? on non
-- primitive Scheme types such as pairs.
eqvSSH :: MonadIO m => Val -> Val -> m Bool
eqvSSH (Bool x)   (Bool y)   = return $ x == y
eqvSSH (Number x) (Number y) = return $ x == y
eqvSSH (Char x)   (Char y)   = return $ x == y
eqvSSH (Symbol x) (Symbol y) = return $ x == y
eqvSSH (Port p)   (Port q)   = return $ p == q
eqvSSH Nil        Nil        = return True
eqvSSH str1 str2
  | stringSH str1, stringSH str2 =
    (==) <$> unwrapStringPH str1 <*> unwrapStringPH str2
eqvSSH v1         v2         = eqSSH v1 v2

-- TODO: instead of defining coercers, we should just import TypeTransformers
-- and implement the standard properly.
-- furthermore, we aren't defining case-insensitive string/char comparisons.
-- Once we have char-foldcase and string-foldcase we should just call those
-- similar to how we'd call a type transformer.

-- generalized over 'MonadIO' so we can use it in 'interleave'.

-- | Scheme eq? primitive. Essentially, pointer
-- equality, though 'Symbol's are handled specially
-- since Thcheme does not maintain a symbol table.
eqSSH :: MonadIO m => Val -> Val -> m Bool
-- so this seems like a weird point of r7rs to me. I'd presume we're
-- kind of expected to make a symbol table, so that any uses of the
-- same name in the program cause us to pull up the same symbol object.
-- That's possible - we just have to be careful to make the names of the
-- symbols the memoized part, and not the whole symbol including library
-- information (once we have libraries).

-- note: It's important that we compare all types of Symbols, since
-- _inside_ the expander there will be frequent eq? comparisons of
-- bindings.
eqSSH (Symbol x) (Symbol y) = pure $ x == y
eqSSH !v1 !v2 = liftIO $ do
  n1 <- makeStableName $! v1
  n2 <- makeStableName $! v2
  return $ n1 == n2
  --n1 <- newStablePtr $! v1
  --n2 <- newStablePtr $! v2
  --let r = n1 == n2
  --freeStablePtr n1
  --freeStablePtr n2
  --return r

-- The rest of the file is essentially dedicated to implementing 'equal?'.
-- The implementation is due to Michael D. Adams and R. Kent Dybvig,
-- "Efficient Nondestructive Equality Checking for Trees and Graphs".
--
-- It would be nice if someone cleaned up the code somewhat... this is more
-- more or less a rapid prototype translated from the Scheme in the paper.

-- | Scheme equal? primitive. Full-powered structural
-- equivalence predicate on directed, possibly-cyclic graphs.
equalSSH :: MonadIO m => Val -> Val -> m Bool
equalSSH v1 v2 = liftIO $ precheckInterleaveEqual v1 v2

k0, kb :: Int
k0 = 400
kb = -40

precheckInterleaveEqual :: Val -> Val -> IO Bool
precheckInterleaveEqual x y = do
  mk <- pre x y k0
  case mk of
    Nothing -> return False
    Just k
      | k > 0 -> return True
      | otherwise -> interleave x y 0

type HashTable = H.LinearHashTable (StableName Val) (Point ())

unionFind :: HashTable -> Val -> Val -> IO Bool
unionFind ht x y = do
  nx  <- makeStableName x
  ny  <- makeStableName y
  mbx <- H.lookup ht nx
  mby <- H.lookup ht ny
  case mbx of
    Nothing -> case mby of
      Nothing -> do
        b <- fresh ()
        H.insert ht nx b
        H.insert ht ny b
        return False
      Just by -> do
        ry <- repr by
        H.insert ht nx ry
        return False
    Just bx -> case mby of
      Nothing -> do
        rx <- repr bx
        H.insert ht ny rx
        return False
      Just by -> do
        rx <- repr bx
        ry <- repr by
        if rx == ry
        then return True
        else union rx ry >> return False

pre :: Val -> Val -> Int -> IO (Maybe Int)
pre x y k = do
  eq <- eqSSH x y
  if | eq -> return $ Just k
     | pairSH x ->
       if not $ pairSH y then return Nothing
       else if k <= 0 then return $ Just k
       else join (pre <$> carPS x <*> carPS y <*> pure (k-1)) >>= \case
          Nothing -> return Nothing
          Just k' ->
            join $ pre <$> cdrPS x <*> cdrPS y <*> pure k'
     | vectorSH x ->
       if not $ vectorSH y then return Nothing
       else vector (vectorLoop True pre) x y k
     | otherwise -> eqvSSH x y <&> \case
       True  -> Just k
       False -> Nothing

-- | Only call this with two vectors, or else it'll panic.
vector :: ([Val] -> [Val] -> Int -> IO (Maybe Int))
          -- ^ call on the vector elems if they are the same length 
       -> Val -> Val -> Int -> IO (Maybe Int)
vector goodToGo x y k =
  let n1 = vectorLengthPH x
      n2 = vectorLengthPH y
  in if n1 /= n2 then return Nothing
  else do
    elemsX <- vectorElemsPH x
    elemsY <- vectorElemsPH y
    goodToGo elemsX elemsY k

vectorLoop :: Bool -- ^ decrement k before passing in?
           -> (Val -> Val -> Int -> IO (Maybe Int))
              -- ^ equality callback
           -> [Val] -> [Val] -> Int -> IO (Maybe Int)
vectorLoop shouldDec e = go where
  go [] [] k = return $ Just k
  go (u:us) (v:vs) k
    | shouldDec =
      if k <= 0 then return $ Just k
      else loop (u:us) (v:vs) (k-1)
    | otherwise = loop (u:us) (v:vs) k
  go _ _ _ = panic "vectorLoop: different lengths" 

  loop (u:us) (v:vs) k = e u v k >>= \case
    Nothing -> return Nothing
    Just k' -> go us vs k'
  loop _ _ _ = panic "loop: empty list"

-- TODO:
-- this is an implementation of the algorithm from
-- section 5, but the algorithm from section 6 has better
-- (usually by a significant constnat) performance.
interleave :: Val -> Val -> Int -> IO Bool
interleave x y k = do
  ht <- H.new
  let callUnionFind x y = unionFind ht x y

      -- e, slow, and fast all return Maybe Int.
      -- Nothing indicates that we should exit, returning False.
      -- Just k indicates that we should proceed with a modified
      -- depth of k.
      e x y k
        | k <= 0 =
          if k == kb
          then randomRIO (0,2*k0) >>= fast x y
          else slow x y k
        | otherwise = fast x y k
      
      slow x y k = do
        eq <- eqSSH x y
        if | eq -> return $ Just k
           | pairSH x ->
             if not $ pairSH y
             then return Nothing
             else do
               ifM (callUnionFind x y)
                   (return $ Just 0)
                   (pair x y (k - 1))
           | vectorSH x ->
             if not $ vectorSH y
             then return Nothing
             else vector (vectorLoop False e) x y (k - 1)
           | otherwise -> eqvSSH x y <&> \case
             True  -> Just k
             False -> Nothing
     
      fast x y initialK = do
        let k = initialK - 1
        eq <- eqSSH x y
        if | eq -> return $ Just k
           | pairSH x ->
             if not $ pairSH y
             then return Nothing
             else pair x y k
           | vectorSH x ->
             if not $ vectorSH y
             then return Nothing
             else vector (vectorLoop False e) x y k
           | otherwise -> eqvSSH x y <&> \case
             True  -> Just k
             False -> Nothing
 
      -- shared by slow and fast
      pair x y k = do
        join (e <$> carPS x <*> carPS y <*> pure k) >>= \case
          Nothing -> return Nothing
          Just k' ->
            join $ e <$> cdrPS x <*> cdrPS y <*> pure k'

  result <- e x y k
  return $ case result of
    Nothing -> False
    Just{}  -> True

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM test t f = test >>= \b -> if b then t else f
