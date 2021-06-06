-- | Parsing monad for Thcheme.
--
-- This monad is NOT thread-safe!
--
-- This file is largely extracted from AlexWrappers.hs
-- and then modified to support input from Thcheme ports
-- instead of from a string. AlexWrappers.hs is a public
-- domain source file that can be found at
-- github.com/simonmar/alex/blob/master/data/AlexWrappers.hs
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Parser.Monad where

import Control.Applicative as App (Applicative (..))
import Control.Monad (when, void)

import Data.Word (Word8)
import Data.Char (ord, isSpace)
import qualified Data.Bits
import Data.IORef
import qualified Data.IntMap as M
import qualified System.IO.Unsafe as Unsafe (unsafePerformIO)

import Val (Val, Port(..))
import Types.Port (pReadChar, pPeekChar)

--import Debug.Trace

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])

type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String,       -- characters built up for the current token
                                -- stored in reverse, so must reverse before
                                -- sending to an action in 'alexMonadScan'.
                  Port)         -- current input Port, diff from traditional Alex

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,tok,s) = (p,c,[],tok,s)

-- | Set the token field of the second 'AlexInput' to be the (finalized)
-- value of the token in the first 'AlexInput'. This is necessary because
-- we do not have access to all the characters of the input until they've
-- been queried by 'alexGetByte', but we want to give the 'AlexInput'
-- from /before/ the token was scanned. See 'alexMonadScan' in Lexer.x
-- for where this is used.
setTokenFrom :: AlexInput -- input with the whole token
             -> AlexInput -- input from before the token
             -> AlexInput -- Input from before, updated with the built-up token
setTokenFrom (_,_,_,tok,_) (p,c,bs,_,port) = 
  let tok' = reverse tok
  in --trace ("read token: \"" ++ tok' ++ "\"")
     (p,c,bs,tok', port)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_tok,_s) = c

-- This is somewhat experimental, because in every sense this is
-- not a safe use of 'unsafePerformIO'. However, Alex requires
-- that this function return its value purely, so here we are.
-- When alex invokes 'alexGetByte', I assume the result of Alex
-- is always strict in the result of 'alexGetByte', and certainly
-- that the decision to invoke 'alexGetByte' /again/ is strict
-- in the result of the previous invocation. So we should at
-- least be guaranteed that things always happen in the right order.
-- The "purity" of this function is the reason that 'Alex'
-- need not mention 'IO' in its definition.
alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,b:bs,tok,s) = Just (b,(p,c,bs,tok,s))
-- it's important that at least one component of the tuple crosses the
-- 'unsafePerformIO' boundary so that GHC doesn't float the 'unsafePerformIO'
-- call outside the implicit lambda. That would result in only ever
-- getting one character.
alexGetByte (posn,_c,[],tok,port) = Unsafe.unsafePerformIO $ do
  let AlexPn addr _ _ = posn
  mCachedResult <- checkAddressMap addr
  case mCachedResult of
    -- may have been cached while reading a different token,
    -- so make sure we return a result that builds on the given token.
    Just res -> return $ withBaseToken tok res
    Nothing -> do
      -- this implements a one-character lookahead lag, so that when
      -- parseDatum completes, the port will be pointed at the first
      -- character after the end of the datum as specified.
      when (addr /= 0) $ void $ pReadChar port
      mNextCharFromPort <- pPeekChar port
      case mNextCharFromPort of
        -- end-of-file: 'delimited' knows that this is an acceptable
        -- delimiter, so we don't need the "extra newline" hack.
        Nothing -> return Nothing
        Just nextChar ->
          let posn' = --trace ("read char from port: " ++ [nextChar]) $
                      alexMove posn nextChar
          in case utf8Encode' nextChar of
            (b, bs) -> do
              let res = posn' `seq` 
                    Just (b, (posn', nextChar, bs, nextChar : tok, port))
              addToAddressMap addr res
              return res
{-# NOINLINE alexGetByte #-}

withBaseToken :: String -> Maybe (Byte,AlexInput) -> Maybe (Byte,AlexInput)
withBaseToken _ Nothing = Nothing
withBaseToken tok (Just (b,(p,c,bs,_,s))) = 
  -- trace ("retrieved cached char '" ++ [c] ++ "' at posn " ++ show p) $
  Just (b,(p,c,bs,c:tok,s))

-- Alex calls 'alexGetByte' several times on the same 'alexInput' object.
-- We need to therefore ensure 'alexGetByte' is pure. We maintain
-- this map that we can read to check if we've seen a particular address
-- of an 'AlexInput' while scanning this token already. We don't need to
-- keep the /whole/ input here (if we did, we'd probably use an (unboxed?)
-- array), we can clear addresses prior to the current input safely whenever
-- 'alexGetInput' is called.
-- Really, we only /want/ to clear it when 'alexMonadScan' is called, but we
-- want to keep this ugliness out of Lexer.x and 'alexMonadScan' has to be
-- defined there. 
-- That works out though, because 'alexGetInput' is never called /while/
-- alex is scanning a token.
addressToResult :: IORef (M.IntMap (Maybe (Byte,AlexInput)))
addressToResult = Unsafe.unsafePerformIO $ newIORef M.empty
{-# NOINLINE addressToResult #-}

checkAddressMap :: Int -> IO (Maybe (Maybe (Byte,AlexInput)))
checkAddressMap addr = do
  currentMap <- readIORef addressToResult
  return $ M.lookup addr currentMap

addToAddressMap :: Int -> Maybe (Byte,AlexInput) -> IO ()
addToAddressMap addr res = modifyIORef' addressToResult (M.insert addr res)

-- We can use this so that we aren't holding around a huge map of addresses
-- all the time.
-- We're given the current position of the lexer when this is called.
-- Anything from before that position will definitely never be seen again.
-- Things newer than that position might be in the map because of
-- the /{ delimited } right contexts.
clearOldFromAddressMap :: AlexPosn -> IO ()
clearOldFromAddressMap (AlexPn addr _ _) = modifyIORef' addressToResult f
        -- M.split returns (<addr, >addr), so to make sure addr itself is
        -- still in the map, we subtract one.
  where f m = snd $ M.split (addr-1) m

-- Clear the map entirely. This guarantees that no state persists
-- between calls to the lexer, which would have very confusing and
-- hard-to-track down effects.
clearAddressMap :: IO ()
clearAddressMap = writeIORef addressToResult M.empty

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alex_tab_size :: Int
alex_tab_size = 8

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (c+alex_tab_size-((c-1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- -----------------------------------------------------------------------------
-- Monad (default and with ByteString input)

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: Port,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode
      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
    }

-- Compile with -funbox-strict-fields for best results!

-- TODO: Don't even export runAlex from the lexer!
runAlex :: Port -> Alex a -> Either String a
runAlex input__ (Alex f) =
  case f (AlexState {alex_bytes = [],
                    alex_pos = alexStartPos,
                    alex_inp = input__,
                    alex_chr = '\n',
                    alex_ust = alexInitUserState,
                    alex_scd = 0}) of
    Left msg -> Left msg
    Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

data PState
  = PState { source_name :: String
    
           , workingMap  :: Labels
           , completeMap :: Labels
             -- | Don't define new labels while parsing a datum comment.
             -- Note: we could use one field for both nested block comments
             -- and nested datum comments, but this would make the code
             -- harder to maintain without substantial gain. The lexer is
             -- threaded through the parser, so sometimes we will have to
             -- lex (nested) block comments in the middle of parsing a (nested)
             -- datum comment.
           , datum_comment_depth :: !Int
           , block_comment_depth :: !Int
             -- | Used to detect the #0=#0# error. Stores the value of
             -- the label currently being defined and the location we should
             -- use if we need to produce an error message.
           , defining_label :: Maybe (AlexPosn, Int)
           
             -- | Used while tokenizing strings and vertical-bar identifiers
             -- as scratch space to build a string.
           , scratch_space :: String

             -- | To integrate with a Scheme implementation, this should be
             -- replaced by a set of flags as appropriate.
           , fold_case :: Bool
           }

initializePState :: String -> Labels -> PState
initializePState srcName completeMap
  = PState { source_name = srcName
           , workingMap = M.empty
           , completeMap = completeMap
           , datum_comment_depth = 0
           , block_comment_depth = 0
           , defining_label = Nothing
           , scratch_space = ""
           , fold_case = False
           }

type Labels = M.IntMap Val
type AlexUserState = PState

alexInitUserState :: PState
alexInitUserState = error "someone used runAlex. Use runAlex' instead!"

-- | Run an Alex parser, giving a source name and the source itself.
-- Returns either a string representing a lexical error, or the result type
-- of the parser.
-- Handles the black-magic laziness that is used to support datum labels.
runAlex' :: Alex a -> String -> Port -> Either String a
runAlex' (Alex f) srcName input =
      -- this unsafePerformIO call is for 'clearAddressMap'.
      -- It needs to go here so that it captures arguments of runAlex',
      -- preventing GHC from floating it out of the implicit lambda.
  let r = Unsafe.unsafePerformIO $ do
        clearAddressMap
        return $
          f AlexState { alex_pos = alexStartPos
                      , alex_inp = input
                      , alex_chr = '\n'
                      , alex_bytes = []
                        -- it is absolutely crucial that the 'mapFromResult r'
                        -- thunk is never forced until the parse is over. If
                        -- the parse fails, we won't ever be trying to force
                        -- problematic vals. If the parse succeeds, then there
                        -- is a map that we can use and we should be good
                        -- to go.
                      , alex_ust = initializePState srcName $ mapFromResult r
                      , alex_scd = 0
                      }
  in case r of
    Left msg    -> Left msg
    Right (_,a) -> Right a
  where
    mapFromResult Left{} = error "result forced early (but you probably won't see this because it will just hang)"
    mapFromResult (Right (AlexState{alex_ust = PState{workingMap = m}}, _)) = m

alexState :: (PState -> (a, PState)) -> Alex a
alexState f = Alex $ \s@AlexState{alex_ust=ust} ->
  let (a, ust') = f ust in Right (s{alex_ust = ust'}, a)
{-# INLINE alexState #-}

alexGet :: Alex PState
alexGet = alexState $ \s -> (s, s)

alexPut :: PState -> Alex ()
alexPut s = alexState $ const ((), s)

alexModify :: (PState -> PState) -> Alex ()
alexModify f = alexGet >>= alexPut . f

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
     -- This unsafePerformIO is for 'clearOldFromAddressMap', and it needs to go
     -- here specifically so that GHC can't float it outside of the lambda
     -- (because the IO action being performed refers to variables bound by it).
     Unsafe.unsafePerformIO $ do
       clearOldFromAddressMap pos
       return $ Right (s, ( pos
                          , c
                          , bs
                          , [] -- empty token
                          , inp__))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,_tok,inp__)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of
                  state__@(AlexState{}) -> Right (state__, ())

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

{- Alex normally defines these, but we don't need them.
alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())
-}

{- We definitely need this one, but since normally the definition
-- of AlexResult would be in the same file, we have to move it to Lexer.x
alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ show line ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__' len action -> do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len
-}

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> Alex result

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)

-- -----------------------------------------------------------------------------
-- Predicate for asserting that the next character is a delimiter.
-- Alex seems to be overzealous when it reads characters for right-contexts,
-- reading usually one more character than required. Since our ports can't
-- backtrack, reading extra characters is a no-deal. Therefore, we write
-- the predicate ourselves.

delimited :: void -> AlexInput -> Int -> AlexInput -> Bool
delimited _ _before _len after
  = case alexGetByte after of
    Nothing -> True
    Just (_,(_,c,_,_,_)) -> isSpace c || c `elem` ("|()[]{};\"" :: String)
