{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Data.ConstRef 
  ( ConstRef(ConstRef)
  ) where

import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import Data.IORef

newtype ConstRef a = CR { ioRef :: IORef a }
  deriving Eq

{- Note: [unsafeDupablePerformIO]
'unsafeDupablePerformIO' is efficient but requires us to know a few things
about the IO action we're performing. It should be
  (1) constant, that is, it always returns the same value
  (2) free of side-effects

While /reading/ a ConstRef satisfies both of these properties, creating
a new one does not. Duplicating a call to 'newIORef' would result in two
distinct IORefs, and that could be bad.
-}

{- Note: [immutable Scheme objects and sharing]
When using datum labels, it's necessary that the correct ConstRef object
is referred to by the later references. This is a little bit fishy so we
must be careful! If fishy stuff starts happening with datum labels, it's
probably here.

Consider the following program:
```
import Data.ConstRef

main :: IO ()
main = do
  let x = 5
      r1 = ConstRef x
      r2 = ConstRef x
      r3 = ConstRef 10
  print $ r1 == r2
  print $ r3 == r3
```
When compiled with optimizations, this program prints
```
True
True
```
because GHC decides that r1 and r2 are defined by a common subexpression
and eliminates it.

I'm not aware of any situation that would cause GHC to do _less_ sharing,
i.e. make `r3 == r3` result in False. This is fine; r7rs does not say that we
are or are not allowed to _decide_ to share equivalent immutable structure
on an ad-hoc basis. Normally we would choose not to share, but it's OK to let
GHC decide to do extra sharing if GHC thinks that that's appropriate. However,
I wouldn't actually expect that to happen in practice, because GHC doesn't
attempt to make sharing decisions at runtime as far as I know.

A future change to the parser may result in the parser having access to an
ST state thread, in which case we can change ConstRef to be pure for reading
but impure (in ST, preferably) to create, and then the parser will be able to
guarantee that sharing happens when it should.

12/9/2020
-}

newConstRef :: a -> ConstRef a
newConstRef x = CR (unsafePerformIO (newIORef x))
{-# NOINLINE newConstRef #-}

readConstRef :: ConstRef a -> a
readConstRef = unsafeDupablePerformIO . readIORef . ioRef

pattern ConstRef :: a -> ConstRef a
pattern ConstRef x <- (readConstRef -> x)
  where ConstRef = newConstRef
