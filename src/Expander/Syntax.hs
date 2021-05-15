{-
Syntax objects.

Syntax objects are a carefully balanced intermediate structure in macro
expansion. They almost exactly correspond to the Scheme data that come
from the parser, except that Symbols are annotated with scope information
that macro expansion will resolve.

When macro expansion has completed, the syntax structure is stripped,
replacing each name in each unique scope set with a generated unique name.

Currently, syntax objects take advantage of the parameterization of
Val over the type of identifiers, and Syntax ~/~ Val. However, in the future,
it's not impossible that it would make sense to instead use a custom
record type, passing around Syntax objects as literal Vals, if we ever
add syntax-case.
-}
module Expander.Syntax
  ( Syntax
  , datumToSyntax
  , syntaxToDatum
  ) where


import Expander.Scopes
import Types (ValF, Val, Identifier(..))

type Syntax = ValF Name

data Name = Name
  { name   :: String
  , scopes :: Scopes
  }

instance Identifier Name where
  nameOf = name

datumToSyntax :: Val -> Syntax
datumToSyntax = fmap (\sym -> Name sym noScopes)

syntaxToDatum :: Syntax -> Val
syntaxToDatum = fmap name
