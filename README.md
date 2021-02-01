# Thcheme

An in-progress R7RS Scheme interpreter, implemented in Haskell.

As one would expect, this project began with Write You a Scheme in 48 Hours,
but has since grown far beyond that. Unlike many other "overgrown" WYAS implementations,
most of the original infrastructure has been replaced. Additionally, other WYAS extensions
tend to incorrectly (or inadquately) implement continuations; which are more complicated
than a standard `ContT` transformer because of the interpreter's state. This implementation
uses the most natural monad and as a result, implementations of built-in control flow
operators like raise-continuable and dynamic-wind are as easy as porting a Scheme implementation
into the Haskell codebase, or even using a Scheme implementation in the standard library.

Full R7RS-small support is in progress. There is currently no userguide. I'm also looking for
a better name, as this one was originally a joke.
