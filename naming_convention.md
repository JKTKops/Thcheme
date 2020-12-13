Many operations are useful at the Haskell level but are _also_ primitive
procedures of scheme (e.x. `cons`). This inevitably leads to naming conflicts.
The convention we use is as follows:

For each argument/return of the function, append the corresponding letter to
the end of the name:

* S - Scheme object (aka `Val`)
* P - Scheme object, but only handles the cases where the object has the
      correct type. P is for "partial". It is OK for partial functions to
      panic when they see an object of the wrong type. Write/use with care!
      A function in `EM` that throws a Scheme error instead of panicking
      should use S, not P.
* R - Ref to an obj, i.e. `Ref Val` or `Ref String`
* C - ConstRef to an obj
* H - any other Haskell type, i.e. `[Val]`, `String`, `Vector` etc.

There is one exception. If the type of the function is `Builtin`, append B
instead of HV (recall `type Builtin = [Val] -> EM Val`).

This also extends to one type that is not a function: `Primitive`. For
primitives, append P.

For example, all of the following are defined in either Val.hs or List.hs:
```haskell
carRR :: Ref PairObj -> EM (Ref Val)
carRS :: Ref PairObj -> EM Val
carCC :: ConstRef IPairObj -> EM (Ref Val)
carCS :: ConstRef IPairObj -> EM Val
carB  :: Builtin
carP  :: Primitive
```
(`carSS` could be defined too, but is not currently.)
