This comes from the paper "Hygienic Macro Expansion," Kohlbecker et al.,
published in 1986, describing hygienic macro expansion.

It is modified for our purposes to include additional features of Scheme.

syntactic domains:
  c : const                constant names
  v : var, w : tsvar       (time-stamped) identifier names
  m : mactok               macro name
  r : ctree                coretok expression
  e : mstree, f : tsmstree (time-stamped) macro expression
  s : stree,  t : tsstree  (time-stamped) expression

  x : const ⋃ var ⋃ mactok ⋃ coretok
  y : const ⋃ tsvar ⋃ mactok ⋃ coretok
  z : coretok ⋃ tsstree

semantic domains:
  STF : tsmstree ⟶ tsstree
  𝜃 : ST = tsmstree ⟶ tsstree (macro transcriber)

Note that 'coretok' is more or less the "primitive macros" and we only care about them specially when they are binding primitives. For all other intents and purposes, they are vars. When a rule gives a name explicitly (the rules for lambda, let, etc.), these names are coretoks. If the name has been shadowed, the rule does not apply.



Auxiliary functions:
  S : ℕ ⟶ var ⟶ tsvar, S i v = "#s$i:" ++ v -- Stamp
  S₀ : var ⟶ tsvar, S₀ = S 0
  U : tsvar ⟶ var, U "#s$i:$v" = v -- Unstamp
  [ ⋅ / ⋅ ] : tsvar × var ⟶ tsstree ⟶ tsstree
    \[v/w1\]w2 = (w1 == w2) ? v : w2
    \[v/w\]x = x
    \[v/w\]r
      | w `elem` Br = r
      | otherwise = Rr \[(v,w)\]
    \[v/w\](t1 t2 ...) = (\[v/w\]t1 \[v/w\]t2 ...)
  
  B : ctree ⟶ \[tsvar\] (Binders-of)
    gets the list of binders for a coretok expression
  R : ctree ⟶ [\tsvar × var\] ⟶ tstree, (apply-Rename)
    applies the given renaming to all "bodies" of the 
    coretok which the coretok's bindings scope over.
    The definition varies for each coretok and is omitted.
  I : ctree ⟶ (tstree ⟶ tstree) ⟶ tstree ⟶ tstree, (apply-Inside)
    applies the given tstree transformer to all bodies of the
    coretok, including those which its bindings don't scope over.
  M : ctree ⟶ ST ⟶ ST (Modify)
    Change the transformer environment based on ctree.
    If a binding of ctree shadows a transformer keyword, the
    keyword is removed from the ST mapping. If ctree introduces
    a new keyword, its keyword-transformer mapping is added to
    ST.

Semantic functions:
  ℰ_h : stree ⟶ ST ⟶ stree
  𝒯   : tsstree ⟶ (var ⟶ tsvar) ⟶ tsstree
  ℰ   : tsstree ⟶ ST ⟶ ℕ ⟶ tsstree
  𝒜   : tsstree ⟶ tsstree
  𝒰   : tsstree ⟶ stree

ℰ_h⟦s⟧ = λ𝜃.𝒰⟦𝒜⟦ℰ⟦𝒯⟦s⟧S₀⟧𝜃j₀⟧⟧ where j₀ = 1

𝒯⟦y⟧ = λτ.y
𝒯⟦v⟧ = λτ.τv
𝒯⟦(z₁ ... zₙ)⟧ = λτ.(𝒯⟦z₁⟧τ ... 𝒯⟦zₙ⟧τ)

ℰ⟦c⟧ = λ𝜃j.c
ℰ⟦w⟧ = λ𝜃j.w
ℰ⟦f⟧ = λ𝜃j.ℰ⟦𝒯⟦(𝜃f)⟧(Sj)⟧𝜃(j+1)
ℰ⟦r⟧ = λ𝜃j.Ir(ℰ⟦⋅⟧(Mr𝜃)j)
ℰ⟦(t₁ ... tₙ)⟧ = λ𝜃j.(ℰ⟦t₁⟧𝜃j ... ℰ⟦tₙ⟧𝜃j)

𝒜⟦v⟧ = v
𝒜⟦y⟧ = y
𝒜⟦r⟧ = I(Rr(zip ws vs))(𝒜⟦⋅⟧)
  where ws = \[w₁ ... wₙ\] = Br
        vs = \[v₁ ... vₙ\] are fresh (one easy way is to swap #s for #g)
𝒜⟦(t₁ ... tₙ)⟧ = (𝒜⟦t₁⟧ ... 𝒜⟦tₙ⟧)

𝒰⟦x⟧ = x
𝒰⟦v⟧ = Uv
𝒰⟦(z₁ ... zₙ)⟧ = (𝒰⟦z₁⟧ ... 𝒰⟦zₙ⟧)

It actually gets a lot more complicated because there are constructs for local syntax
definitions. The body of most coretoks can start with arbitrary (syntax-)defines.
Those defines may not be apparent until some macros are expanded; that means we
must first expand each definition in turn until something isn't a define (or a define
shadows a syntactic keyword that we've expanded during this process), then modify
the keyword environment accordingly, and then expand the rest of the body.