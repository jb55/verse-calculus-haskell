
# Verse Calculus

This demo was vibe-coded with o3 via the [Verse calculus paper][verse-paper]

## Demo

```haskell
demo :: Expr
demo =
  existsE "x" $
  existsE "y" $
  existsE "z" $
        ( varV "x" .=. tupleV [ varS "y", intS 3 ] )
     `seqE`
        ( varV "x" .=. tupleV [ intS 2  , varS "z" ] )
     `seqE`
        valE (varV "y")
```

```bash
ghci> :l VerseCalculus.hs

ghci> printExpr demo
∃x. ∃y. ∃z. x = ⟨y, 3⟩ ; x = ⟨2, z⟩ ; y

ghci> printEval demo
{ x = ⟨y, 3⟩, y = 2, z = 3 }
↳ 2
```

[verse-paper]: https://simon.peytonjones.org/verse-calculus/
