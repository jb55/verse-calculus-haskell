
# Verse Calculus

This demo was vibe-coded with o3 via the [Verse calculus paper][verse-paper]

## Demo

```haskell
ghci> :l VerseCalculus.hs Parser.hs

ghci> mapM_ printEval $ Parser.parseExpr "x. y. z. x = (y, z); x = (z, 2); z"
{ x = ⟨y, z⟩, y = z, z = 2 }
↳ 2

ghci> printExpr demo
∃x. ∃y. ∃z. x = ⟨y, 3⟩ ; x = ⟨2, z⟩ ; y

ghci> printEval demo
{ x = ⟨y, 3⟩, y = 2, z = 3 }
↳ 2
```

[verse-paper]: https://simon.peytonjones.org/verse-calculus/
