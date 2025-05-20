
# Verse Calculus

This demo was vibe-coded with o3 via the [Verse calculus paper][verse-paper]

## Demo

```haskell

$ ghci VC/*

ghci> printExpr VC.MLIREmit.demo
∃x. ∃y. ∃z. x = ⟨y, 3⟩ ; x = ⟨2, z⟩ ; y

ghci> mapM_ putStrLn $ VC.Syntax.runPretty $ runE $ VC.BigStep.eval VC.MLIREmit.demo
{ x = ⟨y, 3⟩, y = 2, z = 3 }
↳ 2
```

[verse-paper]: https://simon.peytonjones.org/verse-calculus/
