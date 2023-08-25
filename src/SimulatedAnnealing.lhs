#!/usr/bin/env ghci

Simulated Annealing
===

> module SimulatedAnnealing where

> import Test.QuickCheck (Arbitrary(..), Gen(..), choose,
>   elements, generate)

```haskell
simulatedAnnealing :: [Double]
                   -> (a -> [a])
                   -> (a -> Double)
                   -> a
                   -> IO a
```

> simulatedAnnealing (t:ts) children value current
>   | t <= 0 = return current
>   | otherwise = 
>       do neighbor <- generate $ elements (children current)
>          let delta = value neighbor - value current
>          if delta > 0
>          then simulatedAnnealing ts children value neighbor
>          else let prob = exp $ (-delta) / t
>               in do lotto <- generate (choose (0, 1) :: Gen Double)
>                     if prob > lotto
>                     then simulatedAnnealing ts children value neighbor
>                     else simulatedAnnealing ts children value current