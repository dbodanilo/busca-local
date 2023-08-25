#!/usr/bin/env ghci

Hill Climbing
===

> module HillClimbing where

```haskell
hillClimbing :: (Ord b)
             => (a -> [a])
             -> (a -> b)
             -> a
             -> a
```

> hillClimbing children value current =
>   let neighbor = maximumValue value (children current)
>   in if value neighbor <= value current
>      then current
>      else hillClimbing children value neighbor
>   where maximumValue v ns =
>           let f n sofar = if v n <= v sofar
>                           then sofar
>                           else n
>           in foldr f current ns