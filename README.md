shackage
========

super hackage


```
Distribution.Version Prelude Data.Version Text.ParserCombinators.ReadP> let readV v = let l = readP_to_S parseVersion v in fst $ head $ drop (length l - 1) l
```
