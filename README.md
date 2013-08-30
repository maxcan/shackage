shackage
========

super hackage

## Purpose
shackage will be a simple angular app replicating some of the functionality of 
hackage while making it much simpler to scan though available packages.

## features

* sortable, filterable, table of packages with dates, ghc functions, extensions

* disqus threads on each package

```
Distribution.Version Prelude Data.Version Text.ParserCombinators.ReadP> let readV v = let l = readP_to_S parseVersion v in fst $ head $ drop (length l - 1) l
```
