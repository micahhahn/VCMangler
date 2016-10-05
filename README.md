# VCMangler
Mangling and Demangling of VC++ names in Haskell

This project aims provide a parser for converting mangled VC++ names into a Haskell data structure and back as well as pretty printing functionality.

Example:

```haskell
parseMangledName "?pow@@YANNN@Z"
```

Returns:

```haskell
Right (Function (Name "pow") (Global Near CDecl) Double_ [Double_,Double_])
```
## TODO:
* Thunks
* Virtual Function Tables
