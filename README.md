# validation-micro

[![GitHub CI](https://github.com/unfoldml/validation-micro/workflows/CI/badge.svg)](https://github.com/unfoldml/validation-micro/actions)

Lightweight pure data validation based on `Applicative` .

`validation-micro` is built around the following data type:

```haskell
data Validation e a
    = Failure e
    | Success a
```

This data type is similar to `Either` but allows accumulating all
errors instead of short-circuiting on the first one.

## Comparison with other packages

`validation-micro` is not the only package that provides the `Validation` data type. 
Here are some differences and commonalities to other validation packages:

+ **Lightweight**. `validation-micro` depends only on `base` and
  `deepseq`, which make it fast to build. 
  So adding validation capabilities to your
  library or application doesn't contribute much to your dependency
  footprint.
+ **Future-proof**. Both `base` and `deepseq` are GHC boot packages, which means that `validation-micro` can be imported as long as GHC can run.
+ **No Selective instance**. This means you cannot choose which validation to use based on the value. On the other hand, we don't need to depend on `selective` which is a somewhat experimental package.
+ **No Monad instance** - but there is a `bindValidation` utility function which behaves like `(>>=)`.

## Copyright

Copyright:  (c) 2014 Chris Allen, Edward Kmett
            (c) 2018-2023 Kowainik
            (c) 2023 Marco Zocca, UnfoldML
License: BSD3
Maintainer:  oss@unfoldml.com
