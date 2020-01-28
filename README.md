# safe-decimal

| Language | Travis |
|:--------:|:------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/fpco/safe-decimal.svg) | [![Travis](https://img.shields.io/travis/fpco/safe-decimal/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/fpco/safe-decimal) |

|      Package       | Hackage | Nightly | LTS |
|:-------------------|:-------:|:-------:|:---:|
|  [`safe-decimal`](https://github.com/fpco/safe-decimal/tree/master/safe-decimal)|                                       [![Hackage](https://img.shields.io/hackage/v/safe-decimal.svg)](https://hackage.haskell.org/package/safe-decimal)|                                                                                                        [![Nightly](https://www.stackage.org/package/safe-decimal/badge/nightly)](https://www.stackage.org/nightly/package/safe-decimal)|                                                                                         [![Nightly](https://www.stackage.org/package/safe-decimal/badge/lts)](https://www.stackage.org/lts/package/safe-decimal)|


An implementation of a decimal point data type, that is backed by any custom integral
type. It is safe, because things like integer overflows, underflows, division by zero
etc. are checked for during the runtime and are prevented in pure code.
