# purescript-free-monadplus

[![Latest release](http://img.shields.io/github/release/Risto-Stevcev/purescript-free-monadplus.svg)](https://github.com/Risto-Stevcev/purescript-free-monadplus/releases)


An implementation of free monad plus in purescript


## Installation

`bower install purescript-free-monadplus`


## Usage

See [unit tests][1] for example usage


## Limitation

The current version of this library is uses the simpler implementation for free monads with monad plus. As a result, it isn't optimized in any way for left-associated binds. The implementation of purescript-free could be copied here, or this library could be merged into that one. PRs are welcome.


## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-free-monadplus).


[1]: https://github.com/Risto-Stevcev/purescript-free-monadplus/blob/master/test/Control/MonadPlus/Free.purs
