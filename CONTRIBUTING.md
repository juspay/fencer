# Contribution Guidelines

This document provides guidelines for contributing to Fencer. Fencer
is a free and open source software project licensed under the
[BSD 3-Clause license](LICENSE) so you are more than welcome to
contribute! There are several ways one can contribute:

- If you are experiencing an issue with Fencer, please use our
  [issue tracker](https://github.com/juspay/fencer/issues). First,
  check if the issue has already been reported by others and if it has
  not been reported open a new issue.
- You can expand and update the documentation. Known limitations with
  the documentation are kept track of in the
  [issue tracker](https://github.com/juspay/fencer/issues). Please
  [fork][1] this repository, make changes there and submit a pull request
  to `juspay/fencer`.
- In case you would like to contribute code, use the
  [issue tracker](https://github.com/juspay/fencer/issues) to find a
  known bug, an enhancement proposal or other needed fix. Then
  [fork][1] this repository, make changes there and submit a pull
  request to `juspay/fencer`. Please be aware of our
  [Style Guide](#style-guide).


## Style Guide

* All modules should use `BasePrelude` as the prelude.
* Imports from external libraries have to be either explicit or
  qualified.
* Imports from `Fencer` modules can be implicit, and should be in most
  cases.
* Avoid defining identifiers with the same names, to keep
  jump-to-definition happy.
* All identifiers should have haddocks.
* Top-level identifiers should not be abbreviated, where feasible. Do
  not use `jsn` instead of `json`, or `dd` instead of `descriptor` or
  `descriptorDefinition`.
* Err on the side of using named arguments (with `Named`). Arguments
  of types `Bool`, `Text`, etc should almost always be named.
* Do not use `String`.
* Do not use `deriving` without qualifying it. Write `deriving stock`
  or `deriving newtype`.


[1]: https://help.github.com/en/github/getting-started-with-github/fork-a-repo
