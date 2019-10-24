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

This is our style guide for writing code:

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


### Git Policy

When working with Git and interacting with GitHub, these are
guidelines we follow:

* The main Git branch is *master*. No development is done directly on
  this branch.
* Do not force-push to the main branch, namely *master*.
* The development is done in branches other than *master*. To
  contribute a feature, fix a bug or to implement an enhancement,
  first find or open a corresponding issue in the [issue
  tracker](https://github.com/juspay/fencer/issues). Next, create a
  branch based on *master*. The branch should be named in accordance
  with a template `<github nick>/<issue no>-<description>`, e.g., a
  branch could be named *rocketman/42-concurrency-fix*.
* Once your are satisfied with your contribution branch, [create a
  pull
  request](https://help.github.com/en/articles/creating-a-pull-request)
  to the *juspay/fencer* repository's *master* branch. You will need
  at least one approving review before you can merge your
  contribution.
* When merging a pull request, use "Squash and merge". We are working
  hard to keep the commit history clean of noise, so e.g., pull
  request fixes and improvements should not be visible in the history
  of *master*. Alternatively, you can locally rebase your branch and
  keep a few commits that logically organize your
  contribution. Finally, no branch merge commits should get into the
  *master* history.

[1]: https://help.github.com/en/github/getting-started-with-github/fork-a-repo
