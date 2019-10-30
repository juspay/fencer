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


### Linting

When developing Fencer, it is important to keep in mind that linting
is performed automatically during continuous integration, i.e., before
a pull request is approved for merging. This means that Haskell code
will be checked against a number of stylistic or programmatic errors
and if any of them match, the pull request will not get an
approval. To save you from waiting on a continuous integration job to
give a linting verdict, you can quickly lint locally before
Git-pushing to a remote repository. When in a `nix-shell`, run:

```bash
hlint --git
```

If there are any issues with the code that match a curated list of
linting patterns, the `hlint` tool will report them and exit with a
non-zero status. To make sure you do not forget to lint before
Git-pushing, you can set up a pre-push Git hook. Copy a script
`scripts/pre-push` to the `.git/hooks` subdirectory. It is important
to keep the script name as is so Git can know when exactly to run it:

```bash
cp scripts/pre-push .git/hooks/
```

Now if you try to push a Haskell source code file that does not pass
linting, Git will print out problems and suggestions how to fix them
as reported by `hlint` and finally it will fail the push to the remote
repository with an error message, e.g.:

```
test/Fencer/Rules/Test.hs:34:49: Warning: Redundant do
Found:
  do Temp.withSystemTempDirectory "fencer-config" $
       \ tempDir ->
         do TIO.writeFile (tempDir </> "config1.yml") domain1Text
            TIO.writeFile (tempDir </> "config2.yaml") domain2Text
            definitions <- loadRulesFromDirectory (#directory tempDir)
                             (#ignoreDotFiles True)
            assertEqual "unexpected definitions"
              (sortOn domainDefinitionId [domain1, domain2])
              (sortOn domainDefinitionId definitions)
Perhaps:
  Temp.withSystemTempDirectory "fencer-config" $
    \ tempDir ->
      do TIO.writeFile (tempDir </> "config1.yml") domain1Text
         TIO.writeFile (tempDir </> "config2.yaml") domain2Text
         definitions <- loadRulesFromDirectory (#directory tempDir)
                          (#ignoreDotFiles True)
         assertEqual "unexpected definitions"
           (sortOn domainDefinitionId [domain1, domain2])
           (sortOn domainDefinitionId definitions)

1 hint
error: failed to push some refs to 'git@github.com:juspay/fencer.git'
```

Finally, keep in mind that a clean worktree is assumed when using this
pre-push hook. If you would still like to use the hook with unstaged
changes, please `git-stash` before pushing to a remote repository.

[1]: https://help.github.com/en/github/getting-started-with-github/fork-a-repo
