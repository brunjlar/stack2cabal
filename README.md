# stack2cabal

This is a simple tool for using
[`cabal new-build`](http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html)
in Haskell projects that have originally been designed
for use with [`stack`](https://docs.haskellstack.org/en/stable/README/).

The tool does two things:

* `cabal new-build` (at the moment) handles only dependencies that are
  either _local_ or _on Hackage_.
  In `stack`, on the other hand, it is possible to use
  dependencies from GitHub by specifying repository-URL and commit
  in the `stack.yaml` file.
  The `stack2cabal` tool scans the `stack.yaml` file for such dependencies,
  clones the repositories into subfolder `./stack2cabal/`
  and checks out the correct commit.

* Using the packages listed in `stack.yaml` and the cloned git repositories,
  `stack path --compiler-exe` to determine the correct GHC version
  and `stack list-dependencies` to get the correct version of each dependency,
  the `stack2cabal` tool creates the `cabal.project` file needed for
  `cabal new-build`, 
  thus making sure that `cabal new-build` will build the same packages as `stack`,
  will use the same GHC version as `stack`
  and will use the same dependency versions as `stack`.
