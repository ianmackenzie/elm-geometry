Thanks for your interest in OpenSolid! Currently, the best way to contribute is
simply to [open a new issue](https://github.com/opensolid/geometry/issues) for
any new features you're interested in or any bugs you notice (including things
like misleading or confusing documentation - documentation issues are just as
important as code issues!). If you're willing to help fix the issue/implement
the feature, please mention that in the issue, but it's certainly not a
requirement!

If you are interested in contributing changes to OpenSolid, please fork this
repository and make a pull request with your proposed changes (although please
open an issue first for major contributions before writing too much code, so we
can discuss different potential approaches).

During development, to run the OpenSolid tests locally, you have two options:

  - Run `elm reactor` inside the `tests` subdirectory. You should then be able
    to open up `localhost:8000` in a browser and click on `All.elm` to run the
    entire test suite. You can also click on any of the files named after
    individual OpenSolid modules (for example, LineSegment2d.elm) to run only
    the tests for that module.
  - Run `elm package install` inside the `tests` subdirectory, install
    [`elm-test`](https://github.com/rtfeldman/node-test-runner) by running
    `npm install -g elm-test`, then run `elm-test` from the root directory of
    this repository to run all the tests from the command line.
