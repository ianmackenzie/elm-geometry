# Introduction

Thanks for your interest in OpenSolid! Currently, the best way to contribute is
simply to [open a new issue](https://github.com/opensolid/geometry/issues) for
any new features you're interested in or any bugs you notice (including things
like misleading or confusing documentation - documentation issues are just as
important as code issues!). If you're willing to help fix the issue/implement
the feature, please mention that in the issue, but it's certainly not a
requirement!

# Contributing changes

If you _are_ interested in contributing changes to OpenSolid, please fork this
repository and make a pull request with your proposed changes (although please
open an issue first for major contributions before writing too much code, so we
can discuss different potential approaches).

During development, to run the OpenSolid tests locally, you have two options:

  - Run `elm reactor` inside the `tests` subdirectory. You should then be able
    to open up `localhost:8000` in a browser and click on `All.elm` to run the
    entire test suite. You can also click on any of the files named after
    individual OpenSolid modules (for example, `LineSegment2d.elm`) to run only
    the tests for that module.
  - Run `elm package install` inside the `tests` subdirectory, install
    [`elm-test`](https://github.com/rtfeldman/node-test-runner) by running
    `npm install -g elm-test`, then run `elm-test` from the root directory of
    this repository to run all the tests from the command line.

When writing your code, try to follow existing code style as much as possible -
in particular, this means:

  - Use [`elm-format`](https://github.com/avh4/elm-format) to format your code.
  - Wrap code to 80 columns.

If you are fixing a bug, please add a test that catches the bug to the relevant
file in the `tests` subdirectory, then add your fix and verify that the test now
passes. If you are adding a new feature, writing tests for your feature is
appreciated but not mandatory.

Don't worry too much about writing documentation - small fixes for things like
typos and formatting are certainly welcome, but I would prefer to write the bulk
of the documentation myself to ensure a consistent style and tone throughout.

## Sublime Text

If you happen to use Sublime Text as your editor, [here](https://gist.github.com/ianmackenzie/77c686282078f853647fd7d4b8894830)
is the (very simple) Sublime Text project file I use for editing OpenSolid code.
I create an `opensolid` directory, place the project file in it, check out all
OpenSolid repositories as subdirectories, then open the project file in Sublime
Text so I can edit all of my checked-out OpenSolid repositories at once; the
resulting directory structure looks like

```
opensolid
├── opensolid.sublime-project
├── geometry
│   ├── README.md
│   ├── .git
│   ├── src
│   ├── ...
├── svg
│   ├── README.md
│   ├── .git
│   ├── src
│   ├── ...
```
