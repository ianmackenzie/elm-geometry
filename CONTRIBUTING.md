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
can discuss different potential approaches). As part of your pull request, make
sure that you add yourself to the [AUTHORS](https://github.com/opensolid/geometry/blob/master/AUTHORS)
file!

## Writing code

When writing your code, try to follow existing code style as much as possible -
in particular, this means:

  - Use [`elm-format`](https://github.com/avh4/elm-format) to format your code.
  - Wrap code to 80 columns.

Don't worry too much about writing documentation - small fixes for things like
typos and formatting are certainly welcome, but I would prefer to write the bulk
of the documentation myself to ensure a consistent style and tone throughout.

## Testing

During development, please run the existing tests periodically to make sure you
haven't accidentally broken anything! To run the tests locally, you have two
options:

  - Run `elm reactor` inside the `tests` subdirectory. You should then be able
    to open up `localhost:8000` in a browser and click on `All.elm` to run the
    entire test suite. You can also click on any of the files named after
    individual OpenSolid modules (for example, `LineSegment2d.elm`) to run only
    the tests for that module.
  - Run `elm package install` inside the `tests` subdirectory, install
    [`elm-test`](https://github.com/rtfeldman/node-test-runner) by running
    `npm install -g elm-test`, then run `elm-test` from the root directory of
    this repository to run all the tests from the command line.

If you are working on fixing a bug, please first add a test that catches the bug
to the relevant file in the `tests` subdirectory, then add your fix and verify
that the test now passes. If you are adding a new feature, writing tests for
your feature is appreciated but not mandatory.

## Committing

Git commits should generally be as small and focused as possible, so that they
can be reviewed individually. Commit messages should follow [the seven rules of
a great Git commit message](https://chris.beams.io/posts/git-commit/#seven-rules):

  - Separate subject from body with a blank line
  - Limit the subject line to 50 characters
  - Capitalize the subject line
  - Do not end the subject line with a period
  - Use the imperative mood in the subject line
  - Wrap the body at 72 characters
  - Use the body to explain what and why vs. how

Here are some sample commits to use as examples:

  - [Fix bug in Arc2d.fromEndpoints](https://github.com/opensolid/geometry/commit/593039e1223727afe04c53b3af170dfa2b9725b0)
  - [Test Frame3d a bit more precisely](https://github.com/opensolid/geometry/commit/bcf22c03ede5b7594dbcbde02a49430311d53679)
  - [Tweak triangle test to avoid spurious failure](https://github.com/opensolid/geometry/commit/bce5df26e5646f14577cd60472fab03101346a74)
  - [Add [Point,Vector]#d.interpolateFrom](https://github.com/opensolid/geometry/commit/0c91e5eaf4089d94783c28f2a10ece3005be89e4)

## Sublime Text configuration

If you happen to use Sublime Text as your editor, [here](https://gist.github.com/ianmackenzie/77c686282078f853647fd7d4b8894830)
is the (very simple) Sublime Text project file I use for editing OpenSolid code.
I create an `opensolid` directory, place the project file in it, check out all
OpenSolid repositories as subdirectories, then open the project file in Sublime
Text so I can edit all of my checked-out OpenSolid repositories at once. The
resulting directory structure looks like:

```
opensolid
├── opensolid.sublime-project
├── geometry
│   ├── .git
│   ├── README.md
│   ├── src
│   ├── ...
├── svg
│   ├── .git
│   ├── README.md
│   ├── src
│   ├── ...
```
