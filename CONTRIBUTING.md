# Introduction

Thanks for your interest in `elm-geometry`! Currently, the best way to
contribute is simply to [open a new issue](https://github.com/ianmackenzie/elm-geometry/issues)
for any new features you're interested in or any bugs you notice (including
things like misleading or confusing documentation - documentation issues are
just as important as code issues!). If you're willing to help fix the
bug/implement the feature, please mention that in the issue, but it's certainly
not a requirement!

Another great way to contribute is to look for [issues marked with *question*](https://github.com/ianmackenzie/elm-geometry/issues?q=is%3Aissue+is%3Aopen+label%3Aquestion),
which are ones where I'm actively seeking feedback from the community on whether
something should be done or what the right approach is. Feedback is welcome on
any issue, though!

In general, I try to follow [the Elm guidelines](https://twitter.com/czaplic/status/928359033844539393)
and ask that you try to as well:

  - Be kind.
  - Learn from everyone.
  - Collaboration requires communication.
  - Not every problem should be solved with code.
  - Communication _is_ contribution.
  - Understand the problem.
  - Explore all possible solutions.
  - Pick one.
  - Simplicity is not just for beginners.
  - It's better to do it _right_ than to do it _right now_.
  - It's not done until the docs are great.
  - Take responsibility for user experiences.
  - Make impossible states impossible.
  - There are worse things than being explicit...

# Contributing changes

If you _are_ interested in contributing changes to `elm-geometry`, please fork
this repository, make a new branch in your fork, commit your changes to that
branch and then make a pull request from that branch (although please open an
issue first for major contributions before writing too much code, so we can
discuss different potential approaches). As part of your pull request, make sure
that you add yourself to the [AUTHORS](https://github.com/opensolid/geometry/blob/master/AUTHORS)
file! Definitely reach out on the [Elm Slack](http://elmlang.herokuapp.com/) if
you have questions (I'm **ianmackenzie**).

## Writing code

When writing your code, try to follow existing code style as much as possible -
in particular, this means:

  - Use [`elm-format`](https://github.com/avh4/elm-format) to format your code.
    I currently use `elm-format` version 0.8.0.
  - Wrap code (mostly) to 80 columns (type annotations and string literals can
    be longer if you want).

Don't worry too much about writing documentation - small fixes for things like
typos and formatting are certainly welcome, but I would prefer to write the bulk
of the documentation myself to ensure a consistent style and tone throughout.

## Testing

During development, please run the existing tests periodically to make sure you
haven't accidentally broken anything! To run the tests:

  - Install [`elm-test`](https://github.com/rtfeldman/node-test-runner) by running `npm install -g elm-test`
  - Run `elm-test` from the root directory of this repository

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

  - [Fix bug in Arc2d.fromEndpoints](https://github.com/ianmackenzie/elm-geometry/commit/593039e1223727afe04c53b3af170dfa2b9725b0)
  - [Test Frame3d a bit more precisely](https://github.com/ianmackenzie/elm-geometry/commit/bcf22c03ede5b7594dbcbde02a49430311d53679)
  - [Tweak triangle test to avoid spurious failure](https://github.com/ianmackenzie/elm-geometry/commit/bce5df26e5646f14577cd60472fab03101346a74)
  - [Add [Point,Vector]#d.interpolateFrom](https://github.com/ianmackenzie/elm-geometry/commit/0c91e5eaf4089d94783c28f2a10ece3005be89e4)
