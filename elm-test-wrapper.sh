#!/bin/sh
SCRIPT_DIR="`dirname $0`"
find $SCRIPT_DIR/tests/VerifyExamples -type f -exec sed -i "s/Expect.equal/Expect.equalWithinTolerance/" {} \;
elm-test
