port module Main exposing (..)

import All
import Test.Runner.Node as NodeRunner
import Json.Encode exposing (Value)


main : NodeRunner.TestProgram
main =
    NodeRunner.run emit All.suite


port emit : ( String, Value ) -> Cmd msg
