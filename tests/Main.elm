port module Main exposing (..)

import All
import Json.Encode exposing (Value)
import Test.Runner.Node as NodeRunner


main : NodeRunner.TestProgram
main =
    NodeRunner.run emit All.suite


port emit : ( String, Value ) -> Cmd msg
