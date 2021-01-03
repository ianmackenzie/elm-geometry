module Tests.VectorBoundingBox4d exposing (length)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters)
import Quantity
import Random exposing (Generator)
import Test exposing (Test)
import Test.Check as Test
import Vector4d exposing (Vector4d)
import VectorBoundingBox4d exposing (VectorBoundingBox4d)


boxAndContainedVector : Generator ( VectorBoundingBox4d Meters coordinates, Vector4d Meters coordinates )
boxAndContainedVector =
    Random.vectorBoundingBox4d
        |> Random.andThen
            (\box ->
                VectorBoundingBox4d.randomVector box
                    |> Random.map (\containedVector -> ( box, containedVector ))
            )


length : Test
length =
    Test.check "length" boxAndContainedVector <|
        \( box, vector ) ->
            Vector4d.length vector
                |> Expect.quantityContainedIn (VectorBoundingBox4d.length box)
