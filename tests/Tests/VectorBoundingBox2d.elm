module Tests.VectorBoundingBox2d exposing (length)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters)
import Quantity
import Random exposing (Generator)
import Test exposing (Test)
import Test.Check as Test
import Vector2d exposing (Vector2d)
import VectorBoundingBox2d exposing (VectorBoundingBox2d)


boxAndContainedVector : Generator ( VectorBoundingBox2d Meters coordinates, Vector2d Meters coordinates )
boxAndContainedVector =
    Random.vectorBoundingBox2d
        |> Random.andThen
            (\box ->
                VectorBoundingBox2d.randomVector box
                    |> Random.map (\containedVector -> ( box, containedVector ))
            )


length : Test
length =
    Test.check "length" boxAndContainedVector <|
        \( box, vector ) ->
            Vector2d.length vector
                |> Expect.quantityContainedIn (VectorBoundingBox2d.length box)
