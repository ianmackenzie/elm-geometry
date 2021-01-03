module Tests.VectorBoundingBox3d exposing (length)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters)
import Quantity
import Random exposing (Generator)
import Test exposing (Test)
import Test.Check as Test
import Vector3d exposing (Vector3d)
import VectorBoundingBox3d exposing (VectorBoundingBox3d)


boxAndContainedVector : Generator ( VectorBoundingBox3d Meters coordinates, Vector3d Meters coordinates )
boxAndContainedVector =
    Random.vectorBoundingBox3d
        |> Random.andThen
            (\box ->
                VectorBoundingBox3d.randomVector box
                    |> Random.map (\containedVector -> ( box, containedVector ))
            )


length : Test
length =
    Test.check "length" boxAndContainedVector <|
        \( box, vector ) ->
            Vector3d.length vector
                |> Expect.quantityContainedIn (VectorBoundingBox3d.length box)
