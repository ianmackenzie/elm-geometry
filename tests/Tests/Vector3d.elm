module Tests.Vector3d exposing (perpendicularTo, sum, vectorScaling)

import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Quantity
import Test exposing (Test)
import Vector3d


sum : Test
sum =
    Test.fuzz (Fuzz.list Fuzz.vector3d) "sum is consistent with plus" <|
        \vectors ->
            Vector3d.sum vectors
                |> Expect.vector3d
                    (List.foldl Vector3d.plus Vector3d.zero vectors)


vectorScaling : Test
vectorScaling =
    Test.describe "scaling 3d vectors"
        [ Test.fuzz Fuzz.length "scaling a zero vector results in a zero vector" <|
            \len ->
                Expect.equal Vector3d.zero (Vector3d.scaleTo len Vector3d.zero)
        , Test.fuzz (Fuzz.tuple ( Fuzz.length, Fuzz.vector3d )) "scaleTo has a consistent length" <|
            \( scale, vector ) ->
                if vector == Vector3d.zero then
                    Vector3d.scaleTo scale vector
                        |> Expect.equal Vector3d.zero

                else
                    Vector3d.scaleTo scale vector
                        |> Vector3d.length
                        |> Expect.quantity (Quantity.abs scale)
        , Test.fuzz Fuzz.vector3d "normalize has a consistent length" <|
            \vector ->
                if vector == Vector3d.zero then
                    Vector3d.normalize vector
                        |> Expect.equal Vector3d.zero

                else
                    Vector3d.normalize vector
                        |> Vector3d.length
                        |> Expect.quantity (Quantity.float 1)
        ]


perpendicularTo : Test
perpendicularTo =
    Test.fuzz Fuzz.vector3d "perpendicularTo works properly" <|
        \vector ->
            Vector3d.perpendicularTo vector
                |> Vector3d.dot vector
                |> Expect.quantity Quantity.zero
