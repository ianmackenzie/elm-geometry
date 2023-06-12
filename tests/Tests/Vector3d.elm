module Tests.Vector3d exposing (perpendicularTo, sum, vectorScaling)

import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Quantity
import Random
import Test exposing (Test)
import Test.Random as Test
import Vector3d


sum : Test
sum =
    Test.check "sum is consistent with plus" (Random.smallList Random.vector3d) <|
        \vectors ->
            Vector3d.sum vectors
                |> Expect.vector3d
                    (List.foldl Vector3d.plus Vector3d.zero vectors)


vectorScaling : Test
vectorScaling =
    Test.describe "scaling 3d vectors"
        [ Test.check "scaling a zero vector results in a zero vector" Random.length <|
            \len ->
                Expect.equal Vector3d.zero (Vector3d.scaleTo len Vector3d.zero)
        , Test.check2 "scaleTo has a consistent length" Random.length Random.vector3d <|
            \scale vector ->
                if vector == Vector3d.zero then
                    Vector3d.scaleTo scale vector
                        |> Expect.equal Vector3d.zero

                else
                    Vector3d.scaleTo scale vector
                        |> Vector3d.length
                        |> Expect.quantity (Quantity.abs scale)
        , Test.check "normalize has a consistent length" Random.vector3d <|
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
    Test.check "perpendicularTo works properly" Random.vector3d <|
        \vector ->
            Vector3d.perpendicularTo vector
                |> Vector3d.dot vector
                |> Expect.quantity Quantity.zero
