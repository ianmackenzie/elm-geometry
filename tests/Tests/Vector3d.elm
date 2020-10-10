module Tests.Vector3d exposing (sum, vectorScaling)

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
        [ Test.fuzz (Fuzz.tuple ( Fuzz.length, Fuzz.vector3d )) "scaleTo has a consistent length" <|
            \( scale, vector ) ->
                Vector3d.scaleTo scale vector
                    |> Vector3d.length
                    |> Expect.quantity (Quantity.abs scale)
        , Test.fuzz Fuzz.vector3d "normalize has a consistent length" <|
            \vector ->
                Vector3d.normalize vector
                    |> Vector3d.length
                    |> Expect.quantity (Quantity.float 1)
        ]
