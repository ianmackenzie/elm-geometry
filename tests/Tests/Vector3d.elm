module Tests.Vector3d exposing (sum)

import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Vector3d


sum : Test
sum =
    Test.fuzz (Fuzz.list Fuzz.vector3d) "sum is consistent with plus" <|
        \vectors ->
            Vector3d.sum vectors
                |> Expect.vector3d
                    (List.foldl Vector3d.plus Vector3d.zero vectors)
