module Tests.Frame3d exposing (frameDirectionsAreOrthonormal)

import Direction3d
import Frame3d
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Quantity
import Test exposing (Test)
import Vector3d


frameDirectionsAreOrthonormal : Test
frameDirectionsAreOrthonormal =
    Test.fuzz Fuzz.frame3d
        "Frame3d basis directions are orthonormal"
        (\frame ->
            let
                xDirectionVector =
                    Direction3d.toVector (Frame3d.xDirection frame)

                yDirectionVector =
                    Direction3d.toVector (Frame3d.yDirection frame)

                zDirectionVector =
                    Direction3d.toVector (Frame3d.zDirection frame)

                tripleProduct =
                    xDirectionVector
                        |> Vector3d.cross yDirectionVector
                        |> Vector3d.dot zDirectionVector

                expectedTripleProduct =
                    if Frame3d.isRightHanded frame then
                        Quantity.cubed (Quantity.float 1)

                    else
                        Quantity.cubed (Quantity.float -1)
            in
            Expect.quantity expectedTripleProduct tripleProduct
        )
