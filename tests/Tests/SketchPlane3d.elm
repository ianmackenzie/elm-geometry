module Tests.SketchPlane3d exposing
    ( normalDirectionIsValid
    , randomlyGeneratedSketchPlanesAreValid
    )

import Direction3d
import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import SketchPlane3d
import Test exposing (Test)
import Test.Check as Test


randomlyGeneratedSketchPlanesAreValid : Test
randomlyGeneratedSketchPlanesAreValid =
    Test.check "Randomly generated sketch planes are valid"
        Random.sketchPlane3d
        Expect.validSketchPlane3d


normalDirectionIsValid : Test
normalDirectionIsValid =
    Test.check "Sketch plane normal direction is valid and is perpendicular to both basis directions"
        Random.sketchPlane3d
        (\sketchPlane ->
            SketchPlane3d.normalDirection sketchPlane
                |> Expect.all
                    [ Expect.validDirection3d
                    , Expect.direction3dPerpendicularTo (SketchPlane3d.xDirection sketchPlane)
                    , Expect.direction3dPerpendicularTo (SketchPlane3d.yDirection sketchPlane)
                    ]
        )
