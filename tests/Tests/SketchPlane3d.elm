module Tests.SketchPlane3d exposing
    ( normalDirectionIsValid
    , randomlyGeneratedSketchPlanesAreValid
    )

import Direction3d
import Expect
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import SketchPlane3d
import Test exposing (Test)


randomlyGeneratedSketchPlanesAreValid : Test
randomlyGeneratedSketchPlanesAreValid =
    Test.fuzz Fuzz.sketchPlane3d
        "Randomly generated sketch planes are valid"
        Expect.validSketchPlane3d


normalDirectionIsValid : Test
normalDirectionIsValid =
    Test.fuzz Fuzz.sketchPlane3d
        "Sketch plane normal direction is valid and is perpendicular to both basis directions"
        (\sketchPlane ->
            let
                xDirection =
                    SketchPlane3d.xDirection sketchPlane

                yDirection =
                    SketchPlane3d.yDirection sketchPlane
            in
            SketchPlane3d.normalDirection sketchPlane
                |> Expect.all
                    [ Expect.validDirection3d
                    , Direction3d.componentIn xDirection >> Expect.float 0
                    , Direction3d.componentIn yDirection >> Expect.float 0
                    ]
        )
