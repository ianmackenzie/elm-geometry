module Tests.Cylinder3d exposing (suite)

import Angle exposing (Angle)
import Circle3d
import Cylinder3d
import Expect
import Frame3d
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Test exposing (Axis3d, Circle3d, Cylinder3d, Frame3d, Plane3d, Point3d, Vector3d)
import Point3d
import Quantity exposing (Quantity)
import Test exposing (Test)
import Vector3d


type alias Transformation coordinates =
    { cylinder : Cylinder3d coordinates -> Cylinder3d coordinates
    , point : Point3d coordinates -> Point3d coordinates
    , circle : Circle3d coordinates -> Circle3d coordinates
    }


rotation : Axis3d coordinates -> Angle -> Transformation coordinates
rotation axis angle =
    { cylinder = Cylinder3d.rotateAround axis angle
    , point = Point3d.rotateAround axis angle
    , circle = Circle3d.rotateAround axis angle
    }


translation : Vector3d coordinates -> Transformation coordinates
translation displacement =
    { cylinder = Cylinder3d.translateBy displacement
    , point = Point3d.translateBy displacement
    , circle = Circle3d.translateBy displacement
    }


scaling : Point3d coordinates -> Float -> Transformation coordinates
scaling centerPoint scale =
    { cylinder = Cylinder3d.scaleAbout centerPoint scale
    , point = Point3d.scaleAbout centerPoint scale
    , circle = Circle3d.scaleAbout centerPoint scale
    }


mirroring : Plane3d coordinates -> Transformation coordinates
mirroring plane =
    { cylinder = Cylinder3d.mirrorAcross plane
    , point = Point3d.mirrorAcross plane
    , circle = Circle3d.mirrorAcross plane
    }


transformationFuzzer : Fuzzer (Transformation coordinates)
transformationFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 rotation Fuzz.axis3d Fuzz.angle
        , Fuzz.map translation Fuzz.vector3d
        , Fuzz.map2 scaling Fuzz.point3d Fuzz.scale
        , Fuzz.map mirroring Fuzz.plane3d
        ]


cylinderAndPoint : Fuzzer ( Cylinder3d coordinates, Point3d coordinates )
cylinderAndPoint =
    Fuzz.map4
        (\cylinder u v theta ->
            let
                halfLength =
                    Quantity.half (Cylinder3d.length cylinder)

                minZ =
                    Quantity.multiplyBy -1.25 halfLength

                maxZ =
                    Quantity.multiplyBy 1.25 halfLength

                radius =
                    Cylinder3d.radius cylinder

                cylinderFrame =
                    Frame3d.fromZAxis (Cylinder3d.axis cylinder)

                z =
                    Quantity.interpolateFrom minZ maxZ u

                r =
                    Quantity.sqrt (Quantity.multiplyBy (v * 1.25) (Quantity.squared radius))

                x =
                    r |> Quantity.multiplyBy (Angle.cos theta)

                y =
                    r |> Quantity.multiplyBy (Angle.sin theta)
            in
            ( cylinder, Point3d.xyzIn cylinderFrame x y z )
        )
        Fuzz.cylinder3d
        Fuzz.parameterValue
        Fuzz.parameterValue
        Fuzz.angle


suite : Test
suite =
    Test.describe "Cylinder3d"
        [ Test.fuzz2
            cylinderAndPoint
            transformationFuzzer
            "Point containment is consistent"
            (\( cylinder, point ) transformation ->
                let
                    initialContainment =
                        Cylinder3d.contains point cylinder

                    transformedPoint =
                        transformation.point point

                    transformedCylinder =
                        transformation.cylinder cylinder

                    finalContainment =
                        Cylinder3d.contains transformedPoint transformedCylinder
                in
                finalContainment |> Expect.equal initialContainment
            )
        , let
            testCap description accessor =
                Test.fuzz2
                    transformationFuzzer
                    Fuzz.cylinder3d
                    description
                    (\transformation cylinder ->
                        let
                            cap =
                                accessor cylinder

                            transformedCylinder =
                                transformation.cylinder cylinder

                            transformedCap =
                                transformation.circle cap

                            capOfTransformed =
                                accessor transformedCylinder
                        in
                        capOfTransformed |> Expect.circle3d transformedCap
                    )
          in
          Test.describe "Caps are consistent through transformation"
            [ testCap "startCap" Cylinder3d.startCap
            , testCap "endCap" Cylinder3d.endCap
            ]
        ]
