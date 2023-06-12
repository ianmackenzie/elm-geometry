module Tests.Cylinder3d exposing (suite)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Circle3d exposing (Circle3d)
import Cylinder3d exposing (Cylinder3d)
import Expect
import Frame3d exposing (Frame3d)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Random exposing (Generator)
import Random.Extra
import Test exposing (Test)
import Test.Random as Test
import Vector3d exposing (Vector3d)


type alias Transformation coordinates =
    { cylinder : Cylinder3d Meters coordinates -> Cylinder3d Meters coordinates
    , point : Point3d Meters coordinates -> Point3d Meters coordinates
    , circle : Circle3d Meters coordinates -> Circle3d Meters coordinates
    , isZeroScale : Bool
    }


rotation : Axis3d Meters coordinates -> Angle -> Transformation coordinates
rotation axis angle =
    { cylinder = Cylinder3d.rotateAround axis angle
    , point = Point3d.rotateAround axis angle
    , circle = Circle3d.rotateAround axis angle
    , isZeroScale = False
    }


translation : Vector3d Meters coordinates -> Transformation coordinates
translation displacement =
    { cylinder = Cylinder3d.translateBy displacement
    , point = Point3d.translateBy displacement
    , circle = Circle3d.translateBy displacement
    , isZeroScale = False
    }


scaling : Point3d Meters coordinates -> Float -> Transformation coordinates
scaling centerPoint scale =
    { cylinder = Cylinder3d.scaleAbout centerPoint scale
    , point = Point3d.scaleAbout centerPoint scale
    , circle = Circle3d.scaleAbout centerPoint scale
    , isZeroScale = scale == 0.0
    }


mirroring : Plane3d Meters coordinates -> Transformation coordinates
mirroring plane =
    { cylinder = Cylinder3d.mirrorAcross plane
    , point = Point3d.mirrorAcross plane
    , circle = Circle3d.mirrorAcross plane
    , isZeroScale = False
    }


transformationGenerator : Generator (Transformation coordinates)
transformationGenerator =
    Random.Extra.choices
        (Random.map2 rotation Random.axis3d Random.angle)
        [ Random.map translation Random.vector3d
        , Random.map2 scaling Random.point3d Random.scale
        , Random.map mirroring Random.plane3d
        ]


cylinderAndPoint : Generator ( Cylinder3d Meters coordinates, Point3d Meters coordinates )
cylinderAndPoint =
    Random.map4
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
        Random.cylinder3d
        Random.parameterValue
        Random.parameterValue
        Random.angle


suite : Test
suite =
    Test.describe "Cylinder3d"
        [ Test.check2 "Point containment is consistent"
            cylinderAndPoint
            transformationGenerator
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
                if transformation.isZeroScale then
                    finalContainment |> Expect.equal True

                else
                    finalContainment |> Expect.equal initialContainment
            )
        , let
            testCap description accessor =
                Test.check2 description
                    transformationGenerator
                    Random.cylinder3d
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
