module Tests.Cone3d exposing (suite)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Circle3d exposing (Circle3d)
import Cone3d exposing (Cone3d)
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
    { cone : Cone3d Meters coordinates -> Cone3d Meters coordinates
    , point : Point3d Meters coordinates -> Point3d Meters coordinates
    , circle : Circle3d Meters coordinates -> Circle3d Meters coordinates
    , isZeroScale : Bool
    }


rotation : Axis3d Meters coordinates -> Angle -> Transformation coordinates
rotation axis angle =
    { cone = Cone3d.rotateAround axis angle
    , point = Point3d.rotateAround axis angle
    , circle = Circle3d.rotateAround axis angle
    , isZeroScale = False
    }


translation : Vector3d Meters coordinates -> Transformation coordinates
translation displacement =
    { cone = Cone3d.translateBy displacement
    , point = Point3d.translateBy displacement
    , circle = Circle3d.translateBy displacement
    , isZeroScale = False
    }


scaling : Point3d Meters coordinates -> Float -> Transformation coordinates
scaling centerPoint scale =
    { cone = Cone3d.scaleAbout centerPoint scale
    , point = Point3d.scaleAbout centerPoint scale
    , circle = Circle3d.scaleAbout centerPoint scale
    , isZeroScale = scale == 0.0
    }


mirroring : Plane3d Meters coordinates -> Transformation coordinates
mirroring plane =
    { cone = Cone3d.mirrorAcross plane
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


coneAndPoint : Generator ( Cone3d Meters coordinates, Point3d Meters coordinates )
coneAndPoint =
    Random.map4
        (\cone u v theta ->
            let
                halfLength =
                    Quantity.half (Cone3d.length cone)

                minZ =
                    Quantity.multiplyBy -1.25 halfLength

                maxZ =
                    Quantity.multiplyBy 1.25 halfLength

                radius =
                    Cone3d.radius cone

                coneFrame =
                    Frame3d.fromZAxis (Cone3d.axis cone)

                z =
                    Quantity.interpolateFrom minZ maxZ u

                r =
                    Quantity.sqrt (Quantity.multiplyBy (v * 1.25) (Quantity.squared radius))

                x =
                    r |> Quantity.multiplyBy (Angle.cos theta)

                y =
                    r |> Quantity.multiplyBy (Angle.sin theta)
            in
            ( cone, Point3d.xyzIn coneFrame x y z )
        )
        Random.cone3d
        Random.parameterValue
        Random.parameterValue
        Random.angle


suite : Test
suite =
    Test.describe "Cone3d"
        [ Test.check2 "Point containment is consistent"
            coneAndPoint
            transformationGenerator
            (\( cone, point ) transformation ->
                let
                    initialContainment =
                        Cone3d.contains point cone

                    transformedPoint =
                        transformation.point point

                    transformedCone =
                        transformation.cone cone

                    finalContainment =
                        Cone3d.contains transformedPoint transformedCone
                in
                if transformation.isZeroScale then
                    finalContainment |> Expect.equal True

                else
                    finalContainment |> Expect.equal initialContainment
            )
        , Test.check2 "Base is consistent through transformation"
            transformationGenerator
            Random.cone3d
            (\transformation cone ->
                let
                    base =
                        Cone3d.base cone

                    transformedCone =
                        transformation.cone cone

                    transformedBase =
                        transformation.circle base

                    baseOfTransformed =
                        Cone3d.base transformedCone
                in
                baseOfTransformed |> Expect.circle3d transformedBase
            )
        ]
