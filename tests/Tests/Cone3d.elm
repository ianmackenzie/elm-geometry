module Tests.Cone3d exposing (suite)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Circle3d exposing (Circle3d)
import Cone3d exposing (Cone3d)
import Expect
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Test exposing (Test)
import Vector3d exposing (Vector3d)


type alias Transformation coordinates =
    { cone : Cone3d Meters coordinates -> Cone3d Meters coordinates
    , point : Point3d Meters coordinates -> Point3d Meters coordinates
    , circle : Circle3d Meters coordinates -> Circle3d Meters coordinates
    }


rotation : Axis3d Meters coordinates -> Angle -> Transformation coordinates
rotation axis angle =
    { cone = Cone3d.rotateAround axis angle
    , point = Point3d.rotateAround axis angle
    , circle = Circle3d.rotateAround axis angle
    }


translation : Vector3d Meters coordinates -> Transformation coordinates
translation displacement =
    { cone = Cone3d.translateBy displacement
    , point = Point3d.translateBy displacement
    , circle = Circle3d.translateBy displacement
    }


scaling : Point3d Meters coordinates -> Float -> Transformation coordinates
scaling centerPoint scale =
    { cone = Cone3d.scaleAbout centerPoint scale
    , point = Point3d.scaleAbout centerPoint scale
    , circle = Circle3d.scaleAbout centerPoint scale
    }


mirroring : Plane3d Meters coordinates -> Transformation coordinates
mirroring plane =
    { cone = Cone3d.mirrorAcross plane
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


coneAndPoint : Fuzzer ( Cone3d Meters coordinates, Point3d Meters coordinates )
coneAndPoint =
    Fuzz.map4
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
        Fuzz.cone3d
        Fuzz.parameterValue
        Fuzz.parameterValue
        Fuzz.angle


suite : Test
suite =
    Test.describe "Cone3d"
        [ Test.fuzz2
            coneAndPoint
            transformationFuzzer
            "Point containment is consistent"
            (\( cone, point ) transformation ->
                let
                    initialContainment =
                        Cone3d.contains point cone

                    transformedPoint =
                        transformation.point point

                    transformedcone =
                        transformation.cone cone

                    finalContainment =
                        Cone3d.contains transformedPoint transformedcone
                in
                finalContainment |> Expect.equal initialContainment
            )
        , Test.fuzz2
            transformationFuzzer
            Fuzz.cone3d
            "Base is consistent through transformation"
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
