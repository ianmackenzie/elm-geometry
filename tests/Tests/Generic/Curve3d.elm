module Tests.Generic.Curve3d exposing (GlobalCoordinates, LocalCoordinates, Operations, transformations)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Test exposing (Test)
import Vector3d exposing (Vector3d)


type GlobalCoordinates
    = GlobalCoordinates


type LocalCoordinates
    = LocalCoordinates


type alias Operations curve coordinates =
    { fuzzer : Fuzzer curve
    , pointOn : curve -> Float -> Point3d Meters coordinates
    , firstDerivative : curve -> Float -> Vector3d Meters coordinates
    , scaleAbout : Point3d Meters coordinates -> Float -> curve -> curve
    , translateBy : Vector3d Meters coordinates -> curve -> curve
    , rotateAround : Axis3d Meters coordinates -> Angle -> curve -> curve
    , mirrorAcross : Plane3d Meters coordinates -> curve -> curve
    }


transformations :
    Operations globalCurve GlobalCoordinates
    -> Operations localCurve LocalCoordinates
    -> (Frame3d Meters GlobalCoordinates { defines : LocalCoordinates } -> localCurve -> globalCurve)
    -> (Frame3d Meters GlobalCoordinates { defines : LocalCoordinates } -> globalCurve -> localCurve)
    -> Test
transformations global local placeIn relativeTo =
    Test.describe "Transformations"
        [ Test.fuzz3
            global.fuzzer
            (Fuzz.tuple ( Fuzz.point3d, Fuzz.scale ))
            Fuzz.parameterValue
            "scaleAbout"
            (\curve ( basePoint, scale ) t ->
                let
                    scaledCurve =
                        global.scaleAbout basePoint scale curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnScaledCurve =
                        global.pointOn scaledCurve t

                    scaledPoint =
                        originalPoint |> Point3d.scaleAbout basePoint scale
                in
                pointOnScaledCurve |> Expect.point3d scaledPoint
            )
        , Test.fuzz3
            global.fuzzer
            Fuzz.vector3d
            Fuzz.parameterValue
            "translateBy"
            (\curve displacement t ->
                let
                    translatedCurve =
                        global.translateBy displacement curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnTranslatedCurve =
                        global.pointOn translatedCurve t

                    translatedPoint =
                        originalPoint |> Point3d.translateBy displacement
                in
                pointOnTranslatedCurve |> Expect.point3d translatedPoint
            )
        , Test.fuzz3
            global.fuzzer
            (Fuzz.tuple
                ( Fuzz.axis3d
                , Fuzz.map Angle.radians (Fuzz.floatRange (-2 * pi) (2 * pi))
                )
            )
            Fuzz.parameterValue
            "rotateAround"
            (\curve ( axis, angle ) t ->
                let
                    rotatedCurve =
                        global.rotateAround axis angle curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnRotatedCurve =
                        global.pointOn rotatedCurve t

                    rotatedPoint =
                        originalPoint |> Point3d.rotateAround axis angle
                in
                pointOnRotatedCurve |> Expect.point3d rotatedPoint
            )
        , Test.fuzz3
            global.fuzzer
            Fuzz.plane3d
            Fuzz.parameterValue
            "mirrorAcross"
            (\curve plane t ->
                let
                    mirroredCurve =
                        global.mirrorAcross plane curve

                    originalPoint =
                        global.pointOn curve t

                    pointOnMirroredCurve =
                        global.pointOn mirroredCurve t

                    mirroredPoint =
                        originalPoint |> Point3d.mirrorAcross plane
                in
                pointOnMirroredCurve |> Expect.point3d mirroredPoint
            )
        , Test.fuzz3
            global.fuzzer
            Fuzz.frame3d
            Fuzz.parameterValue
            "relativeTo"
            (\globalCurve frame t ->
                let
                    localCurve =
                        relativeTo frame globalCurve

                    originalPoint =
                        global.pointOn globalCurve t

                    pointOnLocalCurve =
                        local.pointOn localCurve t

                    localPoint =
                        originalPoint |> Point3d.relativeTo frame
                in
                pointOnLocalCurve |> Expect.point3d localPoint
            )
        , Test.fuzz3
            local.fuzzer
            Fuzz.frame3d
            Fuzz.parameterValue
            "placeIn"
            (\localCurve frame t ->
                let
                    globalCurve =
                        placeIn frame localCurve

                    originalPoint =
                        local.pointOn localCurve t

                    pointOnGlobalCurve =
                        global.pointOn globalCurve t

                    globalPoint =
                        originalPoint |> Point3d.placeIn frame
                in
                pointOnGlobalCurve |> Expect.point3d globalPoint
            )
        ]
