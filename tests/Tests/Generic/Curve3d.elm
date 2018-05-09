module Tests.Generic.Curve3d exposing (Config, transformations)

import Axis3d exposing (Axis3d)
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Test exposing (Test)
import Vector3d exposing (Vector3d)


type alias Config curve =
    { fuzzer : Fuzzer curve
    , pointOn : curve -> Float -> Maybe Point3d
    , derivative : curve -> Float -> Maybe Vector3d
    , scaleAbout : Point3d -> Float -> curve -> curve
    , translateBy : Vector3d -> curve -> curve
    , rotateAround : Axis3d -> Float -> curve -> curve
    , mirrorAcross : Plane3d -> curve -> curve
    , relativeTo : Frame3d -> curve -> curve
    , placeIn : Frame3d -> curve -> curve
    }


parameterValue : Fuzzer Float
parameterValue =
    Fuzz.floatRange 0 1


transformations : Config curve -> Test
transformations config =
    Test.describe "Transformations"
        [ Test.fuzz4
            config.fuzzer
            Fuzz.point3d
            Fuzz.scalar
            parameterValue
            "scaleAbout"
            (\curve basePoint scale t ->
                let
                    scaledCurve =
                        config.scaleAbout basePoint scale curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnScaledCurve =
                        config.pointOn scaledCurve t

                    scaledPoint =
                        originalPoint
                            |> Maybe.map
                                (Point3d.scaleAbout basePoint scale)
                in
                pointOnScaledCurve
                    |> Expect.maybe Expect.point3d scaledPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.vector3d
            parameterValue
            "translateBy"
            (\curve displacement t ->
                let
                    translatedCurve =
                        config.translateBy displacement curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnTranslatedCurve =
                        config.pointOn translatedCurve t

                    translatedPoint =
                        originalPoint
                            |> Maybe.map (Point3d.translateBy displacement)
                in
                pointOnTranslatedCurve
                    |> Expect.maybe Expect.point3d translatedPoint
            )
        , Test.fuzz4
            config.fuzzer
            Fuzz.axis3d
            (Fuzz.floatRange (-2 * pi) (2 * pi))
            parameterValue
            "rotateAround"
            (\curve axis angle t ->
                let
                    rotatedCurve =
                        config.rotateAround axis angle curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnRotatedCurve =
                        config.pointOn rotatedCurve t

                    rotatedPoint =
                        originalPoint
                            |> Maybe.map
                                (Point3d.rotateAround axis angle)
                in
                pointOnRotatedCurve |> Expect.maybe Expect.point3d rotatedPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.plane3d
            parameterValue
            "mirrorAcross"
            (\curve plane t ->
                let
                    mirroredCurve =
                        config.mirrorAcross plane curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnMirroredCurve =
                        config.pointOn mirroredCurve t

                    mirroredPoint =
                        originalPoint |> Maybe.map (Point3d.mirrorAcross plane)
                in
                pointOnMirroredCurve
                    |> Expect.maybe Expect.point3d mirroredPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.frame3d
            parameterValue
            "relativeTo"
            (\curve frame t ->
                let
                    localCurve =
                        config.relativeTo frame curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnLocalCurve =
                        config.pointOn localCurve t

                    localPoint =
                        originalPoint |> Maybe.map (Point3d.relativeTo frame)
                in
                pointOnLocalCurve |> Expect.maybe Expect.point3d localPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.frame3d
            parameterValue
            "placeIn"
            (\curve frame t ->
                let
                    globalCurve =
                        config.placeIn frame curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnGlobalCurve =
                        config.pointOn globalCurve t

                    globalPoint =
                        originalPoint |> Maybe.map (Point3d.placeIn frame)
                in
                pointOnGlobalCurve |> Expect.maybe Expect.point3d globalPoint
            )
        ]
