module Tests.Generic.Curve2d exposing (Config, transformations)

import Axis2d exposing (Axis2d)
import Frame2d exposing (Frame2d)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d exposing (Point2d)
import Test exposing (Test)
import Vector2d exposing (Vector2d)


type alias Config curve =
    { fuzzer : Fuzzer curve
    , pointOn : curve -> Float -> Point2d
    , derivative : curve -> Float -> Vector2d
    , scaleAbout : Point2d -> Float -> curve -> curve
    , translateBy : Vector2d -> curve -> curve
    , rotateAround : Point2d -> Float -> curve -> curve
    , mirrorAcross : Axis2d -> curve -> curve
    , relativeTo : Frame2d -> curve -> curve
    , placeIn : Frame2d -> curve -> curve
    }


parameterValue : Fuzzer Float
parameterValue =
    Fuzz.floatRange 0 1


transformations : Config curve -> Test
transformations config =
    Test.describe "Transformations"
        [ Test.describe "scaleAbout"
            [ Test.fuzz4
                config.fuzzer
                Fuzz.point2d
                Fuzz.scalar
                parameterValue
                "position"
                (\curve basePoint scale t ->
                    let
                        scaledCurve =
                            config.scaleAbout basePoint scale curve

                        originalPoint =
                            config.pointOn curve t

                        pointOnScaledCurve =
                            config.pointOn scaledCurve t

                        scaledPoint =
                            Point2d.scaleAbout basePoint scale originalPoint
                    in
                    pointOnScaledCurve |> Expect.point2d scaledPoint
                )
            , Test.fuzz4
                config.fuzzer
                Fuzz.point2d
                Fuzz.scalar
                parameterValue
                "derivative"
                (\curve basePoint scale t ->
                    let
                        scaledCurve =
                            config.scaleAbout basePoint scale curve

                        originalDerivative =
                            config.derivative curve t

                        derivativeOfScaledCurve =
                            config.derivative scaledCurve t

                        scaledDerivative =
                            Vector2d.scaleBy scale originalDerivative
                    in
                    derivativeOfScaledCurve |> Expect.vector2d scaledDerivative
                )
            ]
        , Test.fuzz3
            config.fuzzer
            Fuzz.vector2d
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
                        Point2d.translateBy displacement originalPoint
                in
                pointOnTranslatedCurve |> Expect.point2d translatedPoint
            )
        , Test.fuzz4
            config.fuzzer
            Fuzz.point2d
            (Fuzz.floatRange (-2 * pi) (2 * pi))
            parameterValue
            "rotateAround"
            (\curve centerPoint angle t ->
                let
                    rotatedCurve =
                        config.rotateAround centerPoint angle curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnRotatedCurve =
                        config.pointOn rotatedCurve t

                    rotatedPoint =
                        Point2d.rotateAround centerPoint angle originalPoint
                in
                pointOnRotatedCurve |> Expect.point2d rotatedPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.axis2d
            parameterValue
            "mirrorAcross"
            (\curve axis t ->
                let
                    mirroredCurve =
                        config.mirrorAcross axis curve

                    originalPoint =
                        config.pointOn curve t

                    pointOnMirroredCurve =
                        config.pointOn mirroredCurve t

                    mirroredPoint =
                        Point2d.mirrorAcross axis originalPoint
                in
                pointOnMirroredCurve |> Expect.point2d mirroredPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.frame2d
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
                        Point2d.relativeTo frame originalPoint
                in
                pointOnLocalCurve |> Expect.point2d localPoint
            )
        , Test.fuzz3
            config.fuzzer
            Fuzz.frame2d
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

                    localPoint =
                        Point2d.placeIn frame originalPoint
                in
                pointOnGlobalCurve |> Expect.point2d localPoint
            )
        ]
