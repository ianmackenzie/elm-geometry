module Generic.Curve2d
    exposing
        ( Config
        , globalization
        , localization
        , rotation
        , scaling
        , translation
        )

import Fuzz exposing (Fuzzer)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Test exposing (Test)


type alias Config curve =
    { fuzzer : Fuzzer curve
    , pointOn : curve -> Float -> Point2d
    , derivative : curve -> Float -> Vector2d
    }


parameterValue : Fuzzer Float
parameterValue =
    Fuzz.floatRange 0 1


scaling : Config curve -> (Point2d -> Float -> curve -> curve) -> Test
scaling config scaleAbout =
    Test.fuzz4
        config.fuzzer
        Fuzz.point2d
        Fuzz.scalar
        parameterValue
        "scaleAbout"
        (\curve basePoint scale t ->
            let
                scaledCurve =
                    scaleAbout basePoint scale curve

                originalPoint =
                    config.pointOn curve t

                pointOnScaledCurve =
                    config.pointOn scaledCurve t

                scaledPoint =
                    Point2d.scaleAbout basePoint scale originalPoint
            in
            pointOnScaledCurve |> Expect.point2d scaledPoint
        )


translation : Config curve -> (Vector2d -> curve -> curve) -> Test
translation config translateBy =
    Test.fuzz3
        config.fuzzer
        Fuzz.vector2d
        parameterValue
        "translateBy"
        (\curve displacement t ->
            let
                translatedCurve =
                    translateBy displacement curve

                originalPoint =
                    config.pointOn curve t

                pointOnTranslatedCurve =
                    config.pointOn translatedCurve t

                translatedPoint =
                    Point2d.translateBy displacement originalPoint
            in
            pointOnTranslatedCurve |> Expect.point2d translatedPoint
        )


rotation : Config curve -> (Point2d -> Float -> curve -> curve) -> Test
rotation config rotateAround =
    Test.fuzz4
        config.fuzzer
        Fuzz.point2d
        (Fuzz.floatRange (-2 * pi) (2 * pi))
        parameterValue
        "rotateAround"
        (\curve centerPoint angle t ->
            let
                rotatedCurve =
                    rotateAround centerPoint angle curve

                originalPoint =
                    config.pointOn curve t

                pointOnRotatedCurve =
                    config.pointOn rotatedCurve t

                rotatedPoint =
                    Point2d.rotateAround centerPoint angle originalPoint
            in
            pointOnRotatedCurve |> Expect.point2d rotatedPoint
        )


localization : Config curve -> (Frame2d -> curve -> curve) -> Test
localization config relativeTo =
    Test.fuzz3
        config.fuzzer
        Fuzz.frame2d
        parameterValue
        "relativeTo"
        (\curve frame t ->
            let
                localCurve =
                    relativeTo frame curve

                originalPoint =
                    config.pointOn curve t

                pointOnLocalCurve =
                    config.pointOn localCurve t

                localPoint =
                    Point2d.relativeTo frame originalPoint
            in
            pointOnLocalCurve |> Expect.point2d localPoint
        )


globalization : Config curve -> (Frame2d -> curve -> curve) -> Test
globalization config placeIn =
    Test.fuzz3
        config.fuzzer
        Fuzz.frame2d
        parameterValue
        "placeIn"
        (\curve frame t ->
            let
                globalCurve =
                    placeIn frame curve

                originalPoint =
                    config.pointOn curve t

                pointOnGlobalCurve =
                    config.pointOn globalCurve t

                localPoint =
                    Point2d.placeIn frame originalPoint
            in
            pointOnGlobalCurve |> Expect.point2d localPoint
        )
