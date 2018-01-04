module Generic.Curve2d
    exposing
        ( Config
        , rotation
        , scaling
        , translation
        )

import Fuzz exposing (Fuzzer)
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
