module Generic.Curve2d
    exposing
        ( rotation
        , scaling
        , translation
        )

import Fuzz exposing (Fuzzer)
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Test exposing (Test)


parameterValue : Fuzzer Float
parameterValue =
    Fuzz.floatRange 0 1


scaling : Fuzzer a -> (Point2d -> Float -> a -> a) -> (a -> Float -> Point2d) -> Test
scaling curveFuzzer scaleAbout pointOn =
    Test.fuzz4
        curveFuzzer
        Fuzz.point2d
        Fuzz.scalar
        parameterValue
        "scaleAbout"
        (\curve basePoint scale t ->
            let
                scaledCurve =
                    scaleAbout basePoint scale curve

                originalPoint =
                    pointOn curve t

                pointOnScaledCurve =
                    pointOn scaledCurve t

                scaledPoint =
                    Point2d.scaleAbout basePoint scale originalPoint
            in
            pointOnScaledCurve |> Expect.point2d scaledPoint
        )


translation : Fuzzer a -> (Vector2d -> a -> a) -> (a -> Float -> Point2d) -> Test
translation curveFuzzer translateBy pointOn =
    Test.fuzz3
        curveFuzzer
        Fuzz.vector2d
        parameterValue
        "translateBy"
        (\curve displacement t ->
            let
                translatedCurve =
                    translateBy displacement curve

                originalPoint =
                    pointOn curve t

                pointOnTranslatedCurve =
                    pointOn translatedCurve t

                translatedPoint =
                    Point2d.translateBy displacement originalPoint
            in
            pointOnTranslatedCurve |> Expect.point2d translatedPoint
        )


rotation : Fuzzer a -> (Point2d -> Float -> a -> a) -> (a -> Float -> Point2d) -> Test
rotation curveFuzzer rotateAround pointOn =
    Test.fuzz4
        curveFuzzer
        Fuzz.point2d
        (Fuzz.floatRange (-2 * pi) (2 * pi))
        parameterValue
        "rotateAround"
        (\curve centerPoint angle t ->
            let
                rotatedCurve =
                    rotateAround centerPoint angle curve

                originalPoint =
                    pointOn curve t

                pointOnRotatedCurve =
                    pointOn rotatedCurve t

                rotatedPoint =
                    Point2d.rotateAround centerPoint angle originalPoint
            in
            pointOnRotatedCurve |> Expect.point2d rotatedPoint
        )
