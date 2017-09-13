module QuadraticSpline2d
    exposing
        ( jsonRoundTrips
        , parameterization
        )

import Expect
import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Expect as Expect
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d
import OpenSolid.Point2d as Point2d
import Test exposing (Test)


exampleSpline =
    QuadraticSpline2d.withControlPoints
        ( Point2d.withCoordinates ( 1, 1 )
        , Point2d.withCoordinates ( 3, 4 )
        , Point2d.withCoordinates ( 5, 1 )
        )


line =
    QuadraticSpline2d.withControlPoints
        ( Point2d.withCoordinates ( 0, 1 )
        , Point2d.withCoordinates ( 3, 1 )
        , Point2d.withCoordinates ( 5, 1 )
        )


parameterization =
    Test.describe "arc length parameterization"
        [ Test.test "at s=0 the result is the starting point of the spline" <|
            \_ ->
                QuadraticSpline2d.arcLengthParameterization 0.001 exampleSpline 0
                    |> Expect.equal (Just (Point2d.withCoordinates ( 1, 1 )))
        , Test.test "at s=5 the result is the starting point of the spline" <|
            \_ ->
                QuadraticSpline2d.arcLengthParameterization 0.001 line 5
                    |> Expect.equal (Just (Point2d.withCoordinates ( 5, 1 )))
        , Test.test "at s=2.5 the result is the starting point of the spline" <|
            \_ ->
                QuadraticSpline2d.arcLengthParameterization 0.1 line 2.5
                    |> Expect.equal (Just (Point2d.withCoordinates ( 2.5, 1 )))
        , Test.test "at s=2.5 is about right" <|
            \_ ->
                let
                    expected =
                        Just (Point2d.withCoordinates ( 3.5195982005959814, 2.39798200912357 ))
                in
                    QuadraticSpline2d.arcLengthParameterization 0.001 exampleSpline 2.5
                        |> Expect.equal expected
        , Test.test "length is as expected" <|
            \_ ->
                QuadraticSpline2d.approximateLength 0.001 line
                    |> Expect.equal 5
        ]


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.quadraticSpline2d
        Encode.quadraticSpline2d
        Decode.quadraticSpline2d
