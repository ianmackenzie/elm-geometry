module QuadraticSpline2d
    exposing
        ( jsonRoundTrips
        , parameterization
        )

import Expect
import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point2d as Point2d
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d
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
        , Test.fuzz Fuzz.quadraticSpline2d "at s = 0 the arcLengthParameterization gives the starting point" <|
            \spline ->
                QuadraticSpline2d.arcLengthParameterization 0.001 spline 0
                    |> Expect.equal (Just <| QuadraticSpline2d.startPoint spline)

        {- does not work - fails, but the shrinker is inefficient taking gigs of ram trying to shrink
           , Test.fuzz Fuzz.quadraticSpline2d "at s = (length spline) the arcLengthParameterization gives the end point" <|
               \spline ->
                   let
                       error =
                           1.0e-6
                   in
                       QuadraticSpline2d.approximateLength error spline
                           |> QuadraticSpline2d.arcLengthParameterization error spline
                           |> Expect.equal (Just <| QuadraticSpline2d.endPoint spline)
        -}
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
                        Just (Point2d.withCoordinates ( 2.901568908789561, 2.4953860425995105 ))
                in
                QuadraticSpline2d.arcLengthParameterization 0.001 exampleSpline 2.5
                    |> Expect.equal expected
        , Test.test "length is as expected" <|
            \_ ->
                QuadraticSpline2d.approximateLength 0.001 line
                    |> Expect.equal 5
        , Test.test "length parameterization at approximate length is Just" <|
            \_ ->
                QuadraticSpline2d.approximateLength 0.001 exampleSpline
                    |> Debug.log "arc length"
                    |> QuadraticSpline2d.arcLengthParameterization 0.001 exampleSpline
                    |> Expect.equal (Just (Point2d.withCoordinates ( 0, 0 )))
        ]


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.quadraticSpline2d
        Encode.quadraticSpline2d
        Decode.quadraticSpline2d
