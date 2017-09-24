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
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import Test exposing (Test)


exampleSpline : QuadraticSpline2d
exampleSpline =
    QuadraticSpline2d.fromControlPoints
        ( Point2d.fromCoordinates ( 1, 1 )
        , Point2d.fromCoordinates ( 3, 4 )
        , Point2d.fromCoordinates ( 5, 1 )
        )


line : QuadraticSpline2d
line =
    QuadraticSpline2d.fromControlPoints
        ( Point2d.fromCoordinates ( 0, 1 )
        , Point2d.fromCoordinates ( 2.5, 1 )
        , Point2d.fromCoordinates ( 5, 1 )
        )


parameterization : Test
parameterization =
    Test.describe "arc length parameterization"
        [ Test.test "at s=0 the result is the starting point of the spline" <|
            \_ ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.001
                            exampleSpline
                in
                QuadraticSpline2d.pointAtLength parameterization 0
                    |> Expect.equal (Just (Point2d.fromCoordinates ( 1, 1 )))
        , Test.fuzz Fuzz.quadraticSpline2d "at s = 0 the arcLengthParameterization gives the starting point" <|
            \spline ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.001
                            spline

                    startPoint =
                        QuadraticSpline2d.startPoint spline
                in
                QuadraticSpline2d.pointAtLength parameterization 0
                    |> Expect.equal (Just startPoint)
        , Test.fuzz Fuzz.quadraticSpline2d "at s = (length spline) the arcLengthParameterization gives the end point" <|
            \spline ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.001 spline

                    endPoint =
                        QuadraticSpline2d.endPoint spline
                in
                QuadraticSpline2d.arcLength parameterization
                    |> QuadraticSpline2d.pointAtLength parameterization
                    |> Expect.equal (Just endPoint)
        , Test.fuzz Fuzz.quadraticSpline2d "at s = (length spline) parameterValueAtLength gives 1" <|
            \spline ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.001 spline
                in
                QuadraticSpline2d.arcLength parameterization
                    |> QuadraticSpline2d.parameterValueAtLength parameterization
                    |> Expect.equal (Just 1)
        , Test.test "at s=5 the result is the end point of the line" <|
            \_ ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.1 line
                in
                QuadraticSpline2d.pointAtLength parameterization 5
                    |> Expect.equal (Just (Point2d.fromCoordinates ( 5, 1 )))
        , Test.test "at s=2.5 the result is the midpoint of the line" <|
            \_ ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.1 line
                in
                QuadraticSpline2d.pointAtLength parameterization 2.5
                    |> Expect.equal (Just (Point2d.fromCoordinates ( 2.5, 1 )))
        , Test.test "at s=2.5 is about right" <|
            \_ ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.001
                            exampleSpline

                    expected =
                        Just (Point2d.fromCoordinates ( 2.900751969936743, 2.496306185698211 ))
                in
                QuadraticSpline2d.pointAtLength parameterization 2.5
                    |> Expect.equal expected
        , Test.test "length is as expected" <|
            \_ ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.001 line
                in
                QuadraticSpline2d.arcLength parameterization
                    |> Expect.equal 5
        , Test.test "length parameterization at approximate length is Just" <|
            \_ ->
                let
                    parameterization =
                        QuadraticSpline2d.arcLengthParameterization 0.001
                            exampleSpline

                    arcLength =
                        QuadraticSpline2d.arcLength parameterization
                in
                QuadraticSpline2d.pointAtLength parameterization arcLength
                    |> Expect.equal (Just (Point2d.fromCoordinates ( 5, 1 )))
        ]


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.quadraticSpline2d
        Encode.quadraticSpline2d
        Decode.quadraticSpline2d
