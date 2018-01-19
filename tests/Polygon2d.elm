module Polygon2d
    exposing
        ( jsonRoundTrips
        , fromConvexHullTest
        )

import Expect
import Generic
import OpenSolid.Geometry.Decode as Decode
import OpenSolid.Geometry.Encode as Encode
import OpenSolid.Geometry.Fuzz as Fuzz
import OpenSolid.Point2d as Point2d
import OpenSolid.Polygon2d as Polygon2d
import Test exposing (Test)


jsonRoundTrips : Test
jsonRoundTrips =
    Generic.jsonRoundTrips Fuzz.polygon2d
        Encode.polygon2d
        Decode.polygon2d


fromConvexHullTest : Test
fromConvexHullTest =
    Test.test "returns the convex hull of a list of points" <|
        \() ->
            [ Point2d.fromCoordinates ( 200, 200 )
            , Point2d.fromCoordinates ( 760, 300 )
            , Point2d.fromCoordinates ( 500, 500 )
            , Point2d.fromCoordinates ( 400, 400 )
            ]
                |> Polygon2d.fromConvexHull
                |> Expect.equal
                    (Polygon2d.fromVertices
                        [ Point2d.fromCoordinates ( 200, 200 )
                        , Point2d.fromCoordinates ( 500, 500 )
                        , Point2d.fromCoordinates ( 760, 300 )
                        ]
                    )
