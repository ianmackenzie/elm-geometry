module Approximate exposing (..)

import Expect
import OpenSolid.Geometry.SegmentTree2d as SegmentTree2d exposing (..)
import OpenSolid.LineSegment2d as LineSegment2d exposing (..)
import OpenSolid.Point2d as Point2d
import Test exposing (Test, describe, test)


segments =
    [ LineSegment2d.fromEndpoints ( Point2d.fromCoordinates ( 0, 1 ), Point2d.fromCoordinates ( 5, 1 ) )
    , LineSegment2d.fromEndpoints ( Point2d.fromCoordinates ( 5, 1 ), Point2d.fromCoordinates ( 10, 1 ) )
    , LineSegment2d.fromEndpoints ( Point2d.fromCoordinates ( 10, 1 ), Point2d.fromCoordinates ( 20, 1 ) )
    ]


point2d =
    Point2d.fromCoordinates


linesegment2d =
    LineSegment2d.fromEndpoints


balancedTree =
    describe "balanced tree"
        [ test "correct lengths at start, split and end for trivial segments" <|
            \_ ->
                let
                    expected =
                        Node { lengthAtSplit = 5, lengthAtEnd = 10 }
                            2
                            (Leaf (linesegment2d ( point2d ( 0, 1 ), point2d ( 5, 1 ) )))
                            (Leaf (linesegment2d ( point2d ( 5, 1 ), point2d ( 10, 1 ) )))
                in
                SegmentTree2d.fromList (List.take 2 segments)
                    |> Expect.equal (Just expected)
        ]
