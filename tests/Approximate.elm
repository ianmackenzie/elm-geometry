module Approximate exposing (..)

import Expect
import OpenSolid.Geometry.SegmentTree2d as SegmentTree2d exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.LineSegment2d as LineSegment2d exposing (..)
import Test exposing (Test, describe, test)


segments =
    [ LineSegment2d.withEndpoints ( Point2d.withCoordinates ( 0, 1 ), Point2d.withCoordinates ( 5, 1 ) )
    , LineSegment2d.withEndpoints ( Point2d.withCoordinates ( 5, 1 ), Point2d.withCoordinates ( 10, 1 ) )
    , LineSegment2d.withEndpoints ( Point2d.withCoordinates ( 10, 1 ), Point2d.withCoordinates ( 20, 1 ) )
    ]


point2d =
    Point2d.withCoordinates


linesegment2d =
    LineSegment2d.withEndpoints


balancedTree =
    describe "balanced tree"
        [ test "insertInTree produces a balanced tree" <|
            \_ ->
                let
                    expected =
                        Node {}
                            11
                            (Node {}
                                5
                                (Node {}
                                    4
                                    (Node {}
                                        3
                                        (Node {} 2 (Leaf 0) (Leaf 1))
                                        (Node {} 2 (Leaf 2) (Leaf 3))
                                    )
                                    (Node {} 2 (Leaf 4) (Leaf 5))
                                )
                                (Node {}
                                    3
                                    (Node {} 2 (Leaf 6) (Leaf 7))
                                    (Node {} 2 (Leaf 8) (Leaf 9))
                                )
                            )
                            (Node {}
                                5
                                (Node {}
                                    4
                                    (Node {}
                                        3
                                        (Node {} 2 (Leaf 10) (Leaf 11))
                                        (Node {} 2 (Leaf 12) (Leaf 13))
                                    )
                                    (Node {} 2 (Leaf 14) (Leaf 15))
                                )
                                (Node {}
                                    3
                                    (Node {} 2 (Leaf 16) (Leaf 17))
                                    (Node {}
                                        2
                                        (Leaf 18)
                                        (Node {} 1 (Leaf 19) (Leaf 20))
                                    )
                                )
                            )
                in
                    List.foldl (SegmentTree2d.extendRight_ (\x y -> Node {} 1 (Leaf x) (Leaf y))) (Leaf 0) (List.range 1 20)
                        |> Expect.equal expected
        , test "correct lengths at start, split and end" <|
            \_ ->
                let
                    expected =
                        (Node { lengthAtSplit = 5, lengthAtEnd = 20 }
                            3
                            (Leaf (linesegment2d ( point2d ( 0, 1 ), point2d ( 5, 1 ) )))
                            (Node { lengthAtSplit = 5, lengthAtEnd = 15 }
                                2
                                (Leaf (linesegment2d ( point2d ( 5, 1 ), point2d ( 10, 1 ) )))
                                (Leaf (linesegment2d ( point2d ( 10, 1 ), point2d ( 20, 1 ) )))
                            )
                        )
                in
                    buildTree segments
                        |> Expect.equal (Just expected)
        , test "correct lengths at start, split and end for trivial segments" <|
            \_ ->
                let
                    expected =
                        (Node { lengthAtSplit = 5, lengthAtEnd = 10 }
                            2
                            (Leaf (linesegment2d ( point2d ( 0, 1 ), point2d ( 5, 1 ) )))
                            (Leaf (linesegment2d ( point2d ( 5, 1 ), point2d ( 10, 1 ) )))
                        )
                in
                    buildTree (List.take 2 segments)
                        |> Expect.equal (Just expected)
        ]