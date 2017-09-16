module OpenSolid.Geometry.SegmentTree2d exposing (..)

import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)


type alias SegmentTree =
    Tree NodeValue LineSegment2d


type alias Height =
    Int


type Tree node leaf
    = Node node Height (Tree node leaf) (Tree node leaf)
    | Leaf leaf


type alias NodeValue =
    { lengthAtStart : Float, lengthAtSplit : Float, lengthAtEnd : Float }


leavesToNode : LineSegment2d -> LineSegment2d -> SegmentTree
leavesToNode segment segment2 =
    Node
        { lengthAtStart = 0
        , lengthAtSplit = LineSegment2d.length segment
        , lengthAtEnd = LineSegment2d.length segment + LineSegment2d.length segment2
        }
        2
        (Leaf segment)
        (Leaf segment2)


getHeight : Tree a b -> Int
getHeight tree =
    case tree of
        Leaf _ ->
            1

        Node _ height _ _ ->
            height


insertInTree : (leaf -> leaf -> Tree node leaf) -> leaf -> Tree node leaf -> Tree node leaf
insertInTree combine segment tree =
    case tree of
        Leaf segment2 ->
            flip combine segment segment2

        Node attributes height leftSubtree rightSubtree ->
            Node attributes (height + 1) leftSubtree (insertInTree combine segment rightSubtree)
                |> balance


tree : node -> Tree node a -> Tree node a -> Tree node a
tree item left right =
    let
        maxHeight : Int
        maxHeight =
            max
                (getHeight left)
                (getHeight right)
                |> (+) 1
    in
        Node item maxHeight left right


{-| Rotate a tree to the left (for balancing).
-}
rotateLeft set =
    case set of
        Node root _ less (Node pivot _ between greater) ->
            tree pivot (tree root less between) greater

        _ ->
            set


{-| Inversely, rotate a tree to the right (for balancing).
-}
rotateRight set =
    case set of
        Node root _ (Node pivot _ less between) greater ->
            tree pivot less (tree root between greater)

        _ ->
            set


heightDiff : Tree a b -> Int
heightDiff tree =
    case tree of
        Leaf _ ->
            0

        Node _ _ left right ->
            getHeight right - getHeight left


balance : Tree a b -> Tree a b
balance set =
    case set of
        Leaf _ ->
            set

        Node attributes _ left right ->
            if heightDiff set == -1 && heightDiff left > 0 then
                tree attributes (rotateLeft left) right |> rotateRight
            else if heightDiff set < -1 then
                -- Node leaning to the left
                rotateRight set
            else if heightDiff set == 2 && heightDiff right < 0 then
                -- Node leaning to the right with subtree leaning left
                tree attributes left (rotateRight right) |> rotateLeft
            else if heightDiff set > 1 then
                -- Node leaning to the right
                rotateLeft set
            else
                -- Balanced tree
                set


{-| Shift the start, split and end lengths by some amount
-}
shift : Float -> SegmentTree -> SegmentTree
shift amount tree =
    case tree of
        Leaf segment ->
            Leaf segment

        Node { lengthAtStart, lengthAtEnd, lengthAtSplit } height leftBranch rightBranch ->
            Node { lengthAtStart = amount + lengthAtStart, lengthAtEnd = lengthAtEnd + amount, lengthAtSplit = lengthAtSplit + amount }
                height
                (shift amount leftBranch)
                (shift amount rightBranch)


{-| recursivey, calculate the start, split and end lengths based on a node's children
-}
recalculateLengths : SegmentTree -> SegmentTree
recalculateLengths tree =
    case tree of
        Leaf _ ->
            tree

        Node _ height leftBranch rightBranch ->
            case ( recalculateLengths leftBranch, recalculateLengths rightBranch ) of
                ( Node params1 _ _ _, Node params2 _ _ _ ) ->
                    Node
                        { lengthAtStart = params1.lengthAtStart
                        , lengthAtSplit = params1.lengthAtEnd
                        , lengthAtEnd = params1.lengthAtEnd + params2.lengthAtEnd
                        }
                        height
                        leftBranch
                        rightBranch

                ( Node params _ _ _, Leaf segment ) ->
                    Node
                        { lengthAtStart = params.lengthAtStart
                        , lengthAtSplit = params.lengthAtEnd
                        , lengthAtEnd = params.lengthAtEnd + LineSegment2d.length segment
                        }
                        height
                        leftBranch
                        rightBranch

                ( Leaf segment, Node params _ leftsubsub rightsubsub ) ->
                    Node
                        { lengthAtStart = 0
                        , lengthAtSplit = LineSegment2d.length segment
                        , lengthAtEnd = params.lengthAtEnd + LineSegment2d.length segment
                        }
                        height
                        leftBranch
                        (shift (LineSegment2d.length segment) rightBranch)

                ( Leaf segment1, Leaf segment2 ) ->
                    Node
                        { lengthAtStart = 0
                        , lengthAtSplit = LineSegment2d.length segment1
                        , lengthAtEnd = LineSegment2d.length segment1 + LineSegment2d.length segment2
                        }
                        height
                        leftBranch
                        rightBranch


buildTree : List LineSegment2d -> Maybe SegmentTree
buildTree segments =
    case segments of
        [] ->
            Nothing

        x :: xs ->
            List.foldl (insertInTree leavesToNode) (Leaf x) xs
                |> recalculateLengths
                |> Just


evaluateTreeAt : SegmentTree -> Float -> Maybe Point2d
evaluateTreeAt tree s =
    case tree of
        Leaf segment ->
            let
                segmentLength =
                    LineSegment2d.length segment
            in
                if s > segmentLength then
                    Nothing
                else
                    let
                        at =
                            if s == 0 then
                                0
                            else
                                s / segmentLength
                    in
                        at
                            |> LineSegment2d.interpolate segment
                            |> Just

        Node { lengthAtStart, lengthAtSplit, lengthAtEnd } _ leftBranch rightBranch ->
            if s < lengthAtStart || s > lengthAtEnd then
                Nothing
            else if s <= lengthAtSplit then
                evaluateTreeAt leftBranch s
            else
                evaluateTreeAt rightBranch (s - lengthAtSplit)



{- -}
