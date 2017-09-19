module OpenSolid.Geometry.SegmentTree2d exposing (..)

import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)


{-| A tree for efficiently creating length parameterizations

The nodes contain a line segment (pair of start and end points)
-}
type alias SegmentTree =
    Tree { lengthAtSplit : Float, lengthAtEnd : Float } LineSegment2d


type alias Height =
    Int


type Tree node leaf
    = Node node Height (Tree node leaf) (Tree node leaf)
    | Leaf leaf


foldr : (a -> b -> b) -> b -> Tree x a -> b
foldr folder default tree =
    case tree of
        Leaf leaf ->
            folder leaf default

        Node _ _ left right ->
            foldr folder (foldr folder default right) left


fromList : List LineSegment2d -> Maybe SegmentTree
fromList leaves =
    case leaves of
        [] ->
            Nothing

        x :: rest ->
            let
                treeFromLeaves leaf1 leaf2 =
                    tree (Leaf leaf1) (Leaf leaf2)
            in
                List.foldl extendRight (Leaf x) rest
                    |> Just


toList : Tree a b -> List b
toList =
    foldr (::) []


getHeight : Tree a b -> Int
getHeight tree =
    case tree of
        Leaf _ ->
            1

        Node _ height _ _ ->
            height


length : SegmentTree -> Float
length tree =
    case tree of
        Leaf segment ->
            LineSegment2d.length segment

        Node { lengthAtEnd } _ _ _ ->
            lengthAtEnd


{-| Extend the tree with a new segment on the right
-}
extendRight : LineSegment2d -> SegmentTree -> SegmentTree
extendRight segment set =
    case set of
        Leaf segment2 ->
            tree (Leaf segment2) (Leaf segment)

        Node attributes height leftSubtree rightSubtree ->
            let
                newRight =
                    extendRight segment rightSubtree
            in
                tree leftSubtree newRight
                    |> balance


{-| Extend the tree with a new segment on the left
-}
extendLeft : LineSegment2d -> SegmentTree -> SegmentTree
extendLeft segment set =
    case set of
        Leaf segment2 ->
            tree (Leaf segment) (Leaf segment2)

        Node attributes height leftSubtree rightSubtree ->
            let
                newLeft =
                    extendLeft segment leftSubtree
            in
                tree newLeft rightSubtree
                    |> balance


tree : SegmentTree -> SegmentTree -> SegmentTree
tree left right =
    let
        maxHeight : Int
        maxHeight =
            max (getHeight left) (getHeight right)
                |> (+) 1
    in
        Node { lengthAtSplit = length left, lengthAtEnd = length left + length right } maxHeight left right


{-| rotate the tree to the left - used in balancing
-}
rotateLeft : SegmentTree -> SegmentTree
rotateLeft set =
    case set of
        Node root _ less (Node pivot _ between greater) ->
            tree (tree less between) greater

        _ ->
            set


{-| rotate the tree to the right - used in balancing
-}
rotateRight : SegmentTree -> SegmentTree
rotateRight set =
    case set of
        Node root _ (Node pivot _ less between) greater ->
            tree less (tree between greater)

        _ ->
            set


heightDiff : Tree a b -> Int
heightDiff tree =
    case tree of
        Leaf _ ->
            0

        Node _ _ left right ->
            getHeight right - getHeight left


balance : SegmentTree -> SegmentTree
balance set =
    case set of
        Leaf _ ->
            set

        Node attributes _ left right ->
            if heightDiff set == -1 && heightDiff left > 0 then
                tree (rotateLeft left) right |> rotateRight
            else if heightDiff set < -1 then
                -- Node leaning to the left
                rotateRight set
            else if heightDiff set == 2 && heightDiff right < 0 then
                -- Node leaning to the right with subtree leaning left
                tree left (rotateRight right) |> rotateLeft
            else if heightDiff set > 1 then
                -- Node leaning to the right
                rotateLeft set
            else
                -- Balanced tree
                set


{-| recursivey, calculate the start, split and end lengths based on a node's children
-}
recalculateLengths : SegmentTree -> SegmentTree
recalculateLengths tree =
    case tree of
        Leaf _ ->
            tree

        Node _ height leftBranch rightBranch ->
            let
                build node =
                    Node node height leftBranch rightBranch
            in
                case ( recalculateLengths leftBranch, recalculateLengths rightBranch ) of
                    ( Node params1 _ _ _, Node params2 _ _ _ ) ->
                        build
                            { lengthAtSplit = params1.lengthAtEnd
                            , lengthAtEnd = params1.lengthAtEnd + params2.lengthAtEnd
                            }

                    ( Node params _ _ _, Leaf segment ) ->
                        build
                            { lengthAtSplit = params.lengthAtEnd
                            , lengthAtEnd = params.lengthAtEnd + LineSegment2d.length segment
                            }

                    ( Leaf segment, Node params _ leftsubsub rightsubsub ) ->
                        build
                            { lengthAtSplit = LineSegment2d.length segment
                            , lengthAtEnd = params.lengthAtEnd + LineSegment2d.length segment
                            }

                    ( Leaf segment1, Leaf segment2 ) ->
                        build
                            { lengthAtSplit = LineSegment2d.length segment1
                            , lengthAtEnd = LineSegment2d.length segment1 + LineSegment2d.length segment2
                            }


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

        Node { lengthAtSplit, lengthAtEnd } _ leftBranch rightBranch ->
            let
                _ =
                    -- Debug.log "looking at" ( s, lengthAtSplit, lengthAtEnd )
                    ()
            in
                if s < 0 || s > lengthAtEnd then
                    Nothing
                else if s <= lengthAtSplit then
                    evaluateTreeAt leftBranch s
                else
                    evaluateTreeAt rightBranch (s - lengthAtSplit)
