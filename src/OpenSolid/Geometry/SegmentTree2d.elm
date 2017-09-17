module OpenSolid.Geometry.SegmentTree2d exposing (..)

import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)


{-| A tree for efficiently creating length parameterizations

The nodes contain a line segment (pair of start and end points)

Every node stores

* left and right children
* the height at which the node is
* inforamation on the children
    - total length of the children
    - length at which the split is


-}
type alias SegmentTree =
    Tree NodeValue LineSegment2d


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


fromList : (LineSegment2d -> LineSegment2d -> NodeValue) -> List LineSegment2d -> Maybe SegmentTree
fromList nodeFromLeaves leaves =
    case leaves of
        [] ->
            Nothing

        x :: rest ->
            let
                treeFromLeaves leaf1 leaf2 =
                    tree (nodeFromLeaves leaf1 leaf2) (Leaf leaf1) (Leaf leaf2)
            in
                List.foldl (extendRight treeFromLeaves) (Leaf x) rest
                    |> Just


toList : Tree a b -> List b
toList =
    foldr (::) []


type alias NodeValue =
    { lengthAtSplit : Float, lengthAtEnd : Float }


leavesToNode : LineSegment2d -> LineSegment2d -> NodeValue
leavesToNode segment segment2 =
    { lengthAtSplit = LineSegment2d.length segment
    , lengthAtEnd = LineSegment2d.length segment + LineSegment2d.length segment2
    }


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


extendRight_ : (leaf -> leaf -> Tree node leaf) -> leaf -> Tree node leaf -> Tree node leaf
extendRight_ combine segment tree =
    case tree of
        Leaf segment2 ->
            flip combine segment segment2

        Node attributes height leftSubtree rightSubtree ->
            let
                newRight =
                    extendRight_ combine segment rightSubtree
            in
                Node attributes (height + 1) leftSubtree newRight
                    |> balance


{-| Extend the tree with a new segment on the right
-}
extendRight : (LineSegment2d -> LineSegment2d -> Tree NodeValue LineSegment2d) -> LineSegment2d -> SegmentTree -> SegmentTree
extendRight combine segment tree =
    case tree of
        Leaf segment2 ->
            flip combine segment segment2

        Node attributes height leftSubtree rightSubtree ->
            let
                newRight =
                    extendRight combine segment rightSubtree
            in
                Node { attributes | lengthAtEnd = attributes.lengthAtSplit + length newRight } (height + 1) leftSubtree newRight
                    |> balance


{-| Extend the tree with a new segment on the left
-}
extendLeft : (LineSegment2d -> LineSegment2d -> Tree NodeValue LineSegment2d) -> LineSegment2d -> SegmentTree -> SegmentTree
extendLeft combine segment tree =
    case tree of
        Leaf segment2 ->
            flip combine segment segment2

        Node attributes height leftSubtree rightSubtree ->
            let
                newLeft =
                    extendLeft combine segment leftSubtree

                leftLength =
                    length newLeft
            in
                Node
                    { attributes
                        | lengthAtSplit = leftLength
                        , lengthAtEnd = leftLength + (attributes.lengthAtEnd - attributes.lengthAtSplit)
                    }
                    (height + 1)
                    newLeft
                    rightSubtree
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
rotateLeft : Tree a b -> Tree a b
rotateLeft set =
    case set of
        Node root _ less (Node pivot _ between greater) ->
            tree pivot (tree root less between) greater

        _ ->
            set


{-| Inversely, rotate a tree to the right (for balancing).
-}
rotateRight : Tree a b -> Tree a b
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


buildTree : List LineSegment2d -> Maybe SegmentTree
buildTree segments =
    fromList leavesToNode segments
        |> Maybe.map recalculateLengths


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
            if s < 0 || s > lengthAtEnd then
                Nothing
            else if s <= lengthAtSplit then
                evaluateTreeAt leftBranch s
            else
                evaluateTreeAt rightBranch (s - lengthAtSplit)



{- -}
