module OpenSolid.Geometry.Approximate exposing (..)

import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)


type alias LengthConfig a =
    { split : Float -> a -> ( a, a )
    , percentageError : Float
    , startAndEndpoint : a -> ( Point2d, Point2d )
    }


length : LengthConfig a -> a -> Float
length config object =
    asPolyline config object
        |> Polyline2d.length


asPolyline : LengthConfig a -> a -> Polyline2d
asPolyline config curve =
    helper config [ curve ] []
        |> segments config
        |> Polyline2d.withVertices


segments : LengthConfig a -> List a -> List Point2d
segments config elements =
    case elements of
        [] ->
            []

        [ x ] ->
            []

        [ x, y ] ->
            let
                ( start, _ ) =
                    config.startAndEndpoint x

                ( _, end ) =
                    config.startAndEndpoint y
            in
                [ start, end ]

        x :: rest ->
            let
                ( start, _ ) =
                    config.startAndEndpoint x
            in
                start :: segments config rest


{-| make the split function tail-recursive
-}
helper : LengthConfig a -> List a -> List a -> List a
helper config remaining accum =
    case remaining of
        [] ->
            accum

        curve :: rest ->
            let
                length item =
                    let
                        ( start, end ) =
                            config.startAndEndpoint item
                    in
                        Point2d.distanceFrom start end

                ( left, right ) =
                    config.split 0.5 curve

                lessAccurate =
                    length curve

                moreAccurate =
                    length left + length right

                average =
                    (lessAccurate + moreAccurate) / 2
            in
                if (average - lessAccurate) / average > config.percentageError then
                    helper config (right :: left :: rest) accum
                else
                    helper config rest (left :: right :: accum)


type alias ParameterizationConfig a =
    { split : Float -> a -> ( a, a )
    , percentageError : Float
    , upperBound : a -> Float
    , lowerBound : a -> Float
    , startAndEndpoint : a -> ( Point2d, Point2d )
    }


type alias NodeValue =
    { lengthAtStart : Float, lengthAtSplit : Float, lengthAtEnd : Float }


type alias SegmentTree =
    Tree NodeValue LineSegment2d


type Tree node leaf
    = Node { node | height : Int } (Tree node leaf) (Tree node leaf)
    | Leaf leaf


leavesToNode : LineSegment2d -> LineSegment2d -> SegmentTree
leavesToNode segment segment2 =
    Node
        { lengthAtStart = 0
        , lengthAtSplit = LineSegment2d.length segment
        , lengthAtEnd = LineSegment2d.length segment + LineSegment2d.length segment2
        , height = 2
        }
        (Leaf segment)
        (Leaf segment2)


getHeight : Tree a b -> Int
getHeight tree =
    case tree of
        Leaf _ ->
            1

        Node { height } _ _ ->
            height


insertInTree : (leaf -> leaf -> Tree node leaf) -> leaf -> Tree node leaf -> Tree node leaf
insertInTree combine segment tree =
    case tree of
        Leaf segment2 ->
            flip combine segment segment2

        Node ({ height } as attributes) leftSubtree rightSubtree ->
            Node { attributes | height = height + 1 } leftSubtree (insertInTree combine segment rightSubtree)
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
        Node { item | height = maxHeight } left right


{-| Rotate a tree to the left (for balancing).
-}
rotateLeft set =
    case set of
        Node root less (Node pivot between greater) ->
            tree pivot (tree root less between) greater

        _ ->
            set


{-| Inversely, rotate a tree to the right (for balancing).
-}
rotateRight set =
    case set of
        Node root (Node pivot less between) greater ->
            tree pivot less (tree root between greater)

        _ ->
            set


heightDiff : Tree a b -> Int
heightDiff tree =
    case tree of
        Leaf _ ->
            0

        Node _ left right ->
            getHeight right - getHeight left


balance : Tree a b -> Tree a b
balance set =
    case set of
        Leaf _ ->
            set

        Node attributes left right ->
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

        Node { lengthAtStart, lengthAtEnd, lengthAtSplit, height } leftBranch rightBranch ->
            Node { lengthAtStart = amount + lengthAtStart, lengthAtEnd = lengthAtEnd + amount, lengthAtSplit = lengthAtSplit + amount, height = height }
                (shift amount leftBranch)
                (shift amount rightBranch)


{-| recursivey, calculate the start, split and end lengths based on a node's children
-}
recalculateLengths : SegmentTree -> SegmentTree
recalculateLengths tree =
    case tree of
        Leaf _ ->
            tree

        Node { height } leftBranch rightBranch ->
            case ( recalculateLengths leftBranch, recalculateLengths rightBranch ) of
                ( Node params1 _ _, Node params2 _ _ ) ->
                    Node
                        { lengthAtStart = params1.lengthAtStart
                        , lengthAtSplit = params1.lengthAtEnd
                        , lengthAtEnd = params1.lengthAtEnd + params2.lengthAtEnd
                        , height = height
                        }
                        leftBranch
                        rightBranch

                ( Node params _ _, Leaf segment ) ->
                    Node
                        { lengthAtStart = params.lengthAtStart
                        , lengthAtSplit = params.lengthAtEnd
                        , lengthAtEnd = params.lengthAtEnd + LineSegment2d.length segment
                        , height = height
                        }
                        leftBranch
                        rightBranch

                ( Leaf segment, Node params leftsubsub rightsubsub ) ->
                    Node
                        { lengthAtStart = 0
                        , lengthAtSplit = LineSegment2d.length segment
                        , lengthAtEnd = params.lengthAtEnd + LineSegment2d.length segment
                        , height = height
                        }
                        leftBranch
                        (shift (LineSegment2d.length segment) rightBranch)

                ( Leaf segment1, Leaf segment2 ) ->
                    Node
                        { lengthAtStart = 0
                        , lengthAtSplit = LineSegment2d.length segment1
                        , lengthAtEnd = LineSegment2d.length segment1 + LineSegment2d.length segment2
                        , height = height
                        }
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

        Node { lengthAtStart, lengthAtSplit, lengthAtEnd } leftBranch rightBranch ->
            if s < lengthAtStart || s > lengthAtEnd then
                Nothing
            else if s <= lengthAtSplit then
                evaluateTreeAt leftBranch s
            else
                evaluateTreeAt rightBranch (s - lengthAtSplit)


arcLengthParameterization_ : LengthConfig a -> a -> Float -> Maybe Point2d
arcLengthParameterization_ config data =
    case asPolyline config data |> Polyline2d.segments |> buildTree of
        Nothing ->
            \_ -> Nothing

        Just tree ->
            \s -> evaluateTreeAt tree s
