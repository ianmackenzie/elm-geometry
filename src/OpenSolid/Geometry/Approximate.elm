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


type Tree
    = Node { lengthAtStart : Float, lengthAtSplit : Float, lengthAtEnd : Float } Tree Tree
    | Leaf LineSegment2d


shift : Float -> Tree -> Tree
shift amount tree =
    case tree of
        Leaf segment ->
            Leaf segment

        Node { lengthAtStart, lengthAtEnd, lengthAtSplit } leftBranch rightBranch ->
            Node { lengthAtStart = amount + lengthAtStart, lengthAtEnd = lengthAtEnd + amount, lengthAtSplit = lengthAtSplit + amount }
                (shift amount leftBranch)
                (shift amount rightBranch)


mappend : Tree -> Tree -> Tree
mappend leftBranch rightBranch =
    case ( leftBranch, rightBranch ) of
        ( Node params1 _ _, Node params2 _ _ ) ->
            Node
                { lengthAtStart = params1.lengthAtStart
                , lengthAtSplit = params1.lengthAtEnd
                , lengthAtEnd = params1.lengthAtEnd + params2.lengthAtEnd
                }
                leftBranch
                rightBranch

        ( Node params _ _, Leaf segment ) ->
            Node
                { lengthAtStart = params.lengthAtStart
                , lengthAtSplit = params.lengthAtEnd
                , lengthAtEnd = params.lengthAtEnd + LineSegment2d.length segment
                }
                leftBranch
                rightBranch

        ( Leaf segment, Node params _ _ ) ->
            Node
                { lengthAtStart = params.lengthAtStart - LineSegment2d.length segment
                , lengthAtSplit = params.lengthAtStart
                , lengthAtEnd = params.lengthAtEnd + LineSegment2d.length segment
                }
                leftBranch
                rightBranch

        ( Leaf segment1, Leaf segment2 ) ->
            Node
                { lengthAtStart = 0
                , lengthAtSplit = LineSegment2d.length segment1
                , lengthAtEnd = LineSegment2d.length segment1 + LineSegment2d.length segment2
                }
                leftBranch
                rightBranch


inPairs : (a -> a -> a) -> List a -> List a
inPairs f items =
    case items of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: rest ->
            f x y :: inPairs f rest


iterate : (List a -> List a) -> List a -> Maybe a
iterate f x =
    case f x of
        [] ->
            Nothing

        [ x ] ->
            Just x

        otherwise ->
            iterate f otherwise


buildTree : List LineSegment2d -> Maybe Tree
buildTree segments =
    segments
        |> List.map Leaf
        |> iterate (inPairs mappend)
        |> Debug.log "tree"


evaluateTreeAt : Tree -> Float -> Maybe Point2d
evaluateTreeAt tree s =
    case tree of
        Leaf segment ->
            let
                segmentLength =
                    LineSegment2d.length segment
            in
                if s > segmentLength then
                    let
                        _ =
                            Debug.log "**** s is too big" ( s, tree )
                    in
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
                let
                    _ =
                        Debug.log "**** s is out of range" ( s, tree )
                in
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


arcLengthParameterization : ParameterizationConfig a -> a -> Float -> Maybe Point2d
arcLengthParameterization config data s =
    let
        splitFurther data =
            let
                lower =
                    config.lowerBound data

                upper =
                    config.upperBound data

                average =
                    (lower + upper) / 2
            in
                (average - lower) / average > config.percentageError

        length item =
            let
                ( start, end ) =
                    config.startAndEndpoint item
            in
                Point2d.distanceFrom start end
    in
        if splitFurther data then
            let
                ( left, right ) =
                    config.split 0.5 data
            in
                if s < config.lowerBound left then
                    arcLengthParameterization config left s
                else if s > config.upperBound data then
                    Nothing
                else if s > config.upperBound left then
                    arcLengthParameterization config right (s - length left)
                else
                    case arcLengthParameterization config left s of
                        Just p ->
                            Just p

                        Nothing ->
                            arcLengthParameterization config right (s - length left)
        else
            let
                ( start, end ) =
                    config.startAndEndpoint data

                factor =
                    length data / s
            in
                Point2d.interpolateFrom start end factor
                    |> Just
