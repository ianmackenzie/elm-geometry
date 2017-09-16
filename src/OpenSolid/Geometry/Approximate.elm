module OpenSolid.Geometry.Approximate exposing (..)

import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Geometry.SegmentTree2d exposing (..)


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


arcLengthParameterization_ : LengthConfig a -> a -> Float -> Maybe Point2d
arcLengthParameterization_ config data =
    case asPolyline config data |> Polyline2d.segments |> buildTree of
        Nothing ->
            \_ -> Nothing

        Just tree ->
            \s -> evaluateTreeAt tree s
