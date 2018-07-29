module Tesselation2d.Voronoi exposing (..)

import Point2d exposing (Point2d)
import Direction2d exposing (Direction2d)
import Vector2d exposing (Vector2d)
import LineSegment2d exposing (LineSegment2d)
import Array exposing (Array)
import Dict exposing (Dict)
import Tesselation2d.Delaunay as Delaunay exposing (GeometryState, SuperTrianglePoints(..))


{-| Validate points at infinity
-}
type Accumulator
    = NoneAtInfinity (List Point2d)
    | OneAtInfinity (List Point2d) ( Point2d, Direction2d )
    | TwoAtInfinity (List Point2d) ( Point2d, Direction2d ) ( Point2d, Direction2d )
    | MoreAtInfinity


{-| If a triangle shares a vertex with the supertriangle, then one of its vertices is at infinity.
These triangles are needed to create good voronoi regions at the outside of the data points.

`centerPointsPerVertex` will annotate a circle's center point with whether its triangle shared a vertex with the supertriangle.
Here we check that there are either 0 or 2 such center points (these are the only two numbers that make sense).
-}
validatePointsAtInfinity : Point2d -> List CircleCenter -> Accumulator -> Accumulator
validatePointsAtInfinity datapoint remaining accumulator =
    case remaining of
        (At vertex) :: rest ->
            case accumulator of
                NoneAtInfinity vertices ->
                    validatePointsAtInfinity
                        datapoint
                        rest
                        (NoneAtInfinity (vertex :: vertices))

                OneAtInfinity vertices atInfinity1 ->
                    validatePointsAtInfinity
                        datapoint
                        rest
                        (OneAtInfinity (vertex :: vertices) atInfinity1)

                TwoAtInfinity vertices atInfinity1 atInfinity2 ->
                    validatePointsAtInfinity
                        datapoint
                        rest
                        (TwoAtInfinity (vertex :: vertices) atInfinity1 atInfinity2)

                MoreAtInfinity ->
                    MoreAtInfinity

        (AtInfinity atInfinity) :: rest ->
            case accumulator of
                NoneAtInfinity vertices ->
                    validatePointsAtInfinity datapoint rest (OneAtInfinity vertices atInfinity)

                OneAtInfinity vertices atInfinity1 ->
                    validatePointsAtInfinity datapoint rest (TwoAtInfinity vertices atInfinity1 atInfinity)

                TwoAtInfinity _ _ _ ->
                    MoreAtInfinity

                MoreAtInfinity ->
                    MoreAtInfinity

        [] ->
            accumulator


type CircleCenter
    = At Point2d
    | AtInfinity ( Point2d, Direction2d )


{-| Find the direction of a point of the supertriangle.
The triangle looks like this:
    2
    |\
    0-1

So point 0 pulls to the bottom-left, 1 to the right and 2 to the top.
-}
lookupDirection : Int -> Direction2d
lookupDirection n =
    Maybe.withDefault (Direction2d.positiveX) <|
        case n of
            0 ->
                Direction2d.from Point2d.origin (Point2d.fromCoordinates ( -1, -1 ))

            1 ->
                Direction2d.from Point2d.origin (Point2d.fromCoordinates ( 1, 0 ))

            2 ->
                Direction2d.from Point2d.origin (Point2d.fromCoordinates ( 0, 1 ))

            _ ->
                Nothing


{-| For every vertex, store the circumcenters of the triangles that it occurs in

We have to be very careful with triangles that share vertices with the supertriangle.
It seems that only triangles with one shared point with the supertriangle are useful.
But, this point at infinity needs to be changed to match the triangle, otherwise all triangles
with a point at infinity would tend to the same 3 points. So we use the direction perpendicular to the
edge between the other two vertices of the face.
-}
centerPointsPerVertex : GeometryState -> Dict Int (List CircleCenter)
centerPointsPerVertex { faces, edges, points } =
    let
        folder face accum =
            if face.marked then
                accum
            else
                case face.superTrianglePoints of
                    One { shared, other1, other2 } ->
                        let
                            -- the direction perpendicular to the edge between the other points
                            -- and the midpoint of that edge
                            perpendicularDirection =
                                case Maybe.map2 Tuple.pair (Array.get other1 points) (Array.get other2 points) of
                                    Nothing ->
                                        Nothing

                                    Just ( start, end ) ->
                                        case Direction2d.from start end of
                                            Just edge ->
                                                let
                                                    rotatedClockwise =
                                                        Direction2d.rotateClockwise edge

                                                    v1 =
                                                        lookupDirection shared
                                                            |> Direction2d.toVector

                                                    v2 =
                                                        Direction2d.toVector rotatedClockwise

                                                    middle =
                                                        LineSegment2d.interpolate (LineSegment2d.from start end) 0.5
                                                in
                                                    Just <|
                                                        if Vector2d.dotProduct v1 v2 >= 0 then
                                                            AtInfinity ( middle, rotatedClockwise )
                                                        else
                                                            AtInfinity ( middle, (Direction2d.rotateCounterclockwise edge) )

                                            Nothing ->
                                                Nothing
                        in
                            case Delaunay.getVertexIndices face edges of
                                Nothing ->
                                    accum

                                Just ( p1, p2, p3 ) ->
                                    case perpendicularDirection of
                                        Nothing ->
                                            accum

                                        Just newPoint ->
                                            accum
                                                |> insertWith p1 (::) [] newPoint
                                                |> insertWith p2 (::) [] newPoint
                                                |> insertWith p3 (::) [] newPoint

                    Zero ->
                        case Delaunay.getVertexIndices face edges of
                            Nothing ->
                                accum

                            Just ( p1, p2, p3 ) ->
                                let
                                    point =
                                        At face.center
                                in
                                    accum
                                        |> insertWith p1 (::) [] point
                                        |> insertWith p2 (::) [] point
                                        |> insertWith p3 (::) [] point

                    _ ->
                        accum
    in
        Array.foldl folder Dict.empty faces


insertWith : comparable -> (a -> b -> b) -> b -> a -> Dict comparable b -> Dict comparable b
insertWith key append default value dict =
    let
        updater optionalValue =
            case optionalValue of
                Nothing ->
                    Just (append value default)

                Just current ->
                    Just (append value current)
    in
        Dict.update key updater dict
