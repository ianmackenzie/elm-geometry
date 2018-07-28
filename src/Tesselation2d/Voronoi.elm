module Tesselation2d.Voronoi exposing (..)

import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import Circle2d
import Direction2d exposing (Direction2d)
import Vector2d exposing (Vector2d)
import Axis2d
import Array exposing (Array)
import Set
import TriangularMesh exposing (TriangularMesh)
import Polygon2d exposing (Polygon2d)
import Dict exposing (Dict)
import Tesselation2d.Delaunay as Delaunay exposing (GeometryState, SuperTrianglePoints(..))


-- Validate points at infinity


type Accumulator
    = NoneAtInfinity (List Point2d)
    | OneAtInfinity (List Point2d) Direction2d
    | TwoAtInfinity (List Point2d) Direction2d Direction2d
    | MoreAtInfinity


{-| If a triangle shares a vertex with the supertriangle, then one of its vertices is at infinity.
These triangles are needed to create good voronoi regions at the outside of the data points.

`centerPointsPerVertex` will annotate a circle's center point with whether its triangle shared a vertex with the supertriangle.
Here we check that there are either 0 or 2 such center points (these are the only two numbers that make sense).
-}
validatePointsAtInfinity : Accumulator -> List CircleCenter -> Accumulator
validatePointsAtInfinity accumulator remaining =
    case remaining of
        (At vertex) :: rest ->
            case accumulator of
                NoneAtInfinity vertices ->
                    validatePointsAtInfinity (NoneAtInfinity (vertex :: vertices)) rest

                OneAtInfinity vertices atInfinity1 ->
                    validatePointsAtInfinity (OneAtInfinity (vertex :: vertices) atInfinity1) rest

                TwoAtInfinity vertices atInfinity1 atInfinity2 ->
                    validatePointsAtInfinity (TwoAtInfinity (vertex :: vertices) atInfinity1 atInfinity2) rest

                MoreAtInfinity ->
                    MoreAtInfinity

        (AtInfinity atInfinity) :: rest ->
            case accumulator of
                NoneAtInfinity vertices ->
                    validatePointsAtInfinity (OneAtInfinity vertices atInfinity) rest

                OneAtInfinity vertices atInfinity1 ->
                    validatePointsAtInfinity (TwoAtInfinity vertices atInfinity1 atInfinity) rest

                TwoAtInfinity vertices atInfinity1 atInfinity2 ->
                    MoreAtInfinity

                MoreAtInfinity ->
                    MoreAtInfinity

        [] ->
            accumulator


type CircleCenter
    = At Point2d
    | AtInfinity Direction2d


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
                            -- find the direction perpendicular to the direction between the other two vertices of the face
                            direction =
                                case Maybe.map2 Direction2d.from (Array.get other1 points) (Array.get other2 points) |> Maybe.andThen identity of
                                    Just edge ->
                                        let
                                            -- there are two perpendicular directions
                                            -- we want the one closes to the actual point at infinity
                                            clockwise =
                                                Direction2d.rotateClockwise edge

                                            v1 =
                                                lookupDirection shared
                                                    |> Direction2d.toVector

                                            v2 =
                                                Direction2d.toVector clockwise
                                        in
                                            if Vector2d.dotProduct v1 v2 < 0 then
                                                Direction2d.rotateCounterclockwise edge
                                            else
                                                clockwise

                                    Nothing ->
                                        Direction2d.x

                            pointAtInfinity =
                                AtInfinity direction
                        in
                            accum
                                |> insertWith shared (::) [] pointAtInfinity
                                |> insertWith other1 (::) [] pointAtInfinity
                                |> insertWith other2 (::) [] pointAtInfinity

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
