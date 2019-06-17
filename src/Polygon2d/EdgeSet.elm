--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Polygon2d.EdgeSet exposing
    ( Edge
    , EdgeSet
    , empty
    , insert
    , leftOf
    , remove
    )

import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Quantity.Extra as Quantity


type alias Edge units coordinates =
    ( Int, LineSegment2d units coordinates )


type EdgeSet units coordinates
    = EdgeSet (List (Edge units coordinates))


empty : EdgeSet units coordinates
empty =
    EdgeSet []


leftOf : Point2d units coordinates -> EdgeSet units coordinates -> Maybe Int
leftOf point (EdgeSet edges) =
    let
        x =
            Point2d.xCoordinate point

        y =
            Point2d.yCoordinate point
    in
    edges
        |> List.foldl
            (\edge current ->
                let
                    ( p1, p2 ) =
                        LineSegment2d.endpoints (Tuple.second edge)

                    x1 =
                        Point2d.xCoordinate p1

                    y1 =
                        Point2d.yCoordinate p1

                    x2 =
                        Point2d.xCoordinate p2

                    y2 =
                        Point2d.yCoordinate p2

                    dx =
                        if y1 == y2 then
                            x |> Quantity.minus (Quantity.max x1 x2)

                        else
                            let
                                ratio =
                                    Quantity.ratio
                                        (y |> Quantity.minus y1)
                                        (y2 |> Quantity.minus y1)
                            in
                            x
                                |> Quantity.minus
                                    (x1
                                        |> Quantity.plus
                                            (Quantity.multiplyBy ratio
                                                (x2 |> Quantity.minus x1)
                                            )
                                    )
                in
                if dx |> Quantity.greaterThanOrEqualTo Quantity.zero then
                    case current of
                        Nothing ->
                            Just ( dx, edge )

                        Just ( currentDx, currentEdge ) ->
                            if dx |> Quantity.lessThanOrEqualTo currentDx then
                                Just ( dx, edge )

                            else
                                current

                else
                    current
            )
            Nothing
        |> Maybe.map (\( dx, ( index, segment ) ) -> index)


insert : Edge units coordinates -> EdgeSet units coordinates -> EdgeSet units coordinates
insert edge (EdgeSet edges) =
    EdgeSet (edge :: edges)


remove : Edge units coordinates -> EdgeSet units coordinates -> EdgeSet units coordinates
remove edge (EdgeSet edges) =
    EdgeSet (List.filter ((/=) edge) edges)
