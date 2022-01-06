--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module BoundingBox2d.PDSGrid exposing (..)

{-| This module implements a grid used for Poisson Disk Sampling.
-}

import Dict exposing (Dict)


type Grid
    = Grid
        { width : Float
        , height : Float
        , radius : Float
        , cellSize : Float
        , gridWidth : Int
        , gridHeight : Int
        , grid : Dict Int ( Float, Float )
        }


initialize : Float -> Float -> Float -> Grid
initialize width height radius =
    let
        cellSize =
            radius * sqrt 0.5
    in
    Grid
        { width = width
        , height = height
        , radius = radius
        , cellSize = cellSize
        , gridWidth = ceiling (width / cellSize)
        , gridHeight = ceiling (height / cellSize)
        , grid = Dict.empty
        }


set : ( Float, Float ) -> Grid -> Grid
set ( x, y ) (Grid grid) =
    Grid { grid | grid = Dict.insert (grid.gridWidth * floor (y / grid.cellSize) + floor (x / grid.cellSize)) ( x, y ) grid.grid }


getRadius : Grid -> Float
getRadius (Grid grid) =
    grid.radius


isNeighborhoodEmpty : ( Float, Float ) -> Grid -> Bool
isNeighborhoodEmpty ( cx, cy ) (Grid { grid, cellSize, gridWidth, gridHeight, radius, width, height }) =
    let
        i_ =
            floor (cx / cellSize)

        j_ =
            floor (cy / cellSize)

        is =
            List.range (max 0 (i_ - 2)) (min (i_ + 3) gridWidth - 1)

        os =
            List.range (max 0 (j_ - 2)) (min (j_ + 3) gridHeight - 1) |> List.map (\j -> j * gridWidth)

        radius2 =
            radius ^ 2

        inBounds =
            0 <= cx && cx < width && 0 <= cy && cy < height
    in
    inBounds
        && List.all
            (\o ->
                List.all
                    (\i ->
                        case Dict.get (o + i) grid of
                            Just ( sx, sy ) ->
                                (sx - cx) ^ 2 + (sy - cy) ^ 2 >= radius2

                            Nothing ->
                                True
                    )
                    is
            )
            os


values : Grid -> List ( Float, Float )
values (Grid { grid }) =
    Dict.values grid
