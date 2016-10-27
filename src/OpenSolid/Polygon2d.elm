{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Polygon2d
    exposing
        ( vertices
        , edges
        , perimeter
        , area
        , clockwiseArea
        , counterclockwiseArea
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , map
        , relativeTo
        , placeIn
        , boundingBox
        )

{-| Various functions for creating and working with `Polygon2d` values. Polygons
can be constructed explicitly by passing a list of vertices to the `Polygon2d`
constructor, for example

    rectangle =
        Polygon2d
            [ Point2d ( 1, 1 )
            , Point2d ( 3, 1 )
            , Point2d ( 3, 2 )
            , Point2d ( 1, 2 )
            ]

For the examples below, assume that `rectangle` has been defined, all OpenSolid
core types have been imported using

    import OpenSolid.Geometry.Types exposing (..)

and all other necessary modules have been imported using the following pattern:

    import OpenSolid.Polygon2d as Polygon2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Accessors

@docs vertices, edges

# Perimeter and area

@docs perimeter, area, clockwiseArea, counterclockwiseArea

# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, map

# Coordinate frames

@docs relativeTo, placeIn
-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Triangle2d as Triangle2d


vertices : Polygon2d -> List Point2d
vertices (Polygon2d vertices') =
    vertices'


edges : Polygon2d -> List LineSegment2d
edges polygon =
    case vertices polygon of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 (\start end -> LineSegment2d ( start, end ))
                all
                (rest ++ [ first ])


perimeter : Polygon2d -> Float
perimeter =
    edges >> List.map LineSegment2d.length >> List.sum


area : Polygon2d -> Float
area =
    abs << counterclockwiseArea


clockwiseArea : Polygon2d -> Float
clockwiseArea polygon =
    -(counterclockwiseArea polygon)


counterclockwiseArea : Polygon2d -> Float
counterclockwiseArea polygon =
    case vertices polygon of
        [] ->
            0

        [ single ] ->
            0

        [ first, second ] ->
            0

        first :: second :: rest ->
            let
                segmentArea start end =
                    Triangle2d.counterclockwiseArea (Triangle2d ( first, start, end ))

                segmentAreas =
                    List.map2 segmentArea (second :: rest) rest
            in
                List.sum segmentAreas


scaleAbout : Point2d -> Float -> Polygon2d -> Polygon2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


rotateAround : Point2d -> Float -> Polygon2d -> Polygon2d
rotateAround point angle =
    map (Point2d.rotateAround point angle)


translateBy : Vector2d -> Polygon2d -> Polygon2d
translateBy vector =
    map (Point2d.translateBy vector)


mirrorAcross : Axis2d -> Polygon2d -> Polygon2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


map : (Point2d -> Point2d) -> Polygon2d -> Polygon2d
map function =
    vertices >> List.map function >> Polygon2d


relativeTo : Frame2d -> Polygon2d -> Polygon2d
relativeTo frame =
    map (Point2d.relativeTo frame)


placeIn : Frame2d -> Polygon2d -> Polygon2d
placeIn frame =
    map (Point2d.placeIn frame)


boundingBox : Polygon2d -> Maybe BoundingBox2d
boundingBox polygon =
    BoundingBox2d.containing (vertices polygon)
