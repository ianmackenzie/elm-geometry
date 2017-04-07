--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


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

{-| <img src="https://opensolid.github.io/images/geometry/icons/polygon2d.svg" alt="Polygon2d" width="160">

A `Polygon2d` represents a closed polygon in 2D, and is defined by a list of
vertices. This module contains a variety of polygon-related functionality, such
as

  - Computing the perimeter and area of polygons
  - Scaling, rotating, translating and mirroring polygons
  - Converting polygons between different coordinate systems

Polygons can be constructed by passing an ordered list of vertices to the
`Polygon2d` constructor, for example

    rectangle =
        Polygon2d
            [ Point2d ( 1, 1 )
            , Point2d ( 3, 1 )
            , Point2d ( 3, 2 )
            , Point2d ( 1, 2 )
            ]

The last vertex is implicitly considered to be connected back to the first
vertex (you do not have to close the polygon explicitly).


# Accessors

@docs vertices, edges


# Perimeter and area

@docs perimeter, area, clockwiseArea, counterclockwiseArea


# Transformations

Transforming a polygon is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, map


# Coordinate frames

@docs relativeTo, placeIn


# Bounds

@docs boundingBox

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Triangle2d as Triangle2d


{-| Get the vertices of a polygon.

    Polygon2d.vertices rectangle
    --> [ Point2d ( 1, 1 )
    --> , Point2d ( 3, 1 )
    --> , Point2d ( 3, 2 )
    --> , Point2d ( 1, 2 )
    --> ]

-}
vertices : Polygon2d -> List Point2d
vertices (Polygon2d vertices_) =
    vertices_


{-| Get the edges of a polygon. This will include an edge from the last point
back to the first point.

    Polygon2d.edges rectangle
    --> [ LineSegment2d ( Point2d ( 1, 1 ), Point2d ( 3, 1 ) )
    --> , LineSegment2d ( Point2d ( 3, 1 ), Point2d ( 3, 2 ) )
    --> , LineSegment2d ( Point2d ( 3, 2 ), Point2d ( 1, 2 ) )
    --> , LineSegment2d ( Point2d ( 1, 2 ), Point2d ( 1, 1 ) )
    --> ]

-}
edges : Polygon2d -> List LineSegment2d
edges polygon =
    case vertices polygon of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 (\start end -> LineSegment2d ( start, end ))
                all
                (rest ++ [ first ])


{-| Get the perimeter of a polygon (the sum of the lengths of its edges).

    Polygon2d.perimeter rectangle
    --> 6

-}
perimeter : Polygon2d -> Float
perimeter =
    edges >> List.map LineSegment2d.length >> List.sum


{-| Get the area of a polygon. This value will never be negative.

    Polygon2d.area rectangle
    --> 2

-}
area : Polygon2d -> Float
area =
    abs << counterclockwiseArea


{-| Get the signed area of a polygon, with polygons with vertices in clockwise
order considered to have positive area and polygons with vertices in
counterclockwise order considered to have negative area.

    Polygon2d.clockwiseArea rectangle
    --> -6

-}
clockwiseArea : Polygon2d -> Float
clockwiseArea polygon =
    -(counterclockwiseArea polygon)


{-| Get the signed area of a polygon, with polygons with vertices in
counterclockwise order considered to have positive area and polygons with
vertices in clockwise order considered to have negative area.

    Polygon2d.counterclockwiseArea rectangle
    --> 6

-}
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


{-| Scale a polygon about a given center point by a given scale.

    point =
        Point2d ( 2, 1 )

    Polygon2d.scaleAbout point 2 rectangle
    --> Polygon2d
    -->     [ Point2d ( 0, 1 )
    -->     , Point2d ( 4, 1 )
    -->     , Point2d ( 4, 3 )
    -->     , Point2d ( 0, 3 )
    -->     ]

-}
scaleAbout : Point2d -> Float -> Polygon2d -> Polygon2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


{-| Rotate a polygon around the given center point counterclockwise by the given
angle (in radians).

    Polygon2d.rotateAround Point2d.origin (degrees 90) rectangle
    --> Polygon2d
    -->     [ Point2d ( -1, 1 )
    -->     , Point2d ( -1, 3 )
    -->     , Point2d ( -2, 3 )
    -->     , Point2d ( -2, 1 )
    -->     ]

-}
rotateAround : Point2d -> Float -> Polygon2d -> Polygon2d
rotateAround point angle =
    map (Point2d.rotateAround point angle)


{-| Translate a polygon by the given displacement.

    displacement =
        Vector2d ( 2, 3 )

    Polygon2d.translateBy displacement rectangle
    --> Polygon2d
    -->     [ Point2d ( 3, 4 )
    -->     , Point2d ( 5, 4 )
    -->     , Point2d ( 5, 5 )
    -->     , Point2d ( 3, 5 )
    -->     ]

-}
translateBy : Vector2d -> Polygon2d -> Polygon2d
translateBy vector =
    map (Point2d.translateBy vector)


{-| Mirror a polygon across the given axis.

    Polygon2d.mirrorAcross Axis2d.x rectangle
    --> Polygon2d
    -->     [ Point2d ( 1, -1 )
    -->     , Point2d ( 3, -1 )
    -->     , Point2d ( 3, -2 )
    -->     , Point2d ( 1, -2 )
    -->     ]

Note that if a polygon's vertices were in counterclockwise order before
mirroring, they will be in clockwise order afterward, and vice versa.

-}
mirrorAcross : Axis2d -> Polygon2d -> Polygon2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


{-| Transform each vertex of a polygon by the given function. All other
transformations can be defined in terms of `map`; for example,

    Polygon2d.mirrorAcross Axis2d.x polygon

is equivalent to

    Polygon2d.map (Point2d.mirrorAcross Axis2d.x) polygon

-}
map : (Point2d -> Point2d) -> Polygon2d -> Polygon2d
map function =
    vertices >> List.map function >> Polygon2d


{-| Take a polygon defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Polygon2d.relativeTo localFrame rectangle
    --> Polygon2d
    -->     [ Point2d ( 0, -1 )
    -->     , Point2d ( 2, -1 )
    -->     , Point2d ( 2, 0 )
    -->     , Point2d ( 0, 0 )
    -->     ]

-}
relativeTo : Frame2d -> Polygon2d -> Polygon2d
relativeTo frame =
    map (Point2d.relativeTo frame)


{-| Take a polygon considered to be defined in local coordinates relative
to a given reference frame, and return that polygon expressed in global
coordinates.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Polygon2d.placeIn localFrame rectangle
    --> Polygon2d
    -->     [ Point2d ( 2, 3 )
    -->     , Point2d ( 4, 3 )
    -->     , Point2d ( 4, 4 )
    -->     , Point2d ( 2, 4 )
    -->     ]

-}
placeIn : Frame2d -> Polygon2d -> Polygon2d
placeIn frame =
    map (Point2d.placeIn frame)


{-| Get the minimal bounding box containing a given polygon. Returns `Nothing`
if the polygon has no vertices.

    Polygon2d.boundingBox rectangle
    --> Just
    -->     (BoundingBox2d
    -->         { minX = 1
    -->         , maxX = 3
    -->         , minY = 1
    -->         , maxY = 2
    -->         }
    -->     )

-}
boundingBox : Polygon2d -> Maybe BoundingBox2d
boundingBox polygon =
    BoundingBox2d.containing (vertices polygon)
