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


module OpenSolid.Triangle2d
    exposing
        ( vertices
        , edges
        , centroid
        , contains
        , area
        , counterclockwiseArea
        , clockwiseArea
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , map
        , relativeTo
        , placeIn
        , placeOnto
        , boundingBox
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/triangle2d.svg" alt="Triangle2d" width="160">

A `Triangle2d` represents a triangle in 2D space, and is defined by its three
vertices. This module contains triangle-related functionality such as:

  - Finding the area and centroid of triangles
  - Scaling, rotating, translating and mirroring triangles
  - Converting triangles between different coordinate systems

Triangles can be constructed by passing a tuple of vertices to the `Triangle2d`
constructor, for example

    exampleTriangle =
        Triangle2d
            ( Point2d ( 1, 1 )
            , Point2d ( 2, 1 )
            , Point2d ( 1, 3 )
            )


# Accessors

@docs vertices, edges


# Basics

@docs centroid, contains


# Area

@docs area, counterclockwiseArea, clockwiseArea


# Transformations

Transforming a triangle is equivalent to transforming its vertices.

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, map


# Coordinate frames

Functions for transforming triangles between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn


# Sketch planes

@docs placeOnto


# Bounds

@docs boundingBox

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Point2d as Point2d


{-| Get the vertices of a triangle.

    ( p1, p2, p3 ) =
        Triangle2d.vertices exampleTriangle


    --> p1 = Point2d ( 1, 1 )
    --> p2 = Point2d ( 2, 1 )
    --> p3 = Point2d ( 1, 3 )

-}
vertices : Triangle2d -> ( Point2d, Point2d, Point2d )
vertices (Triangle2d vertices_) =
    vertices_


{-| Get the edges of a triangle: from the first vertex to the second, from the
second to the third, and from the third back to the first.

    ( e1, e2, e3 ) =
        Triangle2d.edges exampleTriangle


    --> e1 = LineSegment2d ( Point2d ( 1, 1 ), Point2d ( 2, 1 ) )
    --> e2 = LineSegment2d ( Point2d ( 2, 1 ), Point2d ( 1, 3 ) )
    --> e3 = LineSegment2d ( Point2d ( 1, 3 ), Point2d ( 1, 1 ) )

-}
edges : Triangle2d -> ( LineSegment2d, LineSegment2d, LineSegment2d )
edges triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        ( LineSegment2d ( p1, p2 )
        , LineSegment2d ( p2, p3 )
        , LineSegment2d ( p3, p1 )
        )


{-| Get the centroid (center of mass) of a triangle.

    Triangle2d.centroid exampleTriangle
    --> Point2d ( 1.3333, 1.6667 )

-}
centroid : Triangle2d -> Point2d
centroid triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Point2d.vectorFrom p1 p2

        secondVector =
            Point2d.vectorFrom p1 p3

        displacement =
            Vector2d.scaleBy (1.0 / 3.0) (Vector2d.sum firstVector secondVector)
    in
        Point2d.translateBy displacement p1


{-| Check whether a given point is inside a given triangle.

    interiorPoint =
        Point2d ( 1.5, 1.5 )

    Triangle2d.contains interiorPoint exampleTriangle
    --> True

    Triangle2d.contains Point2d.origin exampleTriangle
    --> False

It does not matter whether the triangle's vertices are in clockwise or
counterclockwise order.

-}
contains : Point2d -> Triangle2d -> Bool
contains point triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        crossProduct startVertex endVertex =
            let
                vectorToPoint =
                    Point2d.vectorFrom startVertex point

                segmentVector =
                    Point2d.vectorFrom startVertex endVertex
            in
                Vector2d.crossProduct segmentVector vectorToPoint

        firstProduct =
            crossProduct p1 p2

        secondProduct =
            crossProduct p2 p3

        thirdProduct =
            crossProduct p3 p1
    in
        (firstProduct >= 0 && secondProduct >= 0 && thirdProduct >= 0)
            || (firstProduct <= 0 && secondProduct <= 0 && thirdProduct <= 0)


{-| Get the area of a triangle. The result will always be positive regardless of
whether the triangle's vertices are in clockwise or counterclockwise order.

    Triangle2d.area exampleTriangle
    --> 1.0

-}
area : Triangle2d -> Float
area =
    counterclockwiseArea >> abs


{-| Get the signed area of a triangle, returning a positive value if the
triangle's vertices are in counterclockwise order and a negative value
otherwise.

    Triangle2d.counterclockwiseArea exampleTriangle
    --> 1.0

-}
counterclockwiseArea : Triangle2d -> Float
counterclockwiseArea triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Point2d.vectorFrom p1 p2

        secondVector =
            Point2d.vectorFrom p1 p3
    in
        0.5 * Vector2d.crossProduct firstVector secondVector


{-| Get the signed area of a triangle, returning a positive value if the
triangle's vertices are in clockwise order and a negative value otherwise.

    Triangle2d.clockwiseArea exampleTriangle
    --> -1.0

-}
clockwiseArea : Triangle2d -> Float
clockwiseArea triangle =
    -(counterclockwiseArea triangle)


{-| Scale a triangle about a given point by a given scale.

    Triangle2d.scaleAbout Point2d.origin 2 exampleTriangle
    --> Triangle2d
    -->     ( Point2d ( 2, 2 )
    -->     , Point2d ( 4, 2 )
    -->     , Point2d ( 2, 6 )
    -->     )

Note that scaling by a negative value will result in the 'winding direction' of
the triangle being flipped - if the triangle's vertices were in counterclockwise
order before the negative scaling, they will be in clockwise order afterwards
and vice versa.

-}
scaleAbout : Point2d -> Float -> Triangle2d -> Triangle2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


{-| Rotate a triangle around a given point by a given angle (in radians).

    Triangle2d.rotateAround Point2d.origin (degrees 90) exampleTriangle
    --> Triangle2d
    -->     ( Point2d ( -1, 1 )
    -->     , Point2d ( -1, 2 )
    -->     , Point2d ( -3, 1 )
    -->     )

-}
rotateAround : Point2d -> Float -> Triangle2d -> Triangle2d
rotateAround centerPoint angle =
    map (Point2d.rotateAround centerPoint angle)


{-| Translate a triangle by a given displacement.

    displacement =
        Vector2d ( 2, -3 )

    Triangle2d.translateBy displacement exampleTriangle
    --> Triangle2d
    -->     ( Point2d ( 3, -2 )
    -->     , Point2d ( 4, -2 )
    -->     , Point2d ( 3, 0 )
    -->     )

-}
translateBy : Vector2d -> Triangle2d -> Triangle2d
translateBy vector =
    map (Point2d.translateBy vector)


{-| Mirror a triangle across a given axis.

    Triangle2d.mirrorAcross Axis2d.y exampleTriangle
    --> Triangle2d
    -->     ( Point2d ( -1, 1 )
    -->     , Point2d ( -2, 1 )
    -->     , Point2d ( -1, 3 )
    -->     )

Note that mirroring a triangle will result in its 'winding direction' being
flipped - if the triangle's vertices were in counterclockwise order before
mirroring, they will be in clockwise order afterwards and vice versa.

-}
mirrorAcross : Axis2d -> Triangle2d -> Triangle2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


{-| Transform each vertex of a triangle by a given function and create a new
triangle from the resulting points. Most other transformation functions can be
defined in terms of `map`; for example,

    Triangle.mirrorAcross Axis2d.x triangle

is equivalent to

    Triangle.map (Point2d.mirrorAcross Axis2d.x) triangle

-}
map : (Point2d -> Point2d) -> Triangle2d -> Triangle2d
map function triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        Triangle2d ( function p1, function p2, function p3 )


{-| Take a triangle defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Triangle2d.relativeTo localFrame exampleTriangle
    --> Triangle2d
    -->     ( Point2d ( 0, -1 )
    -->     , Point2d ( 1, -1 )
    -->     , Point2d ( 0, 1 )
    -->     )

-}
relativeTo : Frame2d -> Triangle2d -> Triangle2d
relativeTo frame =
    map (Point2d.relativeTo frame)


{-| Take a triangle considered to be defined in local coordinates relative to a
given reference frame, and return that triangle expressed in global coordinates.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    Triangle2d.placeIn localFrame exampleTriangle
    --> Triangle2d
    -->     ( Point2d ( 2, 3 )
    -->     , Point2d ( 3, 3 )
    -->     , Point2d ( 2, 5 )
    -->     )

-}
placeIn : Frame2d -> Triangle2d -> Triangle2d
placeIn frame =
    map (Point2d.placeIn frame)


{-| Take a triangle defined in 2D coordinates within a particular sketch
plane and return the corresponding triangle in 3D.

    Triangle2d.placeOnto SketchPlane3d.xz exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( 1, 0, 1 )
    -->     , Point3d ( 2, 0, 1 )
    -->     , Point3d ( 1, 0, 3 )
    -->     )

-}
placeOnto : SketchPlane3d -> Triangle2d -> Triangle3d
placeOnto sketchPlane =
    let
        place =
            Point2d.placeOnto sketchPlane
    in
        \triangle ->
            let
                ( p1, p2, p3 ) =
                    vertices triangle
            in
                Triangle3d ( place p1, place p2, place p3 )


{-| Get the minimal bounding box containing a given triangle.

    Triangle2d.boundingBox exampleTriangle
    --> BoundingBox2d
    -->     { minX = 1
    -->     , maxX = 2
    -->     , minY = 1
    -->     , maxY = 3
    -->     }

-}
boundingBox : Triangle2d -> BoundingBox2d
boundingBox triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3
    in
        BoundingBox2d
            { minX = min x1 (min x2 x3)
            , maxX = max x1 (max x2 x3)
            , minY = min y1 (min y2 y3)
            , maxY = max y1 (max y2 y3)
            }
