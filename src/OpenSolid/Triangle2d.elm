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

{-| Various functions for creating and working with `Triangle2d` values.
Triangles are defined by their three vertices and can be constructed by passing
a tuple of vertices to the `Triangle2d` constructor, for example

    exampleTriangle =
        Triangle2d
            ( Point2d ( 1, 1 )
            , Point2d ( 2, 1 )
            , Point2d ( 1, 3 )
            )

## Reading this documentation

For the examples below, assume that `exampleTriangle` has been defined as above,
all OpenSolid core types have been imported using

    import OpenSolid.Geometry.Types exposing (..)

and all other necessary modules have been imported using the following pattern:

    import OpenSolid.Triangle2d as Triangle2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

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


vertices : Triangle2d -> ( Point2d, Point2d, Point2d )
vertices (Triangle2d vertices') =
    vertices'


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
            Vector2d.times (1.0 / 3.0) (Vector2d.plus secondVector firstVector)
    in
        Point2d.translateBy displacement p1


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


area : Triangle2d -> Float
area =
    counterclockwiseArea >> abs


clockwiseArea : Triangle2d -> Float
clockwiseArea triangle =
    -(counterclockwiseArea triangle)


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


scaleAbout : Point2d -> Float -> Triangle2d -> Triangle2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


rotateAround : Point2d -> Float -> Triangle2d -> Triangle2d
rotateAround centerPoint angle =
    map (Point2d.rotateAround centerPoint angle)


translateBy : Vector2d -> Triangle2d -> Triangle2d
translateBy vector =
    map (Point2d.translateBy vector)


mirrorAcross : Axis2d -> Triangle2d -> Triangle2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


map : (Point2d -> Point2d) -> Triangle2d -> Triangle2d
map function triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        Triangle2d ( function p1, function p2, function p3 )


relativeTo : Frame2d -> Triangle2d -> Triangle2d
relativeTo frame =
    map (Point2d.relativeTo frame)


placeIn : Frame2d -> Triangle2d -> Triangle2d
placeIn frame =
    map (Point2d.placeIn frame)


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
