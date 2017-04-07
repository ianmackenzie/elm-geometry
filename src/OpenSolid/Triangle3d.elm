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


module OpenSolid.Triangle3d
    exposing
        ( vertices
        , edges
        , centroid
        , area
        , normalDirection
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , projectOnto
        , map
        , relativeTo
        , placeIn
        , projectInto
        , boundingBox
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/triangle3d.svg" alt="Triangle3d" width="160">

A `Triangle3d` represents a triangle in 3D space, and is defined by its three
vertices. This module contains triangle-related functionality such as:

  - Finding the area and centroid of triangles
  - Scaling, rotating, translating, mirroring and projecting triangles
  - Converting triangles between different coordinate systems

Triangles can be constructed by passing a tuple of vertices to the `Triangle3d`
constructor, for example

    exampleTriangle =
        Triangle3d
            ( Point3d ( 1, 0, 0 )
            , Point3d ( 2, 0, 0 )
            , Point3d ( 2, 1, 3 )
            )


# Accessors

@docs vertices, edges


# Basics

@docs centroid, area, normalDirection


# Transformations

Transforming a triangle is equivalent to transforming its vertices.

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto, map


# Coordinate frames

Functions for transforming triangles between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto


# Bounds

@docs boundingBox

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Point3d as Point3d


{-| Get the vertices of a triangle.

    ( p1, p2, p3 ) =
        Triangle3d.vertices exampleTriangle


    --> p1 = Point3d ( 1, 0, 0 )
    --> p2 = Point3d ( 2, 0, 0 )
    --> p3 = Point3d ( 2, 1, 3 )

-}
vertices : Triangle3d -> ( Point3d, Point3d, Point3d )
vertices (Triangle3d vertices_) =
    vertices_


{-| Get the edges of a triangle: from the first vertex to the second, from the
second to the third, and from the third back to the first.

    ( e1, e2, e3 ) =
        Triangle3d.edges exampleTriangle


    --> e1 = LineSegment3d ( Point3d ( 1, 0, 0 ), Point3d ( 2, 0, 0 ) )
    --> e2 = LineSegment3d ( Point3d ( 2, 0, 0 ), Point3d ( 2, 1, 3 ) )
    --> e3 = LineSegment3d ( Point3d ( 2, 1, 3 ), Point3d ( 1, 0, 0 ) )

-}
edges : Triangle3d -> ( LineSegment3d, LineSegment3d, LineSegment3d )
edges triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        ( LineSegment3d ( p1, p2 )
        , LineSegment3d ( p2, p3 )
        , LineSegment3d ( p3, p1 )
        )


{-| Get the centroid (center of mass) of a triangle.

    Triangle3d.centroid exampleTriangle
    --> Point3d ( 1.6667, 0.6667, 1 )

-}
centroid : Triangle3d -> Point3d
centroid triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Point3d.vectorFrom p1 p2

        secondVector =
            Point3d.vectorFrom p1 p3

        displacement =
            Vector3d.scaleBy (1.0 / 3.0) (Vector3d.sum firstVector secondVector)
    in
        Point3d.translateBy displacement p1


{-| Get the area of a triangle. This value is always positive.

    Triangle3d.area exampleTriangle
    --> 1.5811

-}
area : Triangle3d -> Float
area triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Point3d.vectorFrom p1 p2

        secondVector =
            Point3d.vectorFrom p1 p3
    in
        0.5 * Vector3d.length (Vector3d.crossProduct firstVector secondVector)


{-| Attempt to find the normal direction to a triangle. The resulting direction
will be oriented such that the triangle vertices are in counterclockwise order
around it according to the right-hand rule. If the triangle is degenerate (its
three vertices are collinear), returns `Nothing`.

    Triangle3d.normalDirection exampleTriangle
    --> Just (Direction3d ( 0, -0.9487, 0.3162 ))

-}
normalDirection : Triangle3d -> Maybe Direction3d
normalDirection triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        v1 =
            Point3d.vectorFrom p1 p2

        v2 =
            Point3d.vectorFrom p2 p3
    in
        Vector3d.direction (Vector3d.crossProduct v1 v2)


{-| Scale a triangle about a given point by a given scale.

    Triangle3d.scaleAbout Point3d.origin 2 exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( 2, 0, 0 )
    -->     , Point3d ( 4, 0, 0 )
    -->     , Point3d ( 4, 2, 6 )
    -->     )

-}
scaleAbout : Point3d -> Float -> Triangle3d -> Triangle3d
scaleAbout centerPoint scale =
    map (Point3d.scaleAbout centerPoint scale)


{-| Rotate a triangle around a given axis by a given angle (in radians).

    Triangle3d.rotateAround Axis3d.z (degrees 90) exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( 0, 1, 0 )
    -->     , Point3d ( 0, 2, 0 )
    -->     , Point3d ( -1, 2, 3 )
    -->     )

-}
rotateAround : Axis3d -> Float -> Triangle3d -> Triangle3d
rotateAround axis angle =
    map (Point3d.rotateAround axis angle)


{-| Translate a triangle by a given displacement.

    displacement =
        Vector3d ( 2, -1, 3 )

    Triangle3d.translateBy displacement exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( 3, -1, 3 )
    -->     , Point3d ( 4, -1, 3 )
    -->     , Point3d ( 4, 0, 6 )
    -->     )

-}
translateBy : Vector3d -> Triangle3d -> Triangle3d
translateBy vector =
    map (Point3d.translateBy vector)


{-| Mirror a triangle across a given plane.

    Triangle3d.mirrorAcross Plane3d.yz exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( -1, 0, 0 )
    -->     ( Point3d ( -2, 0, 0 )
    -->     ( Point3d ( -2, 1, 3 )
    -->     )

-}
mirrorAcross : Plane3d -> Triangle3d -> Triangle3d
mirrorAcross plane =
    map (Point3d.mirrorAcross plane)


{-| Project a triangle onto a given plane.

    Triangle3d.projectOnto Plane3d.xy exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( 1, 0, 0 )
    -->     , Point3d ( 2, 0, 0 )
    -->     , Point3d ( 2, 1, 0 )
    -->     )

    Triangle3d.projectOnto Plane3d.xz exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( 1, 0, 0 )
    -->     , Point3d ( 2, 0, 0 )
    -->     , Point3d ( 2, 0, 3 )
    -->     )

-}
projectOnto : Plane3d -> Triangle3d -> Triangle3d
projectOnto plane =
    map (Point3d.projectOnto plane)


{-| Transform each vertex of a triangle by a given function and create a new
triangle from the resulting points. Most other transformation functions can be
defined in terms of `map`; for example,

    Triangle.projectOnto Plane3d.xz triangle

is equivalent to

    Triangle.map (Point3d.projectOnto Plane3d.xz) triangle

-}
map : (Point3d -> Point3d) -> Triangle3d -> Triangle3d
map function triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        Triangle3d ( function p1, function p2, function p3 )


{-| Take a triangle defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d ( 2, 1, 3 ))

    Triangle3d.relativeTo localFrame exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( -1, -1, -3 )
    -->     , Point3d ( 0, -1, -3 )
    -->     , Point3d ( 0, 0, 0 )
    -->     )

-}
relativeTo : Frame3d -> Triangle3d -> Triangle3d
relativeTo frame =
    map (Point3d.relativeTo frame)


{-| Take a triangle considered to be defined in local coordinates relative to a
given reference frame, and return that triangle expressed in global coordinates.

    localFrame =
        Frame3d.at (Point3d ( 2, 1, 3 ))

    Triangle3d.placeIn localFrame exampleTriangle
    --> Triangle3d
    -->     ( Point3d ( 3, 1, 3 )
    -->     , Point3d ( 4, 1, 3 )
    -->     , Point3d ( 4, 2, 6 )
    -->     )

-}
placeIn : Frame3d -> Triangle3d -> Triangle3d
placeIn frame =
    map (Point3d.placeIn frame)


{-| Project a triangle into a given sketch plane. Conceptually, this projects
the triangle onto the plane and then expresses the projected triangle in 2D
sketch coordinates.

    Triangle3d.projectInto SketchPlane3d.xy exampleTriangle
    --> Triangle2d
    -->     ( Point2d ( 1, 0 )
    -->     , Point2d ( 2, 0 )
    -->     , Point2d ( 2, 1 )
    -->     )

    Triangle3d.projectInto SketchPlane3d.zx exampleTriangle
    --> Triangle2d
    -->     ( Point2d ( 0, 1 )
    -->     , Point2d ( 0, 2 )
    -->     , Point2d ( 3, 2 )
    -->     )

-}
projectInto : SketchPlane3d -> Triangle3d -> Triangle2d
projectInto sketchPlane triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        project =
            Point3d.projectInto sketchPlane
    in
        Triangle2d ( project p1, project p2, project p3 )


{-| Get the minimal bounding box containing a given triangle.

    Triangle3d.boundingBox exampleTriangle
    --> BoundingBox3d
    -->     { minX = 1
    -->     , maxX = 2
    -->     , minY = 0
    -->     , maxY = 1
    -->     , minZ = 0
    -->     , maxZ = 3
    -->     }

-}
boundingBox : Triangle3d -> BoundingBox3d
boundingBox triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ( x3, y3, z3 ) =
            Point3d.coordinates p3
    in
        BoundingBox3d
            { minX = min x1 (min x2 x3)
            , maxX = max x1 (max x2 x3)
            , minY = min y1 (min y2 y3)
            , maxY = max y1 (max y2 y3)
            , minZ = min z1 (min z2 z3)
            , maxZ = max z1 (max z2 z3)
            }
