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


module OpenSolid.LineSegment3d
    exposing
        ( along
        , endpoints
        , startPoint
        , endPoint
        , reverse
        , midpoint
        , interpolate
        , length
        , squaredLength
        , direction
        , normalDirection
        , vector
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

{-| <img src="https://opensolid.github.io/images/geometry/icons/lineSegment3d.svg" alt="LineSegment3d" width="160">

A `LineSegment3d` is a line between two points in 3D. This module contains
functionality such as:

  - Interpolating within a line segment or measuring its length
  - Scaling, rotating, translating, mirroring or projecting a line segment
  - Converting a line segment between local and global coordinates in different
    reference frames

Line segments can be constructed by passing a tuple of start and end points to
the `LineSegment3d` constructor, for example

    exampleLineSegment =
        LineSegment3d
            ( Point3d ( 1, 2, 3 )
            , Point3d ( 4, 5, 6 )
            )


# Constructors

@docs along


# Endpoints

@docs startPoint, endPoint, endpoints, reverse


# Interpolation

@docs midpoint, interpolate


# Length and direction

@docs length, squaredLength, direction, normalDirection, vector


# Transformations

Transforming a line segment is equivalent to transforming its start and end
points and forming a new line segment between the resulting points.

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto, map


# Coordinate frames

Functions for transforming line segments between local and global coordinates in
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


{-| Construct a line segment collinear with the given axis, with its endpoints
at the given distances from the axis' origin point.

    LineSegment3d.along Axis3d.x 3 5
    --> LineSegment3d
    -->     ( Point3d ( 3, 0, 0 )
    -->     , Point3d ( 5, 0, 0 )
    -->     )

    LineSegment3d.along Axis3d.y 2 -4
    --> LineSegment3d
    -->     ( Point3d ( 0, 2, 0 )
    -->     , Point3d ( 0, -4, 0 )
    -->     )

-}
along : Axis3d -> Float -> Float -> LineSegment3d
along axis start end =
    LineSegment3d ( Point3d.along axis start, Point3d.along axis end )


{-| Get the start point of a line segment.

    LineSegment3d.startPoint exampleLineSegment
    --> Point3d ( 1, 2, 3 )

-}
startPoint : LineSegment3d -> Point3d
startPoint (LineSegment3d ( start, _ )) =
    start


{-| Get the end point of a line segment.

    LineSegment3d.endPoint exampleLineSegment
    --> Point3d ( 4, 5, 6 )

-}
endPoint : LineSegment3d -> Point3d
endPoint (LineSegment3d ( _, end )) =
    end


{-| Get the endpoints of a line segment as a tuple.

    ( p1, p2 ) =
        LineSegment3d.endpoints lineSegment

-}
endpoints : LineSegment3d -> ( Point3d, Point3d )
endpoints (LineSegment3d endpoints_) =
    endpoints_


{-| Reverse a line segment, swapping its start and end points.

    LineSegment3d.reverse exampleLineSegment
    --> LineSegment3d
    -->     ( Point3d ( 4, 5, 6 )
    -->     , Point3d ( 1, 2, 3 )
    -->     )

-}
reverse : LineSegment3d -> LineSegment3d
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment3d ( p2, p1 )


{-| Get the midpoint of a line segment.

    LineSegment3d.midpoint exampleLineSegment
    --> Point3d ( 2.5, 3.5, 4.5 )

-}
midpoint : LineSegment3d -> Point3d
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| Interpolate a line segment between its start and end points; a value of 0.0
corresponds to the start point of the line segment, a value of 0.5 corresponds
to its midpoint and a value of 1.0 corresponds to its end point. Values less
than 0.0 or greater than 1.0 can be used to extrapolate.

    LineSegment3d.interpolate exampleLineSegment (1 / 3)
    --> Point3d ( 2, 4, 5 )

    LineSegment3d.interpolate exampleLineSegment (-1 / 3)
    --> Point3d ( 0, 1, 2 )

-}
interpolate : LineSegment3d -> Float -> Point3d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
        Point3d.interpolateFrom start end


{-| Get the length of a line segment.

    LineSegment3d.length exampleLineSegment
    --> 5.1962

-}
length : LineSegment3d -> Float
length =
    vector >> Vector3d.length


{-| Get the squared length of a line segment. Slightly more efficient than
`length` since it avoids a square root.

    LineSegment3d.squaredLength exampleLineSegment
    --> 27

-}
squaredLength : LineSegment3d -> Float
squaredLength =
    vector >> Vector3d.squaredLength


{-| Get the direction from a line segment's start point to its end point. If the
line segment has zero length (the start and end points are the same), returns
`Nothing`.

    LineSegment3d.direction exampleLineSegment
    --> Just (Direction3d ( 0.5774, 0.5774, 0.5774 ))

-}
direction : LineSegment3d -> Maybe Direction3d
direction =
    vector >> Vector3d.direction


{-| Get an arbitrary direction perpendicular to a line segment. If the line
segment has zero length, returns `Nothing`.

    LineSegment3d.normalDirection exampleLineSegment
    --> Just (Direction3d ( 0, -0.7071, 0.7071 ))

-}
normalDirection : LineSegment3d -> Maybe Direction3d
normalDirection =
    vector >> Vector3d.perpendicularTo >> Vector3d.direction


{-| Get the vector from a line segment's start point to its end point.

    LineSegment3d.vector exampleLineSegment
    --> Vector3d ( 2, 2, 2 )

-}
vector : LineSegment3d -> Vector3d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point3d.vectorFrom p1 p2


{-| Scale a line segment about the given center point by the given scale.

    point =
        Point3d ( 1, 1, 1 )

    LineSegment3d.scaleAbout point 2 exampleLineSegment
    --> LineSegment3d
    -->     ( Point3d ( 1, 3, 5 )
    -->     , Point3d ( 7, 9, 11 )
    -->     )

-}
scaleAbout : Point3d -> Float -> LineSegment3d -> LineSegment3d
scaleAbout point scale =
    map (Point3d.scaleAbout point scale)


{-| Rotate a line segment around a given axis by a given angle (in radians).

    LineSegment3d.rotateAround Axis3d.z (degrees 90) exampleLineSegment
    --> LineSegment3d
    -->     ( Point3d ( -2, 1, 3 )
    -->     , Point3d ( -5, 4, 6 )
    -->     )

-}
rotateAround : Axis3d -> Float -> LineSegment3d -> LineSegment3d
rotateAround axis angle =
    map (Point3d.rotateAround axis angle)


{-| Translate a line segment by a given displacement.

    displacement =
        Vector3d ( 1, 2, 3 )

    LineSegment3d.translateBy displacement exampleLineSegment
    --> LineSegment3d
    -->     ( Point3d ( 2, 4, 6 )
    -->     , Point3d ( 5, 7, 9 )
    -->     )

-}
translateBy : Vector3d -> LineSegment3d -> LineSegment3d
translateBy vector =
    map (Point3d.translateBy vector)


{-| Mirror a line segment across a plane.

    LineSegment3d.mirrorAcross Plane3d.xy exampleLineSegment
    --> LineSegment3d
    -->     ( Point3d ( 1, 2, -3 )
    -->     , Point3d ( 4, 5, -6 )
    -->     )

-}
mirrorAcross : Plane3d -> LineSegment3d -> LineSegment3d
mirrorAcross plane =
    map (Point3d.mirrorAcross plane)


{-| Project a line segment onto a plane.

    LineSegment3d.projectOnto Plane3d.yz exampleLineSegment
    --> LineSegment3d
    -->     ( Point3d ( 0, 2, 3 )
    -->     , Point3d ( 0, 5, 6 )
    -->     )

-}
projectOnto : Plane3d -> LineSegment3d -> LineSegment3d
projectOnto plane =
    map (Point3d.projectOnto plane)


{-| Transform the start and end points of a line segment by a given function
and create a new line segment from the resulting points. Most other
transformation functions can be defined in terms of `map`; for example,

    LineSegment3d.projectOnto Plane3d.xy

is equivalent to

    LineSegment3d.map (Point3d.projectOnto Plane3d.xy)

-}
map : (Point3d -> Point3d) -> LineSegment3d -> LineSegment3d
map function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment3d ( function p1, function p2 )


{-| Take a line segment defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    LineSegment3d.relativeTo localFrame exampleLineSegment
    --> LineSegment3d
    -->     ( Point3d ( 0, 0, 0 )
    -->     , Point3d ( 3, 3, 3 )
    -->     )

-}
relativeTo : Frame3d -> LineSegment3d -> LineSegment3d
relativeTo frame =
    map (Point3d.relativeTo frame)


{-| Take a line segment considered to be defined in local coordinates relative
to a given reference frame, and return that line segment expressed in global
coordinates.

    localFrame =
        Frame3d.at (Point3d ( 1, 2, 3 ))

    LineSegment3d.placeIn localFrame exampleLineSegment
    --> LineSegment3d
    -->     ( Point3d ( 2, 4, 6 )
    -->     , Point3d ( 5, 7, 9 )
    -->     )

-}
placeIn : Frame3d -> LineSegment3d -> LineSegment3d
placeIn frame =
    map (Point3d.placeIn frame)


{-| Project a line segment into a given sketch plane. Conceptually, this
projects the line segment onto the plane and then expresses the projected
line segment in 2D sketch coordinates.

    LineSegment3d.projectInto SketchPlane3d.xy exampleLineSegment
    --> LineSegment2d
    -->     ( Point2d ( 1, 2 )
    -->     , Point2d ( 4, 5 )
    -->     )

    LineSegment3d.projectInto SketchPlane3d.yz exampleLineSegment
    --> LineSegment2d
    -->     ( Point2d ( 2, 3 )
    -->     , Point2d ( 5, 6 )
    -->     )

    LineSegment3d.projectInto SketchPlane3d.zx exampleLineSegment
    --> LineSegment2d
    -->     ( Point2d ( 3, 1 )
    -->     , Point2d ( 6, 4 )
    -->     )

-}
projectInto : SketchPlane3d -> LineSegment3d -> LineSegment2d
projectInto sketchPlane lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment

        project =
            Point3d.projectInto sketchPlane
    in
        LineSegment2d ( project p1, project p2 )


{-| Get the minimal bounding box containing a line segment.

    LineSegment3d.boundingBox exampleLineSegment
    --> BoundingBox3d
    -->     { minX = 1
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     , minZ = 3
    -->     , maxZ = 6
    -->     }

-}
boundingBox : LineSegment3d -> BoundingBox3d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point3d.hull p1 p2
