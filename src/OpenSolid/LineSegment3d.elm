{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.LineSegment3d
    exposing
        ( endpoints
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
        , placeOnto
        , boundingBox
        )

{-| Various functions for creating and working with `LineSegment3d` values. For
the examples below, assume that all OpenSolid core types have been imported
using

    import OpenSolid.Core.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.LineSegment3d as LineSegment3d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

Line segments can be constructed explicitly by passing a tuple of start and end
points to the `LineSegment3d` constructor. For all examples, assume the
following line segment has been defined:

    lineSegment =
        LineSegment3d
            ( Point3d ( 1, 2, 3 )
            , Point3d ( 4, 5, 6 )
            )

# Endpoints

@docs endpoints, startPoint, endPoint, reverse

# Interpolation

@docs midpoint, interpolate

# Length and direction

@docs length, squaredLength, direction, normalDirection, vector

# Transformations

Transforming a line segment is equivalent to transforming its start and end
points and forming a new line segment between the resulting points.

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto, map

# Coordinate frames

Functions for transforming points between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn

# Sketch planes

@docs projectInto, placeOnto

# Bounds

@docs boundingBox
-}

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Point3d as Point3d
import OpenSolid.LineSegment2d as LineSegment2d


{-| Get the endpoints of a line segment as a tuple.

    ( p1, p2 ) =
        LineSegment3d.endpoints lineSegment

    p1 ==
        Point3d ( 1, 2, 3 )

    p2 ==
        Point3d ( 4, 5, 6 )
-}
endpoints : LineSegment3d -> ( Point3d, Point3d )
endpoints (LineSegment3d endpoints') =
    endpoints'


{-| Get the start point of a line segment.

    LineSegment3d.startPoint lineSegment ==
        Point3d ( 1, 2, 3 )
-}
startPoint : LineSegment3d -> Point3d
startPoint (LineSegment3d ( start, _ )) =
    start


{-| Get the end point of a line segment.

    LineSegment3d.endPoint lineSegment ==
        Point3d ( 4, 5, 6 )
-}
endPoint : LineSegment3d -> Point3d
endPoint (LineSegment3d ( _, end )) =
    end


{-| Reverse a line segment, swapping its start and end points.

    LineSegment3d.reverse lineSegment ==
        LineSegment3d
            ( Point3d ( 4, 5, 6 )
            , Point3d ( 1, 2, 3 )
            )
-}
reverse : LineSegment3d -> LineSegment3d
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment3d ( p2, p1 )


{-| Get the midpoint of a line segment.

    LineSegment3d.midpoint lineSegment ==
        Point3d ( 2.5, 3.5, 4.5 )
-}
midpoint : LineSegment3d -> Point3d
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| Interpolate a line segment between its start and end points; a value of 0.0
corresponds to the start point of the line segment, a value of 0.5 corresponds
to its midpoint and a value of 1.0 corresponds to its end point. Negative values
or values greater than 1 can be used to extrapolate.

    LineSegment3d.interpolate lineSegment (1 / 3) ==
        Point3d ( 2, 4, 5 )

    LineSegment3d.interpolate lineSegment (-1 / 3) ==
        Point3d ( 0, 1, 2 )
-}
interpolate : LineSegment3d -> Float -> Point3d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
        Point3d.interpolate start end


{-| Get the length of a line segment.

    LineSegment3d.length lineSegment ==
        5.1962
-}
length : LineSegment3d -> Float
length =
    vector >> Vector3d.length


{-| Get the squared length of a line segment. Slightly more efficient than
`length` since it avoids a square root.

    LineSegment3d.squaredLength lineSegment ==
        27
-}
squaredLength : LineSegment3d -> Float
squaredLength =
    vector >> Vector3d.squaredLength


{-| Get the direction from a line segment's start point to its end point. If the
line segment has zero length (the start and end points are the same), returns
`Nothing`.

    LineSegment3d.direction lineSegment ==
        Just (Direction3d ( 0.5774, 0.5774, 0.5774 ))
-}
direction : LineSegment3d -> Maybe Direction3d
direction =
    vector >> Vector3d.direction


{-| Get an arbitrary direction perpendicular to a line segment. Equivalent to
`LineSegment3d.direction >> Maybe.map Direction3d.perpendicularTo`. If the line
segment has zero length, returns `Nothing`.

    LineSegment3d.normalDirection lineSegment ==
        Just (Direction3d ( 0, -0.7071, 0.7071 ))
-}
normalDirection : LineSegment3d -> Maybe Direction3d
normalDirection =
    vector >> Vector3d.perpendicularTo >> Vector3d.direction


{-| Get the vector from a given line segment's start point to its end point.

    LineSegment3d.vector lineSegment ==
        Vector3d ( 2, 2, 2 )
-}
vector : LineSegment3d -> Vector3d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point3d.vectorFrom p1 p2


{-| Perform a uniform scaling about the given center point.

    point =
        Point3d ( 1, 1, 1 )

    LineSegment3d.scaleAbout point 2 lineSegment ==
        LineSegment3d
            ( Point3d ( 1, 3, 5 )
            , Point3d ( 7, 9, 11 )
            )
-}
scaleAbout : Point3d -> Float -> LineSegment3d -> LineSegment3d
scaleAbout point scale =
    map (Point3d.scaleAbout point scale)


{-| Rotate around a given axis by a given angle (in radians).

    LineSegment3d.rotateAround Axis3d.z (degrees 90) lineSegment ==
        LineSegment3d
            ( Point3d ( -2, 1, 3 )
            , Point3d ( -5, 4, 6 )
            )
-}
rotateAround : Axis3d -> Float -> LineSegment3d -> LineSegment3d
rotateAround axis angle =
    map (Point3d.rotateAround axis angle)


{-| Translate a line segment by a given displacement.

    displacement =
        Vector3d ( 1, 2, 3 )

    LineSegment3d.translateBy displacement lineSegment ==
        LineSegment3d
            ( Point3d ( 2, 4, 6 )
            , Point3d ( 5, 7, 9 )
            )
-}
translateBy : Vector3d -> LineSegment3d -> LineSegment3d
translateBy vector =
    map (Point3d.translateBy vector)


{-| Mirror a line segment across a plane.

    LineSegment3d.mirrorAcross Plane3d.xy lineSegment ==
        LineSegment3d
            ( Point3d ( 1, 2, -3 )
            , Point3d ( 4, 5, -6 )
            )
-}
mirrorAcross : Plane3d -> LineSegment3d -> LineSegment3d
mirrorAcross plane =
    map (Point3d.mirrorAcross plane)


{-| Project a line segment onto a plane.

    LineSegment3d.projectOnto Plane3d.yz lineSegment ==
        LineSegment3d
            ( Point3d ( 0, 2, 3 )
            , Point3d ( 0, 5, 6 )
            )
-}
projectOnto : Plane3d -> LineSegment3d -> LineSegment3d
projectOnto plane =
    map (Point3d.projectOnto plane)


{-| Transform the start and end points of a line segment by a given function
and create a new line segment from the resulting points. Most other
transformation functions can be defined in terms of `map`; for example,

    LineSegment3d.projectOnto Plane3d.xy lineSegment ==
        LineSegment3d.map (Point3d.projectOnto Plane3d.xy) lineSegment
-}
map : (Point3d -> Point3d) -> LineSegment3d -> LineSegment3d
map function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment3d ( function p1, function p2 )


relativeTo : Frame3d -> LineSegment3d -> LineSegment3d
relativeTo frame =
    map (Point3d.relativeTo frame)


placeIn : Frame3d -> LineSegment3d -> LineSegment3d
placeIn frame =
    map (Point3d.placeIn frame)


projectInto : SketchPlane3d -> LineSegment3d -> LineSegment2d
projectInto sketchPlane lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment

        project =
            Point3d.projectInto sketchPlane
    in
        LineSegment2d ( project p1, project p2 )


placeOnto : SketchPlane3d -> LineSegment2d -> LineSegment3d
placeOnto sketchPlane lineSegment2d =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment2d

        place =
            Point3d.placeOnto sketchPlane
    in
        LineSegment3d ( place p1, place p2 )


boundingBox : LineSegment3d -> BoundingBox3d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point3d.hull p1 p2
