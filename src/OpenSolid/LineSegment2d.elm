{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.LineSegment2d
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
        , boundingBox
        )

{-| Various functions for creating and working with `LineSegment2d` values. For
the examples below, assume that all OpenSolid core types have been imported
using

    import OpenSolid.Core.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.LineSegment2d as LineSegment2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

Line segments can be constructed explicitly by passing a tuple of start and end
points to the `LineSegment2d` constructor. For all examples, assume the
following line segment has been defined:

    lineSegment =
        LineSegment2d
            ( Point2d ( 1, 2 )
            , Point2d ( 3, 4 )
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

# Bounds

@docs boundingBox
-}

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Point2d as Point2d


{-| Get the endpoints of a line segment as a tuple.

    ( p1, p2 ) =
        LineSegment2d.endpoints lineSegment

    p1 ==
        Point2d ( 1, 2 )

    p2 ==
        Point2d ( 3, 4 )
-}
endpoints : LineSegment2d -> ( Point2d, Point2d )
endpoints (LineSegment2d endpoints') =
    endpoints'


{-| Get the start point of a line segment.

    LineSegment2d.startPoint lineSegment ==
        Point2d ( 1, 2 )
-}
startPoint : LineSegment2d -> Point2d
startPoint (LineSegment2d ( start, _ )) =
    start


{-| Get the end point of a line segment.

    LineSegment2d.endPoint lineSegment ==
        Point2d ( 3, 4 )
-}
endPoint : LineSegment2d -> Point2d
endPoint (LineSegment2d ( _, end )) =
    end


{-| Reverse a line segment, swapping its start and end points.

    LineSegment2d.reverse lineSegment ==
        LineSegment2d
            ( Point2d ( 3, 4 )
            , Point2d ( 1, 2 )
            )
-}
reverse : LineSegment2d -> LineSegment2d
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment2d ( p2, p1 )


{-| Get the midpoint of a line segment.

    LineSegment2d.midpoint lineSegment ==
        Point2d ( 2, 3 )
-}
midpoint : LineSegment2d -> Point2d
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| Interpolate a line segment between its start and end points; a value of 0.0
corresponds to the start point of the line segment, a value of 0.5 corresponds
to its midpoint and a value of 1.0 corresponds to its end point. Negative values
or values greater than 1 can be used to extrapolate.

    LineSegment2d.interpolate lineSegment 0.25 ==
        Point2d ( 1.5, 2.5 )

    LineSegment2d.interpolate lineSegment 1.5 ==
        Point2d ( 4, 5 )
-}
interpolate : LineSegment2d -> Float -> Point2d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
        Point2d.interpolate start end


{-| Get the length of a line segment.

    LineSegment2d.length lineSegment ==
        2.8284
-}
length : LineSegment2d -> Float
length =
    vector >> Vector2d.length


{-| Get the squared length of a line segment. Slightly more efficient than
`length` since it avoids a square root.

    LineSegment2d.squaredLength lineSegment ==
        8
-}
squaredLength : LineSegment2d -> Float
squaredLength =
    vector >> Vector2d.squaredLength


{-| Get the direction from a line segment's start point to its end point. If the
line segment has zero length (the start and end points are the same), returns
`Nothing`.

    LineSegment2d.direction lineSegment ==
        Just (Direction2d ( 0.7071, 0.7071 ))
-}
direction : LineSegment2d -> Maybe Direction2d
direction =
    vector >> Vector2d.direction


{-| Get the direction perpendicular to a line segment, pointing to the left.
Equivalent to
`LineSegment2d.direction >> Maybe.map Direction2d.perpendicularTo`. If the line
segment has zero length, returns `Nothing`.

    LineSegment2d.normalDirection lineSegment ==
        Just (Direction2d ( -0.7071, 0.7071 ))
-}
normalDirection : LineSegment2d -> Maybe Direction2d
normalDirection =
    vector >> Vector2d.perpendicularTo >> Vector2d.direction


{-| Get the vector from a given line segment's start point to its end point.

    LineSegment2d.vector lineSegment ==
        Vector2d ( 2, 2 )
-}
vector : LineSegment2d -> Vector2d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point2d.vectorFrom p1 p2


{-| Perform a uniform scaling about the given center point.

    point =
        Point2d ( 1, 1 )

    LineSegment2d.scaleAbout point 2 lineSegment ==
        LineSegment2d
            ( Point2d ( 1, 3 )
            , Point2d ( 5, 7 )
            )
-}
scaleAbout : Point2d -> Float -> LineSegment2d -> LineSegment2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


{-| Rotate around a given center point counterclockwise by a given angle (in
radians).

    LineSegment2d.rotateAround Point2d.origin (degrees 90) lineSegment ==
        LineSegment2d
            ( Point2d ( -2, 1 )
            , Point2d ( -4, 3 )
            )
-}
rotateAround : Point2d -> Float -> LineSegment2d -> LineSegment2d
rotateAround centerPoint angle =
    map (Point2d.rotateAround centerPoint angle)


{-| Translate a line segment by a given displacement.

    displacement =
        Vector2d ( 1, 2 )

    LineSegment2d.translateBy displacement lineSegment ==
        LineSegment2d
            ( Point2d ( 2, 4 )
            , Point2d ( 4, 6 )
            )
-}
translateBy : Vector2d -> LineSegment2d -> LineSegment2d
translateBy vector =
    map (Point2d.translateBy vector)


{-| Mirror a line segment across an axis.

    LineSegment2d.mirrorAcross Axis2d.y lineSegment ==
        LineSegment2d
            ( Point2d ( -1, 2 )
            , Point2d ( -3, 4 )
            )

Note that the endpoints of a mirrored segment are equal to the mirrored
endpoints of the original segment, but the `normalDirection` of a mirrored
segment is the *opposite* of the mirrored normal direction of the original
segment (since the normal direction is always considered to be 'to the left' of
the line segment). In some cases it may be necessary to `reverse` the mirrored
segment or otherwise account for the normal direction being 'wrong'.
-}
mirrorAcross : Axis2d -> LineSegment2d -> LineSegment2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


{-| Project a line segment onto an axis.

    LineSegment2d.projectOnto Axis2d.x lineSegment ==
        LineSegment2d
            ( Point2d ( 1, 0 )
            , Point2d ( 3, 0 )
            )

    LineSegment2d.projectOnto Axis2d.y lineSegment ==
        LineSegment2d
            ( Point2d ( 0, 2 )
            , Point2d ( 0, 4 )
            )
-}
projectOnto : Axis2d -> LineSegment2d -> LineSegment2d
projectOnto axis =
    map (Point2d.projectOnto axis)


{-| Transform the start and end points of a line segment by a given function
and create a new line segment from the resulting points. Most other
transformation functions can be defined in terms of `map`; for example,

    LineSegment2d.projectOnto Axis2d.x lineSegment ==
        LineSegment2d.map (Point2d.projectOnto Axis2d.x) lineSegment
-}
map : (Point2d -> Point2d) -> LineSegment2d -> LineSegment2d
map function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment2d ( function p1, function p2 )


{-| Take a line segment defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    LineSegment2d.relativeTo localFrame lineSegment ==
        LineSegment2d
            ( Point2d ( 0, 0 )
            , Point2d ( 2, 2 )
            )
-}
relativeTo : Frame2d -> LineSegment2d -> LineSegment2d
relativeTo frame =
    map (Point2d.relativeTo frame)


{-| Take a line segment considered to be defined in local coordinates relative
to a given reference frame, and return that line segment expressed in global
coordinates.

    localFrame =
        Frame2d.at (Point2d ( 1, 2 ))

    LineSegment2d.placeIn localFrame lineSegment ==
        LineSegment2d
            ( Point2d ( 2, 4 )
            , Point2d ( 4, 6 )
            )
-}
placeIn : Frame2d -> LineSegment2d -> LineSegment2d
placeIn frame =
    map (Point2d.placeIn frame)


{-| Get the minimal bounding box containing a given line segment.

    LineSegment2d.boundingBox lineSegment ==
        BoundingBox2d
            { minX = 1
            , maxX = 3
            , minY = 2
            , maxY = 4
            }
-}
boundingBox : LineSegment2d -> BoundingBox2d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point2d.hull p1 p2
