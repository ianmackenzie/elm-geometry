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

@docs endpoints, startPoint, endPoint

# Interpolation

@docs midpoint, interpolate

# Length and direction

@docs length, squaredLength, direction, normalDirection, vector

# Transformations

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
`LineSegment2d.direction |> Maybe.map Direction2d.perpendicularTo`. If the line
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


scaleAbout : Point2d -> Float -> LineSegment2d -> LineSegment2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


rotateAround : Point2d -> Float -> LineSegment2d -> LineSegment2d
rotateAround centerPoint angle =
    map (Point2d.rotateAround centerPoint angle)


translateBy : Vector2d -> LineSegment2d -> LineSegment2d
translateBy vector =
    map (Point2d.translateBy vector)


mirrorAcross : Axis2d -> LineSegment2d -> LineSegment2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


projectOnto : Axis2d -> LineSegment2d -> LineSegment2d
projectOnto axis =
    map (Point2d.projectOnto axis)


map : (Point2d -> Point2d) -> LineSegment2d -> LineSegment2d
map function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        LineSegment2d ( function p1, function p2 )


relativeTo : Frame2d -> LineSegment2d -> LineSegment2d
relativeTo frame =
    map (Point2d.relativeTo frame)


placeIn : Frame2d -> LineSegment2d -> LineSegment2d
placeIn frame =
    map (Point2d.placeIn frame)


boundingBox : LineSegment2d -> BoundingBox2d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
        Point2d.hull p1 p2
