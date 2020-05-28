--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module LineSegment2d exposing
    ( LineSegment2d
    , fromEndpoints, from, fromPointAndVector, along
    , startPoint, endPoint, endpoints, midpoint, length, direction, perpendicularDirection, vector, boundingBox
    , interpolate
    , intersectionPoint, intersectionWithAxis
    , signedDistanceAlong, signedDistanceFrom
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints
    , at, at_
    , relativeTo, placeIn
    )

{-| A `LineSegment2d` is a line between two points in 2D. This module contains
functionality such as:

  - Interpolating within a line segment or measuring its length
  - Scaling, rotating, translating, mirroring or projecting a line segment
  - Converting a line segment between local and global coordinates in different
    reference frames

@docs LineSegment2d


# Constructors

@docs fromEndpoints, from, fromPointAndVector, along


# Properties

@docs startPoint, endPoint, endpoints, midpoint, length, direction, perpendicularDirection, vector, boundingBox


# Interpolation

@docs interpolate


# Intersection

@docs intersectionPoint, intersectionWithAxis


# Measurement

@docs signedDistanceAlong, signedDistanceFrom


# Transformations

These transformations generally behave just like [the ones in the `Point2d`
module](Point2d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate, Squared)
import Quantity.Interval as Interval exposing (Interval)
import Vector2d exposing (Vector2d)


{-| -}
type alias LineSegment2d units coordinates =
    Types.LineSegment2d units coordinates


{-| Construct a line segment from its two endpoints:

    exampleLineSegment =
        LineSegment2d.fromEndpoints
            ( Point2d.meters 1 2
            , Point2d.meters 3 4
            )

-}
fromEndpoints : ( Point2d units coordinates, Point2d units coordinates ) -> LineSegment2d units coordinates
fromEndpoints =
    Types.LineSegment2d


{-| Construct a line segment from the first point to the second;

    LineSegment2d.from firstPoint secondPoint

is equivalent to

    LineSegment2d.fromEndpoints ( firstPoint, secondPoint )

-}
from : Point2d units coordinates -> Point2d units coordinates -> LineSegment2d units coordinates
from startPoint_ endPoint_ =
    fromEndpoints ( startPoint_, endPoint_ )


{-| Construct a line segment lying on the given axis, with its endpoints at the
given distances from the axis' origin point.

    LineSegment2d.along Axis2d.x
        (Length.meters 3)
        (Length.meters 5)
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.meters 3 0
    -->     , Point2d.meters 5 0
    -->     )

-}
along : Axis2d units coordinates -> Quantity Float units -> Quantity Float units -> LineSegment2d units coordinates
along axis start end =
    fromEndpoints ( Point2d.along axis start, Point2d.along axis end )


{-| Construct a line segment given its start point and the vector from its
start point to its end point;

    LineSegment2d.fromPointAndVector point vector

is equivalent to

    LineSegment2d.fromEndpoints
        ( point
        , point |> Point2d.translateBy vector
        )

-}
fromPointAndVector : Point2d units coordinates -> Vector2d units coordinates -> LineSegment2d units coordinates
fromPointAndVector givenPoint givenVector =
    fromEndpoints ( givenPoint, givenPoint |> Point2d.translateBy givenVector )


{-| Convert a line segment from one units type to another, by providing a
conversion factor given as a rate of change of destination units with respect to
source units.
-}
at : Quantity Float (Rate units2 units1) -> LineSegment2d units1 coordinates -> LineSegment2d units2 coordinates
at rate (Types.LineSegment2d ( p1, p2 )) =
    Types.LineSegment2d ( Point2d.at rate p1, Point2d.at rate p2 )


{-| Convert a line segment from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> LineSegment2d units1 coordinates -> LineSegment2d units2 coordinates
at_ rate lineSegment =
    at (Quantity.inverse rate) lineSegment


{-| Get the start point of a line segment.
-}
startPoint : LineSegment2d units coordinates -> Point2d units coordinates
startPoint (Types.LineSegment2d ( start, _ )) =
    start


{-| Get the end point of a line segment.
-}
endPoint : LineSegment2d units coordinates -> Point2d units coordinates
endPoint (Types.LineSegment2d ( _, end )) =
    end


{-| Get the endpoints of a line segment as a tuple.

    ( p1, p2 ) =
        LineSegment2d.endpoints lineSegment

-}
endpoints : LineSegment2d units coordinates -> ( Point2d units coordinates, Point2d units coordinates )
endpoints (Types.LineSegment2d endpoints_) =
    endpoints_


{-| Reverse a line segment, swapping its start and end points.
-}
reverse : LineSegment2d units coordinates -> LineSegment2d units coordinates
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( p2, p1 )


{-| Get the midpoint of a line segment.

    LineSegment2d.midpoint exampleLineSegment
    --> Point2d.meters 2 3

-}
midpoint : LineSegment2d units coordinates -> Point2d units coordinates
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| Interpolate a line segment between its start and end points; a value of 0.0
corresponds to the start point of the line segment, a value of 0.5 corresponds
to its midpoint and a value of 1.0 corresponds to its end point. Values less
than 0.0 or greater than 1.0 can be used to extrapolate.

    LineSegment2d.interpolate exampleLineSegment 0.25
    --> Point2d.meters 1.5 2.5

    LineSegment2d.interpolate exampleLineSegment 1.5
    --> Point2d.meters 4 5

If you just need to interpolate between two points, you don't have to construct
a line segment first - you can use [`Point2d.interpolateFrom`](Point2d#interpolateFrom)
directly.

-}
interpolate : LineSegment2d units coordinates -> Float -> Point2d units coordinates
interpolate lineSegment t =
    let
        ( start, end ) =
            endpoints lineSegment
    in
    Point2d.interpolateFrom start end t


{-| Get the length of a line segment.

    LineSegment2d.length exampleLineSegment
    --> Length.meters 2.8284

-}
length : LineSegment2d units coordinates -> Quantity Float units
length givenSegment =
    Vector2d.length (vector givenSegment)


{-| Get the direction from a line segment's start point to its end point. If the
line segment has zero length (the start and end points are the same), returns
`Nothing`.

    LineSegment2d.direction exampleLineSegment
    --> Just (Direction2d.degrees 45)

-}
direction : LineSegment2d units coordinates -> Maybe (Direction2d coordinates)
direction givenSegment =
    Vector2d.direction (vector givenSegment)


{-| Get the direction perpendicular to a line segment, pointing to the left. If
the line segment has zero length, returns `Nothing`.

    LineSegment2d.perpendicularDirection exampleLineSegment
    --> Just (Direction2d.degrees 135)

-}
perpendicularDirection : LineSegment2d units coordinates -> Maybe (Direction2d coordinates)
perpendicularDirection givenSegment =
    Vector2d.direction (Vector2d.perpendicularTo (vector givenSegment))


{-| Get the vector from a given line segment's start point to its end point.

    LineSegment2d.vector exampleLineSegment
    --> Vector2d.meters 2 2

-}
vector : LineSegment2d units coordinates -> Vector2d units coordinates
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    Vector2d.from p1 p2


{-| Attempt to find the unique intersection point of two line segments. If there
is no such point (the two line segments do not touch, or they overlap), returns
`Nothing`.

    -- 4 corners of a square

    a =
        Point2d.meters 0 0

    b =
        Point2d.meters 1 0

    c =
        Point2d.meters 1 1

    d =
        Point2d.meters 0 1

    -- definition of some segments with those points

    ab =
        LineSegment2d.from a b
    ...

    -- searching for intersections

    LineSegment2d.intersectionPoint ab bc
    --> Just (Point2d.meters 1 0)
    -- corner point b

    LineSegment2d.intersectionPoint ac bd
    --> Just (Point2d.meters 0.5 0.5)
    -- diagonal crossing at square center

    LineSegment2d.intersectionPoint ab cd
    --> Nothing -- parallel lines

    LineSegment2d.intersectionPoint ab ab
    --> Nothing -- collinear lines

Note that if the endpoint of one line segment lies on the other line segment,
numerical roundoff means that the intersection may or may not be found. If two
segments have a shared endpoint (the two segments meet in something like a 'V',
where the end point of one segment is the start point of the next), that point
is guaranteed to be returned as the intersection point, but if two segments meet
in a 'T' shape the intersection point may or may not be found.

-}
intersectionPoint : LineSegment2d units coordinates -> LineSegment2d units coordinates -> Maybe (Point2d units coordinates)
intersectionPoint lineSegment1 lineSegment2 =
    -- The two line segments are:
    -- p |--- r ---| p_
    -- q |--- s ---| q_
    let
        ( p, p_ ) =
            endpoints lineSegment1

        ( q, q_ ) =
            endpoints lineSegment2

        r =
            vector lineSegment1

        s =
            vector lineSegment2

        pq =
            Vector2d.from p q

        pq_ =
            Vector2d.from p q_

        qp_ =
            Vector2d.from q p_

        pqXr =
            pq |> Vector2d.cross r

        pqXs =
            pq |> Vector2d.cross s

        sXqp_ =
            s |> Vector2d.cross qp_

        rXpq_ =
            r |> Vector2d.cross pq_

        tDenominator =
            pqXs |> Quantity.minus sXqp_

        uDenominator =
            pqXr |> Quantity.plus rXpq_
    in
    if tDenominator == Quantity.zero || uDenominator == Quantity.zero then
        -- Segments are parallel or collinear.
        -- In collinear case, we check if there is only one intersection point.
        if r |> Vector2d.dot s |> Quantity.lessThan Quantity.zero then
            if p_ == q_ then
                -- p |----- p_ | q_ -----| q
                Just p_

            else if p == q then
                -- q_ |----- q | p -----| p_
                Just p

            else
                Nothing

        else if p_ == q then
            -- p |----- p_ | q -----| q_
            Just p_

        else if p == q_ then
            -- q |----- q_ | p -----| p_
            Just p

        else
            Nothing

    else
        -- Segments are not parallel.
        -- We search for the intersection point of the two lines.
        let
            t =
                Quantity.ratio pqXs tDenominator

            u =
                Quantity.ratio pqXr uDenominator
        in
        if (0 <= t && t <= 1) && (0 <= u && u <= 1) then
            -- Intersection is within both segments.
            let
                -- Ensure interpolation happens from the closest
                -- endpoint (this should be more numerically stable, and
                -- also mostly ensures that intersection is symmetric)
                intersection =
                    if min t (1 - t) <= min u (1 - u) then
                        interpolate lineSegment1 t

                    else
                        interpolate lineSegment2 u
            in
            Just intersection

        else
            Nothing


{-| Attempt to find the unique intersection point of a line segment with an
axis. If there is no such point (the line segment does not touch the axis, or
lies perfectly along it), returns `Nothing`.

    lineSegment =
        LineSegment2d.fromEndpoints
            ( Point2d.meters 1 -1
            , Point2d.meters 4 1
            )

    LineSegment2d.intersectionWithAxis Axis2d.x lineSegment
    --> Just (Point2d.meters 2.5 0)

    LineSegment2d.intersectionWithAxis Axis2d.y lineSegment
    --> Nothing

-}
intersectionWithAxis : Axis2d units coordinates -> LineSegment2d units coordinates -> Maybe (Point2d units coordinates)
intersectionWithAxis axis lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment

        d1 =
            Point2d.signedDistanceFrom axis p1

        d2 =
            Point2d.signedDistanceFrom axis p2

        product =
            d1 |> Quantity.times d2
    in
    if product |> Quantity.lessThan Quantity.zero then
        -- The two points are on opposite sides of the axis, so there is a
        -- unique intersection point in between them
        let
            t =
                Quantity.ratio d1 (d1 |> Quantity.minus d2)
        in
        Just (Point2d.interpolateFrom p1 p2 t)

    else if product |> Quantity.greaterThan Quantity.zero then
        -- Both points are on the same side of the axis, so no intersection
        -- point exists
        Nothing

    else if d1 /= Quantity.zero then
        -- d2 must be zero since the product is zero, so only p2 is on the axis
        Just p2

    else if d2 /= Quantity.zero then
        -- d1 must be zero since the product is zero, so only p1 is on the axis
        Just p1

    else if p1 == p2 then
        -- Both d1 and d2 are zero, so both p1 and p2 are on the axis but also
        -- happen to be equal to each other, so the line segment is actually
        -- just a single point on the axis
        Just p1

    else
        -- Both endpoints lie on the axis and are not equal to each other - no
        -- unique intersection point
        Nothing


{-| Measure the distance of a line segment along an axis. This is the range of distances
along the axis resulting from projecting the line segment perpendicularly onto the axis.

Note that reversing the line segment will _not_ affect the result.

-}
signedDistanceAlong : Axis2d units coordinates -> LineSegment2d units coordinates -> Interval Float units
signedDistanceAlong axis (Types.LineSegment2d ( p1, p2 )) =
    Interval.from
        (Point2d.signedDistanceAlong axis p1)
        (Point2d.signedDistanceAlong axis p2)


{-| Measure the distance of a line segment from an axis. If the returned interval:

  - is entirely positive, then the line segment is to the left of the axis
  - is entirely negative, then the line segment is to the right of the axis
  - contains zero, then the line segment crosses the axis

Note that reversing the line segment will _not_ affect the result.

-}
signedDistanceFrom : Axis2d units coordinates -> LineSegment2d units coordinates -> Interval Float units
signedDistanceFrom axis (Types.LineSegment2d ( p1, p2 )) =
    Interval.from
        (Point2d.signedDistanceFrom axis p1)
        (Point2d.signedDistanceFrom axis p2)


{-| Scale a line segment about the given center point by the given scale.
-}
scaleAbout : Point2d units coordinates -> Float -> LineSegment2d units coordinates -> LineSegment2d units coordinates
scaleAbout point scale =
    mapEndpoints (Point2d.scaleAbout point scale)


{-| Rotate a line segment counterclockwise around a given center point by a
given angle.
-}
rotateAround : Point2d units coordinates -> Angle -> LineSegment2d units coordinates -> LineSegment2d units coordinates
rotateAround centerPoint angle =
    mapEndpoints (Point2d.rotateAround centerPoint angle)


{-| Translate a line segment by a given displacement.
-}
translateBy : Vector2d units coordinates -> LineSegment2d units coordinates -> LineSegment2d units coordinates
translateBy displacementVector =
    mapEndpoints (Point2d.translateBy displacementVector)


{-| Translate a line segment in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> LineSegment2d units coordinates -> LineSegment2d units coordinates
translateIn translationDirection distance lineSegment =
    translateBy (Vector2d.withLength distance translationDirection) lineSegment


{-| Mirror a line segment across an axis. Note that the endpoints of a mirrored
segment are equal to the mirrored endpoints of the original segment, but as a
result the normal direction of a mirrored segment is the _opposite_ of the
mirrored normal direction of the original segment (since the normal direction is
always considered to be 'to the left' of the line segment).
-}
mirrorAcross : Axis2d units coordinates -> LineSegment2d units coordinates -> LineSegment2d units coordinates
mirrorAcross axis =
    mapEndpoints (Point2d.mirrorAcross axis)


{-| Project a line segment onto an axis.
-}
projectOnto : Axis2d units coordinates -> LineSegment2d units coordinates -> LineSegment2d units coordinates
projectOnto axis =
    mapEndpoints (Point2d.projectOnto axis)


{-| Transform the start and end points of a line segment by a given function
and create a new line segment from the resulting points. Most other
transformation functions can be defined in terms of `mapEndpoints`; for example,

    LineSegment2d.projectOnto axis

is equivalent to

    LineSegment2d.mapEndpoints (Point2d.projectOnto axis)

-}
mapEndpoints : (Point2d unitsA coordinatesA -> Point2d unitsB coordinatesB) -> LineSegment2d unitsA coordinatesA -> LineSegment2d unitsB coordinatesB
mapEndpoints function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( function p1, function p2 )


{-| Take a line segment defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> LineSegment2d units globalCoordinates -> LineSegment2d units localCoordinates
relativeTo frame =
    mapEndpoints (Point2d.relativeTo frame)


{-| Take a line segment considered to be defined in local coordinates relative
to a given reference frame, and return that line segment expressed in global
coordinates.
-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> LineSegment2d units localCoordinates -> LineSegment2d units globalCoordinates
placeIn frame =
    mapEndpoints (Point2d.placeIn frame)


{-| Get the minimal bounding box containing a given line segment.

    LineSegment2d.boundingBox exampleLineSegment
    --> BoundingBox2d.from
    -->     (Point2d.meters 1 2)
    -->     (Point2d.meters 3 4)

-}
boundingBox : LineSegment2d units coordinates -> BoundingBox2d units coordinates
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    BoundingBox2d.from p1 p2
