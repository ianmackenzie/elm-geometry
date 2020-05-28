--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module LineSegment3d exposing
    ( LineSegment3d
    , fromEndpoints, from, fromPointAndVector, along, on
    , startPoint, endPoint, endpoints, midpoint, length, direction, perpendicularDirection, vector, boundingBox
    , interpolate
    , intersectionWithPlane
    , signedDistanceAlong, signedDistanceFrom
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints
    , at, at_
    , relativeTo, placeIn, projectInto
    )

{-| A `LineSegment3d` is a line between two points in 3D. This module contains
functionality such as:

  - Interpolating within a line segment or measuring its length
  - Scaling, rotating, translating, mirroring or projecting a line segment
  - Converting a line segment between local and global coordinates in different
    reference frames

@docs LineSegment3d


# Constructors

@docs fromEndpoints, from, fromPointAndVector, along, on


# Properties

@docs startPoint, endPoint, endpoints, midpoint, length, direction, perpendicularDirection, vector, boundingBox


# Interpolation

@docs interpolate


# Intersection

@docs intersectionWithPlane


# Measurement

@docs signedDistanceAlong, signedDistanceFrom


# Transformations

These transformations generally behave just like [the ones in the `Point3d`
module](Point3d#transformations).

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Rate, Squared)
import Quantity.Interval as Interval exposing (Interval)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias LineSegment3d units coordinates =
    Types.LineSegment3d units coordinates


{-| Construct a line segment from its two endpoints:

    exampleLineSegment =
        LineSegment3d.fromEndpoints
            ( Point3d.meters 1 2 3
            , Point3d.meters 4 5 6
            )

-}
fromEndpoints : ( Point3d units coordinates, Point3d units coordinates ) -> LineSegment3d units coordinates
fromEndpoints givenEndpoints =
    Types.LineSegment3d givenEndpoints


{-| Construct a line segment from the first point to the second;

    LineSegment3d.from firstPoint secondPoint

is equivalent to

    LineSegment3d.fromEndpoints ( firstPoint, secondPoint )

-}
from : Point3d units coordinates -> Point3d units coordinates -> LineSegment3d units coordinates
from givenStartPoint givenEndPoint =
    fromEndpoints ( givenStartPoint, givenEndPoint )


{-| Construct a line segment lying on the given axis, with its endpoints at the
given distances from the axis' origin point.

    LineSegment3d.along Axis3d.x
        (Length.meters 3)
        (Length.meters 5)
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 3 0 0
    -->     , Point3d.meters 5 0 0
    -->     )

-}
along : Axis3d units coordinates -> Quantity Float units -> Quantity Float units -> LineSegment3d units coordinates
along axis startDistance endDistance =
    fromEndpoints
        ( Point3d.along axis startDistance
        , Point3d.along axis endDistance
        )


{-| Construct a line segment given its start point and the vector from its
start point to its end point;

    LineSegment3d.fromPointAndVector point vector

is equivalent to

    LineSegment3d.fromEndpoints
        ( point
        , point |> Point3d.translateBy vector
        )

-}
fromPointAndVector : Point3d units coordinates -> Vector3d units coordinates -> LineSegment3d units coordinates
fromPointAndVector givenPoint givenVector =
    fromEndpoints ( givenPoint, givenPoint |> Point3d.translateBy givenVector )


{-| Construct a 3D line segment lying _on_ a sketch plane by providing a 2D line
segment specified in XY coordinates _within_ the sketch plane.

    LineSegment3d.on SketchPlane3d.yz <|
        LineSegment2d.fromEndpoints
            ( Point2d.meters 1 2
            , Point2d.meters 3 4
            )
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 0 1 2
    -->     , Point3d.meters 0 3 4
    -->     )

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> LineSegment2d units coordinates2d -> LineSegment3d units coordinates3d
on sketchPlane lineSegment2d =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment2d
    in
    fromEndpoints
        ( Point3d.on sketchPlane p1
        , Point3d.on sketchPlane p2
        )


{-| Convert a line segment from one units type to another, by providing a
conversion factor given as a rate of change of destination units with respect to
source units.
-}
at : Quantity Float (Rate units2 units1) -> LineSegment3d units1 coordinates -> LineSegment3d units2 coordinates
at rate (Types.LineSegment3d ( p1, p2 )) =
    Types.LineSegment3d ( Point3d.at rate p1, Point3d.at rate p2 )


{-| Convert a line segment from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> LineSegment3d units1 coordinates -> LineSegment3d units2 coordinates
at_ rate lineSegment =
    at (Quantity.inverse rate) lineSegment


{-| Get the start point of a line segment.
-}
startPoint : LineSegment3d units coordinates -> Point3d units coordinates
startPoint (Types.LineSegment3d ( start, _ )) =
    start


{-| Get the end point of a line segment.
-}
endPoint : LineSegment3d units coordinates -> Point3d units coordinates
endPoint (Types.LineSegment3d ( _, end )) =
    end


{-| Get the endpoints of a line segment as a tuple.

    ( p1, p2 ) =
        LineSegment3d.endpoints lineSegment

-}
endpoints : LineSegment3d units coordinates -> ( Point3d units coordinates, Point3d units coordinates )
endpoints (Types.LineSegment3d lineSegmentEndpoints) =
    lineSegmentEndpoints


{-| Reverse a line segment, swapping its start and end points.
-}
reverse : LineSegment3d units coordinates -> LineSegment3d units coordinates
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( p2, p1 )


{-| Get the midpoint of a line segment.

    LineSegment3d.midpoint exampleLineSegment
    --> Point3d.meters 2.5 3.5 4.5

-}
midpoint : LineSegment3d units coordinates -> Point3d units coordinates
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| Interpolate a line segment between its start and end points; a value of 0.0
corresponds to the start point of the line segment, a value of 0.5 corresponds
to its midpoint and a value of 1.0 corresponds to its end point. Values less
than 0.0 or greater than 1.0 can be used to extrapolate.

    LineSegment3d.interpolate exampleLineSegment (1 / 3)
    --> Point3d.meters 2 4 5

    LineSegment3d.interpolate exampleLineSegment (-1 / 3)
    --> Point3d.meters 0 1 2

If you just need to interpolate between two points, you don't have to construct
a line segment first - you can use [`Point3d.interpolateFrom`](Point3d#interpolateFrom)
directly.

-}
interpolate : LineSegment3d units coordinates -> Float -> Point3d units coordinates
interpolate lineSegment t =
    let
        ( start, end ) =
            endpoints lineSegment
    in
    Point3d.interpolateFrom start end t


{-| Try to find the unique intersection point of a line segment with a plane. If
the line segment does not intersect the plane, or if it is coplanar with it
(lying perfectly in the plane), returns `Nothing`.
-}
intersectionWithPlane : Plane3d units coordinates -> LineSegment3d units coordinates -> Maybe (Point3d units coordinates)
intersectionWithPlane plane lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment

        d1 =
            Point3d.signedDistanceFrom plane p1

        d2 =
            Point3d.signedDistanceFrom plane p2

        product =
            d1 |> Quantity.times d2
    in
    if product |> Quantity.lessThan Quantity.zero then
        -- The two points are on opposite sides of the plane, so there is a
        -- unique intersection point in between them
        let
            t =
                Quantity.ratio d1 (d1 |> Quantity.minus d2)
        in
        Just (Point3d.interpolateFrom p1 p2 t)

    else if product |> Quantity.greaterThan Quantity.zero then
        -- Both points are on the same side of the plane, so no intersection
        -- point exists
        Nothing

    else if d1 /= Quantity.zero then
        -- d2 must be zero since the product is zero, so only p2 is on the plane
        Just p2

    else if d2 /= Quantity.zero then
        -- d1 must be zero since the product is zero, so only p1 is on the plane
        Just p1

    else if p1 == p2 then
        -- Both d1 and d2 are zero, so both p1 and p2 are on the plane but also
        -- happen to be equal to each other, so the line segment is actually
        -- just a single point on the plane
        Just p1

    else
        -- Both endpoints lie on the plane and are not equal to each other - no
        -- unique intersection point
        Nothing


{-| Measure the distance of a line segment along an axis. This is the range of distances
along the axis resulting from projecting the line segment perpendicularly onto the axis.

Note that reversing the line segment will _not_ affect the result.

-}
signedDistanceAlong : Axis3d units coordinates -> LineSegment3d units coordinates -> Interval Float units
signedDistanceAlong axis (Types.LineSegment3d ( p1, p2 )) =
    Interval.from
        (Point3d.signedDistanceAlong axis p1)
        (Point3d.signedDistanceAlong axis p2)


{-| Measure the distance of a line segment from a plane. If the returned interval:

  - is entirely positive, then the line segment is above the plane
  - is entirely negative, then the line segment is below the plane
  - contains zero, then the line segment crosses the plane

Note that reversing the line segment will _not_ affect the result.

-}
signedDistanceFrom : Plane3d units coordinates -> LineSegment3d units coordinates -> Interval Float units
signedDistanceFrom plane (Types.LineSegment3d ( p1, p2 )) =
    Interval.from
        (Point3d.signedDistanceFrom plane p1)
        (Point3d.signedDistanceFrom plane p2)


{-| Get the length of a line segment.

    LineSegment3d.length exampleLineSegment
    --> Length.meters 5.1962

-}
length : LineSegment3d units coordinates -> Quantity Float units
length lineSegment =
    Vector3d.length (vector lineSegment)


{-| Get the direction from a line segment's start point to its end point. If the
line segment has zero length (the start and end points are the same), returns
`Nothing`.

    LineSegment3d.direction exampleLineSegment
    --> Just <|
    -->     Direction3d.xyZ (Angle.degrees 45)
    -->         (Angle.degrees 35.26)

-}
direction : LineSegment3d units coordinates -> Maybe (Direction3d coordinates)
direction lineSegment =
    Vector3d.direction (vector lineSegment)


{-| Get an arbitrary direction perpendicular to a line segment. If the line
segment has zero length, returns `Nothing`.

    LineSegment3d.perpendicularDirection exampleLineSegment
    --> Just (Direction3d.yz (Angle.degrees 135))

-}
perpendicularDirection : LineSegment3d units coordinates -> Maybe (Direction3d coordinates)
perpendicularDirection lineSegment =
    Vector3d.direction (Vector3d.perpendicularTo (vector lineSegment))


{-| Get the vector from a line segment's start point to its end point.

    LineSegment3d.vector exampleLineSegment
    --> Vector3d.meters 2 2 2

-}
vector : LineSegment3d units coordinates -> Vector3d units coordinates
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    Vector3d.from p1 p2


{-| Scale a line segment about the given center point by the given scale.
-}
scaleAbout : Point3d units coordinates -> Float -> LineSegment3d units coordinates -> LineSegment3d units coordinates
scaleAbout point scale lineSegment =
    mapEndpoints (Point3d.scaleAbout point scale) lineSegment


{-| Rotate a line segment around a given axis by a given angle.
-}
rotateAround : Axis3d units coordinates -> Angle -> LineSegment3d units coordinates -> LineSegment3d units coordinates
rotateAround axis angle lineSegment =
    mapEndpoints (Point3d.rotateAround axis angle) lineSegment


{-| Translate a line segment by a given displacement.
-}
translateBy : Vector3d units coordinates -> LineSegment3d units coordinates -> LineSegment3d units coordinates
translateBy displacementVector lineSegment =
    mapEndpoints (Point3d.translateBy displacementVector) lineSegment


{-| Translate a line segment in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> LineSegment3d units coordinates -> LineSegment3d units coordinates
translateIn translationDirection distance lineSegment =
    translateBy (Vector3d.withLength distance translationDirection) lineSegment


{-| Mirror a line segment across a plane.
-}
mirrorAcross : Plane3d units coordinates -> LineSegment3d units coordinates -> LineSegment3d units coordinates
mirrorAcross plane lineSegment =
    mapEndpoints (Point3d.mirrorAcross plane) lineSegment


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a line segment onto a plane.
-}
projectOnto : Plane3d units coordinates -> LineSegment3d units coordinates -> LineSegment3d units coordinates
projectOnto plane lineSegment =
    mapEndpoints (Point3d.projectOnto plane) lineSegment


{-| Transform the start and end points of a line segment by a given function
and create a new line segment from the resulting points. Most other
transformation functions can be defined in terms of `mapEndpoints`; for example,

    LineSegment3d.projectOnto plane

is equivalent to

    LineSegment3d.mapEndpoints (Point3d.projectOnto plane)

-}
mapEndpoints : (Point3d units1 coordinates1 -> Point3d units2 coordinates2) -> LineSegment3d units1 coordinates1 -> LineSegment3d units2 coordinates2
mapEndpoints function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( function p1, function p2 )


{-| Take a line segment defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> LineSegment3d units globalCoordinates -> LineSegment3d units localCoordinates
relativeTo frame lineSegment =
    mapEndpoints (Point3d.relativeTo frame) lineSegment


{-| Take a line segment considered to be defined in local coordinates relative
to a given reference frame, and return that line segment expressed in global
coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> LineSegment3d units localCoordinates -> LineSegment3d units globalCoordinates
placeIn frame lineSegment =
    mapEndpoints (Point3d.placeIn frame) lineSegment


{-| Project a line segment into a given sketch plane.
-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> LineSegment3d units coordinates3d -> LineSegment2d units coordinates2d
projectInto sketchPlane lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment

        project =
            Point3d.projectInto sketchPlane
    in
    LineSegment2d.fromEndpoints ( project p1, project p2 )


{-| Get the minimal bounding box containing a line segment.

    LineSegment3d.boundingBox exampleLineSegment
    --> BoundingBox3d.from
    -->     (Point3d.meters 1 2 3)
    -->     (Point3d.meters 4 5 6)

-}
boundingBox : LineSegment3d units coordinates -> BoundingBox3d units coordinates
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    BoundingBox3d.from p1 p2
