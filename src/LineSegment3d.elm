--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module LineSegment3d exposing
    ( LineSegment3d
    , fromEndpoints, from, along, on
    , startPoint, endPoint, endpoints, midpoint, length, direction, perpendicularDirection, vector, boundingBox
    , interpolate
    , intersectionWithPlane
    , reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints
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

@docs fromEndpoints, from, along, on


# Properties

@docs startPoint, endPoint, endpoints, midpoint, length, squaredLength, direction, perpendicularDirection, vector, boundingBox


# Interpolation

@docs interpolate


# Intersection

@docs intersectionWithPlane


# Transformations

Transforming a line segment is equivalent to transforming its start and end
points and forming a new line segment between the resulting points.

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints


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
import Quantity exposing (Quantity, Squared)
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

    LineSegment3d.along Axis3d.x 3 5
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 3 0 0
    -->     , Point3d.meters 5 0 0
    -->     )

    LineSegment3d.along Axis3d.y 2 -4
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 0 2 0
    -->     , Point3d.meters 0 -4 0
    -->     )

-}
along : Axis3d units coordinates -> Quantity Float units -> Quantity Float units -> LineSegment3d units coordinates
along axis startDistance endDistance =
    fromEndpoints
        ( Point3d.along axis startDistance
        , Point3d.along axis endDistance
        )


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


{-| Get the start point of a line segment.

    LineSegment3d.startPoint exampleLineSegment
    --> Point3d.meters 1 2 3

-}
startPoint : LineSegment3d units coordinates -> Point3d units coordinates
startPoint (Types.LineSegment3d ( start, _ )) =
    start


{-| Get the end point of a line segment.

    LineSegment3d.endPoint exampleLineSegment
    --> Point3d.meters 4 5 6

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

    LineSegment3d.reverse exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 4 5 6
    -->     , Point3d.meters 1 2 3
    -->     )

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


{-| TODO
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


{-| Get the length of a line segment.

    LineSegment3d.length exampleLineSegment
    --> 5.1962

-}
length : LineSegment3d units coordinates -> Quantity Float units
length lineSegment =
    Vector3d.length (vector lineSegment)


{-| Get the direction from a line segment's start point to its end point. If the
line segment has zero length (the start and end points are the same), returns
`Nothing`.

    LineSegment3d.direction exampleLineSegment
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (Angle.degrees 45)
    -->         (Angle.degrees 35.26)
    -->     )

-}
direction : LineSegment3d units coordinates -> Maybe (Direction3d coordinates)
direction lineSegment =
    Vector3d.direction (vector lineSegment)


{-| Get an arbitrary direction perpendicular to a line segment. If the line
segment has zero length, returns `Nothing`.

    LineSegment3d.perpendicularDirection exampleLineSegment
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (Angle.degrees -90)
    -->         (Angle.degrees 45)
    -->     )

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

    point =
        Point3d.meters 1 1 1

    LineSegment3d.scaleAbout point 2 exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 1 3 5
    -->     , Point3d.meters 7 9 11
    -->     )

-}
scaleAbout : Point3d units coordinates -> Float -> LineSegment3d units coordinates -> LineSegment3d units coordinates
scaleAbout point scale lineSegment =
    mapEndpoints (Point3d.scaleAbout point scale) lineSegment


{-| Rotate a line segment around a given axis by a given angle (in radians).

    exampleLineSegment
        |> LineSegment3d.rotateAround Axis3d.z (Angle.degrees 90)
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters -2 1 3
    -->     , Point3d.meters -5 4 6
    -->     )

-}
rotateAround : Axis3d units coordinates -> Angle -> LineSegment3d units coordinates -> LineSegment3d units coordinates
rotateAround axis angle lineSegment =
    mapEndpoints (Point3d.rotateAround axis angle) lineSegment


{-| Translate a line segment by a given displacement.

    displacement =
        Vector3d.meters 1 2 3

    exampleLineSegment
        |> LineSegment3d.translateBy displacement
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 2 4 6
    -->     , Point3d.meters 5 7 9
    -->     )

-}
translateBy : Vector3d units coordinates -> LineSegment3d units coordinates -> LineSegment3d units coordinates
translateBy displacementVector lineSegment =
    mapEndpoints (Point3d.translateBy displacementVector) lineSegment


{-| Translate a line segment in a given direction by a given distance;

    LineSegment3d.translateIn direction distance

is equivalent to

    LineSegment3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d coordinates -> Quantity Float units -> LineSegment3d units coordinates -> LineSegment3d units coordinates
translateIn translationDirection distance lineSegment =
    translateBy (Vector3d.withLength distance translationDirection) lineSegment


{-| Mirror a line segment across a plane.

    exampleLineSegment
        |> LineSegment3d.mirrorAcross Plane3d.xy
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 1 2 -3
    -->     , Point3d.meters 4 5 -6
    -->     )

-}
mirrorAcross : Plane3d units coordinates -> LineSegment3d units coordinates -> LineSegment3d units coordinates
mirrorAcross plane lineSegment =
    mapEndpoints (Point3d.mirrorAcross plane) lineSegment


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a line segment onto a plane.

    LineSegment3d.projectOnto Plane3d.yz exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 0 2 3
    -->     , Point3d.meters 0 5 6
    -->     )

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

    localFrame =
        Frame3d.atPoint
            (Point3d.meters 1 2 3)

    LineSegment3d.relativeTo localFrame exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 0 0 0
    -->     , Point3d.meters 3 3 3
    -->     )

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> LineSegment3d units globalCoordinates -> LineSegment3d units localCoordinates
relativeTo frame lineSegment =
    mapEndpoints (Point3d.relativeTo frame) lineSegment


{-| Take a line segment considered to be defined in local coordinates relative
to a given reference frame, and return that line segment expressed in global
coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.meters 1 2 3)

    LineSegment3d.placeIn localFrame exampleLineSegment
    --> LineSegment3d.fromEndpoints
    -->     ( Point3d.meters 2 4 6
    -->     , Point3d.meters 5 7 9
    -->     )

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> LineSegment3d units localCoordinates -> LineSegment3d units globalCoordinates
placeIn frame lineSegment =
    mapEndpoints (Point3d.placeIn frame) lineSegment


{-| Project a line segment into a given sketch plane. Conceptually, this finds
the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the line segment onto the plane and then expresses the projected line segment
in 2D sketch coordinates.

    exampleLineSegment
        |> LineSegment3d.projectInto SketchPlane3d.xy
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.meters 1 2
    -->     , Point2d.meters 4 5
    -->     )

    exampleLineSegment
        |> LineSegment3d.projectInto SketchPlane3d.yz
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.meters 2 3
    -->     , Point2d.meters 5 6
    -->     )

    exampleLineSegment
        |> LineSegment3d.projectInto SketchPlane3d.zx
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.meters 3 1
    -->     , Point2d.meters 6 4
    -->     )

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
    --> BoundingBox3d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     , minZ = 3
    -->     , maxZ = 6
    -->     }

-}
boundingBox : LineSegment3d units coordinates -> BoundingBox3d units coordinates
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    Point3d.hull2 p1 p2
