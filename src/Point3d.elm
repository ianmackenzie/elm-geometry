--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Point3d exposing
    ( Point3d
    , origin
    , fromCoordinates, fromCoordinatesIn, midpoint, centroid, interpolateFrom, along, on, circumcenter
    , fromTuple, toTuple, fromRecord, toRecord
    , coordinates, coordinatesIn, xCoordinate, yCoordinate, zCoordinate
    , equalWithin, lexicographicComparison
    , distanceFrom, squaredDistanceFrom, signedDistanceAlong, distanceFromAxis, squaredDistanceFromAxis, signedDistanceFrom
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectOntoAxis
    , relativeTo, placeIn, projectInto
    )

{-| A `Point3d` represents a position in 3D space and is defined by its X, Y and
Z coordinates. This module contains a variety of point-related functionality,
such as

  - Measuring distance between points, or the distance of a point from an axis
    or a plane
  - Scaling, rotating, translating, mirroring and projecting points
  - Converting points between different coordinate systems

Points are distinct from vectors but interact with them in well-defined ways;
you can translate a point by a vector to result in a new point, or you can
compute the vector from one point to another, but you cannot 'add' two points
like you can add two vectors.

@docs Point3d


# Constants

@docs origin


# Constructors

@docs fromCoordinates, fromCoordinatesIn, midpoint, centroid, interpolateFrom, along, on, circumcenter


# Conversion

@docs fromTuple, toTuple, fromRecord, toRecord


# Properties

@docs coordinates, coordinatesIn, xCoordinate, yCoordinate, zCoordinate


# Comparison

@docs equalWithin, lexicographicComparison


# Measurement

@docs distanceFrom, squaredDistanceFrom, signedDistanceAlong, distanceFromAxis, squaredDistanceFromAxis, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectOntoAxis


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Angle exposing (Angle)
import Bootstrap.Frame3d as Frame3d
import Bootstrap.Plane3d as Plane3d
import Bootstrap.SketchPlane3d as SketchPlane3d
import Direction3d exposing (Direction3d)
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis3d, Frame3d, Plane3d, SketchPlane3d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Squared, Unitless)
import Quantity.Extra as Quantity
import Vector3d exposing (Vector3d)


addTo : Point3d units coordinates -> Vector3d units coordinates -> Point3d units coordinates
addTo point vector =
    translateBy vector point


{-| -}
type alias Point3d units coordinates =
    Types.Point3d units coordinates


{-| The point (0, 0, 0).

    Point3d.origin
    --> Point3d.fromCoordinates ( 0, 0, 0 )

-}
origin : Point3d units coordinates
origin =
    fromCoordinates ( Quantity.zero, Quantity.zero, Quantity.zero )


{-| Construct a point from its X, Y and Z coordinates.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

-}
fromCoordinates : ( Quantity Float units, Quantity Float units, Quantity Float units ) -> Point3d units coordinates
fromCoordinates givenCoordinates =
    Types.Point3d givenCoordinates


{-| Construct a point halfway between two other points.

    p1 =
        Point3d.fromCoordinates ( 1, 1, 1 )

    p2 =
        Point3d.fromCoordinates ( 3, 7, 9 )

    Point3d.midpoint p1 p2
    --> Point3d.fromCoordinates ( 2, 4, 5 )

-}
midpoint : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5


{-| Find the centroid of a list of points. Returns `Nothing` if the list is
empty.

    p0 =
        Point3d.origin

    p1 =
        Point3d.fromCoordinates ( 1, 0, 1 )

    p2 =
        Point3d.fromCoordinates ( 0, 1, 1 )

    Point3d.centroid [ p0, p1, p2 ]
    --> Just (Point3d.fromCoordinates ( 0.3333, 0.3333, 0.6667 ))

-}
centroid : List (Point3d units coordinates) -> Maybe (Point3d units coordinates)
centroid points =
    case points of
        [] ->
            Nothing

        first :: rest ->
            let
                ( x0, y0, z0 ) =
                    coordinates first
            in
            Just (centroidHelp x0 y0 z0 1 Quantity.zero Quantity.zero Quantity.zero rest)


centroidHelp : Quantity Float units -> Quantity Float units -> Quantity Float units -> Float -> Quantity Float units -> Quantity Float units -> Quantity Float units -> List (Point3d units coordinates) -> Point3d units coordinates
centroidHelp x0 y0 z0 count dx dy dz points =
    case points of
        point :: remaining ->
            let
                ( x, y, z ) =
                    coordinates point

                newDx =
                    dx |> Quantity.plus (x |> Quantity.minus x0)

                newDy =
                    dy |> Quantity.plus (y |> Quantity.minus y0)

                newDz =
                    dz |> Quantity.plus (z |> Quantity.minus z0)
            in
            centroidHelp x0 y0 z0 (count + 1) newDx newDy newDz remaining

        [] ->
            let
                scale =
                    1 / count
            in
            fromCoordinates
                ( x0 |> Quantity.plus (Quantity.multiplyBy scale dx)
                , y0 |> Quantity.plus (Quantity.multiplyBy scale dy)
                , z0 |> Quantity.plus (Quantity.multiplyBy scale dz)
                )


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point3d.fromCoordinates ( 1, 2, 4 )

    endPoint =
        Point3d.fromCoordinates ( 1, 2, 8 )

    Point3d.interpolateFrom startPoint endPoint 0.25
    --> Point3d.fromCoordinates ( 1, 2, 5 )

Partial application may be useful:

    interpolatedPoint : Float -> Point3d
    interpolatedPoint =
        Point3d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point3d.fromCoordinates ( 1, 2, 4 )
    --> , Point3d.fromCoordinates ( 1, 2, 6 )
    --> , Point3d.fromCoordinates ( 1, 2, 8 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point3d.fromCoordinates ( 1, 2, 2 )

    interpolatedPoint 1.25
    --> Point3d.fromCoordinates ( 1, 2, 9 )

-}
interpolateFrom : Point3d units coordinates -> Point3d units coordinates -> Float -> Point3d units coordinates
interpolateFrom p1 p2 t =
    let
        ( x1, y1, z1 ) =
            coordinates p1

        ( x2, y2, z2 ) =
            coordinates p2
    in
    fromCoordinates
        ( Quantity.interpolateFrom x1 x2 t
        , Quantity.interpolateFrom y1 y2 t
        , Quantity.interpolateFrom z1 z2 t
        )


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point3d.along Axis3d.z 2
    --> Point3d.fromCoordinates ( 0, 0, 2 )

Positive and negative distances are interpreted relative to the direction of the
axis:

    horizontalAxis =
        Axis3d.withDirection Direction3d.negativeX
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    Point3d.along horizontalAxis 3
    --> Point3d.fromCoordinates ( -2, 1, 1 )

    Point3d.along horizontalAxis -3
    --> Point3d.fromCoordinates ( 4, 1, 1 )

-}
along : Axis3d units coordinates -> Quantity Float units -> Point3d units coordinates
along (Types.Axis3d axis) distance =
    axis.originPoint
        |> translateBy (Vector3d.withLength distance axis.direction)


{-| Construct a 3D point lying _on_ a sketch plane by providing a 2D point
specified in XY coordinates _within_ the sketch plane.

    Point3d.on SketchPlane3d.xy <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 2, 1, 0 )

    Point3d.on SketchPlane3d.xz <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 2, 0, 1 )

The sketch plane can have any position and orientation:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x
                (degrees 45)
            |> SketchPlane3d.moveTo
                (Point3d.fromCoordinates ( 10, 10, 10 ))

    Point3d.on tiltedSketchPlane <|
        Point2d.fromCoordinates ( 2, 1 )
    --> Point3d.fromCoordinates ( 12, 10.7071, 10.7071 )

-}
on : SketchPlane3d units coordinates3d coordinates2d -> Point2d units coordinates2d -> Point3d units coordinates3d
on sketchPlane point2d =
    let
        ( x0, y0, z0 ) =
            coordinates (SketchPlane3d.originPoint sketchPlane)

        ( ux, uy, uz ) =
            Direction3d.components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            Direction3d.components (SketchPlane3d.yDirection sketchPlane)

        ( x, y ) =
            Point2d.coordinates point2d
    in
    fromCoordinates
        ( x0 |> Quantity.plus (Quantity.aXbY ux x vx y)
        , y0 |> Quantity.plus (Quantity.aXbY uy x vy y)
        , z0 |> Quantity.plus (Quantity.aXbY uz x vz y)
        )


{-| Construct a point given its local coordinates within a particular frame:

    frame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    Point3d.fromCoordinatesIn frame ( 1, 2, 3 )
    --> Point3d.fromCoordinates ( 2, 3, 4 )

-}
fromCoordinatesIn : Frame3d units globalCoordinates localCoordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units ) -> Point3d units globalCoordinates
fromCoordinatesIn frame localCoordinates =
    let
        ( x, y, z ) =
            localCoordinates

        ( x0, y0, z0 ) =
            coordinates (Frame3d.originPoint frame)

        ( x1, y1, z1 ) =
            Direction3d.components (Frame3d.xDirection frame)

        ( x2, y2, z2 ) =
            Direction3d.components (Frame3d.yDirection frame)

        ( x3, y3, z3 ) =
            Direction3d.components (Frame3d.zDirection frame)
    in
    fromCoordinates
        ( x0 |> Quantity.plus (Quantity.aXbYcZ x1 x x2 y x3 z)
        , y0 |> Quantity.plus (Quantity.aXbYcZ y1 x y2 y y3 z)
        , z0 |> Quantity.plus (Quantity.aXbYcZ z1 x z2 y z3 z)
        )


{-| Attempt to find the circumcenter of three points; this is the center of the
circle that passes through all three points. If the three given points are
collinear, returns `Nothing`.

    Point3d.circumcenter
        (Point3d.fromCoordinates ( 1, 0, 0 ))
        (Point3d.fromCoordinates ( 0, 1, 0 ))
        (Point3d.fromCoordinates ( 0, 0, 1 ))
    --> Just (Point3d.fromCoordinates (0.33, 0.33, 0.33))

    Point3d.circumcenter
        Point3d.origin
        (Point3d.fromCoordinates ( 1, 0, 0 ))
        (Point3d.fromCoordinates ( 2, 0, 0 ))
    --> Nothing

-}
circumcenter : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Maybe (Point3d units coordinates)
circumcenter p1 p2 p3 =
    let
        a2 =
            squaredDistanceFrom p1 p2

        b2 =
            squaredDistanceFrom p2 p3

        c2 =
            squaredDistanceFrom p3 p1

        t1 =
            a2 |> Quantity.times (b2 |> Quantity.plus c2 |> Quantity.minus a2)

        t2 =
            b2 |> Quantity.times (c2 |> Quantity.plus a2 |> Quantity.minus b2)

        t3 =
            c2 |> Quantity.times (a2 |> Quantity.plus b2 |> Quantity.minus c2)

        sum =
            t1 |> Quantity.plus t2 |> Quantity.plus t3
    in
    if sum == Quantity.zero then
        Nothing

    else
        let
            w1 =
                Quantity.ratio t1 sum

            w2 =
                Quantity.ratio t2 sum

            w3 =
                Quantity.ratio t3 sum

            ( x1, y1, z1 ) =
                coordinates p1

            ( x2, y2, z2 ) =
                coordinates p2

            ( x3, y3, z3 ) =
                coordinates p3
        in
        Just <|
            fromCoordinates
                ( Quantity.aXbYcZ w1 x3 w2 x1 w3 x2
                , Quantity.aXbYcZ w1 y3 w2 y1 w3 y2
                , Quantity.aXbYcZ w1 z3 w2 z1 w3 z2
                )


fromTuple : ( Float, Float, Float ) -> Point3d Unitless coordinates
fromTuple ( x, y, z ) =
    fromCoordinates
        ( Quantity.float x
        , Quantity.float y
        , Quantity.float z
        )


toTuple : Point3d Unitless coordinates -> ( Float, Float, Float )
toTuple point =
    ( Quantity.toFloat (xCoordinate point)
    , Quantity.toFloat (yCoordinate point)
    , Quantity.toFloat (zCoordinate point)
    )


fromRecord : { x : Float, y : Float, z : Float } -> Point3d Unitless coordinates
fromRecord { x, y, z } =
    fromCoordinates
        ( Quantity.float x
        , Quantity.float y
        , Quantity.float z
        )


toRecord : Point3d Unitless coordinates -> { x : Float, y : Float, z : Float }
toRecord point =
    { x = Quantity.toFloat (xCoordinate point)
    , y = Quantity.toFloat (yCoordinate point)
    , z = Quantity.toFloat (zCoordinate point)
    }


{-| Get the coordinates of a point as a tuple.

    ( x, y, z ) =
        Point3d.coordinates point

-}
coordinates : Point3d units coordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units )
coordinates (Types.Point3d pointCoordinates) =
    pointCoordinates


{-| Get the coordinates of a point within a given frame.

    point =
        Point3d.fromCoordinates
            ( Length.meters 2
            , Length.meters 3
            , Length.meters 4
            )

    frame =
        Frame3d.atCoordinates
            ( Length.meters 1
            , Length.meters 1
            , Length.meters 1
            )

    point |> Point3d.coordinatesIn frame
    --> ( Length.meters 1
    --> , Length.meters 2
    --> , Length.meters 3
    --> )

-}
coordinatesIn : Frame3d units globalCoordinates localCoordinates -> Point3d units globalCoordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units )
coordinatesIn frame point =
    let
        ( x, y, z ) =
            coordinates point

        ( x0, y0, z0 ) =
            coordinates (Frame3d.originPoint frame)

        dx =
            x |> Quantity.minus x0

        dy =
            y |> Quantity.minus y0

        dz =
            z |> Quantity.minus z0

        ( x1, y1, z1 ) =
            Direction3d.components (Frame3d.xDirection frame)

        ( x2, y2, z2 ) =
            Direction3d.components (Frame3d.yDirection frame)

        ( x3, y3, z3 ) =
            Direction3d.components (Frame3d.zDirection frame)
    in
    ( Quantity.aXbYcZ x1 dx y1 dy z1 dz
    , Quantity.aXbYcZ x2 dx y2 dy z2 dz
    , Quantity.aXbYcZ x3 dx y3 dy z3 dz
    )


{-| Get the X coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.xCoordinate
    --> 2

-}
xCoordinate : Point3d units coordinates -> Quantity Float units
xCoordinate (Types.Point3d ( x, _, _ )) =
    x


{-| Get the Y coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.yCoordinate
    --> 1

-}
yCoordinate : Point3d units coordinates -> Quantity Float units
yCoordinate (Types.Point3d ( _, y, _ )) =
    y


{-| Get the Z coordinate of a point.

    Point3d.fromCoordinates ( 2, 1, 3 )
        |> Point3d.zCoordinate
    --> 3

-}
zCoordinate : Point3d units coordinates -> Quantity Float units
zCoordinate (Types.Point3d ( _, _, z )) =
    z


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point3d.fromCoordinates ( 2, 1, 3 )

    secondPoint =
        Point3d.fromCoordinates ( 2.0002, 0.9999, 3.0001 )

    Point3d.equalWithin 1e-3 firstPoint secondPoint
    --> True

    Point3d.equalWithin 1e-6 firstPoint secondPoint
    --> False

-}
equalWithin : Quantity Float units -> Point3d units coordinates -> Point3d units coordinates -> Bool
equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint
        |> Quantity.lessThanOrEqualTo (Quantity.squared tolerance)


{-| Compare two `Point3d` values lexicographically: first by X coordinate, then
by Y, then by Z. Can be used to provide a sort order for `Point3d` values.
-}
lexicographicComparison : Point3d units coordinates -> Point3d units coordinates -> Order
lexicographicComparison firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            coordinates firstPoint

        ( x2, y2, z2 ) =
            coordinates secondPoint
    in
    if x1 /= x2 then
        Quantity.compare x1 x2

    else if y1 /= y2 then
        Quantity.compare y1 y2

    else
        Quantity.compare z1 z2


{-| Find the distance from the first point to the second.

    p1 =
        Point3d.fromCoordinates ( 1, 1, 2 )

    p2 =
        Point3d.fromCoordinates ( 2, 3, 4 )

    Point3d.distanceFrom p1 p2
    --> 3

Partial application can be useful:

    points =
        [ Point3d.fromCoordinates ( 3, 4, 5 )
        , Point3d.fromCoordinates ( 10, 10, 10 )
        , Point3d.fromCoordinates ( -1, 2, -3 )
        ]

    points
        |> List.sortBy
            (Point3d.distanceFrom Point3d.origin)
    --> [ Point3d.fromCoordinates ( -1, 2, -3 )
    --> , Point3d.fromCoordinates ( 3, 4, 5 )
    --> , Point3d.fromCoordinates ( 10, 10, 10 )
    --> ]

-}
distanceFrom : Point3d units coordinates -> Point3d units coordinates -> Quantity Float units
distanceFrom firstPoint secondPoint =
    Quantity.sqrt (squaredDistanceFrom firstPoint secondPoint)


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point3d.squaredDistanceFrom p1 p2
        > (tolerance * tolerance)

is equivalent to but slightly more efficient than

    Point3d.distanceFrom p1 p2 > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `distanceFrom` is much more
readable!

-}
squaredDistanceFrom : Point3d units coordinates -> Point3d units coordinates -> Quantity Float (Squared units)
squaredDistanceFrom firstPoint secondPoint =
    Vector3d.squaredLength (Vector3d.from firstPoint secondPoint)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis3d.withDirection Direction3d.x
            (Point3d.fromCoordinates ( 1, 0, 0 ))

    point =
        Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.signedDistanceAlong axis point
    --> 2

    Point3d.signedDistanceAlong axis Point3d.origin
    --> -1

-}
signedDistanceAlong : Axis3d units coordinates -> Point3d units coordinates -> Quantity Float units
signedDistanceAlong (Types.Axis3d axis) point =
    Vector3d.from axis.originPoint point |> Vector3d.componentIn axis.direction


{-| Find the perpendicular (nearest) distance of a point from an axis.

    point =
        Point3d.fromCoordinates ( -3, 4, 0 )

    Point3d.distanceFromAxis Axis3d.x point
    --> 4

    Point3d.distanceFromAxis Axis3d.y point
    --> 3

    Point3d.distanceFromAxis Axis3d.z point
    --> 5

Note that unlike in 2D, the result is always positive (unsigned) since there is
no such thing as the left or right side of an axis in 3D.

-}
distanceFromAxis : Axis3d units coordinates -> Point3d units coordinates -> Quantity Float units
distanceFromAxis axis point =
    Quantity.sqrt (squaredDistanceFromAxis axis point)


{-| Find the square of the perpendicular distance of a point from an axis. As
with `distanceFrom`/`squaredDistanceFrom` this is slightly more efficient than
`distanceFromAxis` since it avoids a square root.
-}
squaredDistanceFromAxis : Axis3d units coordinates -> Point3d units coordinates -> Quantity Float (Squared units)
squaredDistanceFromAxis (Types.Axis3d axis) point =
    let
        displacement =
            Vector3d.from axis.originPoint point

        perpendicularDisplacement =
            displacement
                |> Vector3d.minus
                    (displacement |> Vector3d.projectionIn axis.direction)
    in
    Vector3d.squaredLength perpendicularDisplacement


{-| Find the perpendicular distance of a point from a plane. The result will be
positive if the point is 'above' the plane and negative if it is 'below', with
'up' defined by the normal direction of the plane.

    plane =
        Plane3d.withNormalDirection Direction3d.y
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    point =
        Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.signedDistanceFrom plane point
    --> 1

    Point3d.signedDistanceFrom plane Point3d.origin
    --> -2

This means that flipping a plane (reversing its normal direction) will also flip
the sign of the result of this function:

    flippedPlane =
        Plane3d.reverseNormal plane

    Point3d.signedDistanceFrom flippedPlane point
    --> -1

    Point3d.signedDistanceFrom flippedPlane Point3d.origin
    --> 2

-}
signedDistanceFrom : Plane3d units coordinates -> Point3d units coordinates -> Quantity Float units
signedDistanceFrom plane point =
    let
        ( x, y, z ) =
            coordinates point

        ( x0, y0, z0 ) =
            coordinates (Plane3d.originPoint plane)

        ( nx, ny, nz ) =
            Direction3d.components (Plane3d.normalDirection plane)

        dx =
            x |> Quantity.minus x0

        dy =
            y |> Quantity.minus y0

        dz =
            z |> Quantity.minus z0
    in
    Quantity.aXbYcZ nx dx ny dy nz dz


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point3d.fromCoordinates ( 1, 1, 1 )

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.scaleAbout centerPoint 3 point
    --> Point3d.fromCoordinates ( 1, 4, 7 )

    Point3d.scaleAbout centerPoint 0.5 point
    --> Point3d.fromCoordinates ( 1, 1.5, 2 )

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point3d units coordinates -> Float -> Point3d units coordinates -> Point3d units coordinates
scaleAbout centerPoint scale point =
    Vector3d.from centerPoint point
        |> Vector3d.scaleBy scale
        |> addTo centerPoint


{-| Rotate a point around an axis by a given angle (in radians).

    axis =
        Axis3d.x

    angle =
        degrees 45

    point =
        Point3d.fromCoordinates ( 3, 1, 0 )

    Point3d.rotateAround axis angle point
    --> Point3d.fromCoordinates ( 3, 0.7071, 0.7071 )

Rotation direction is given by the right-hand rule, counterclockwise around the
direction of the axis.

-}
rotateAround : Axis3d units coordinates -> Angle -> Point3d units coordinates -> Point3d units coordinates
rotateAround ((Types.Axis3d axis) as axis_) angle point =
    Vector3d.from axis.originPoint point
        |> Vector3d.rotateAround axis_ angle
        |> addTo axis.originPoint


{-| Translate a point by a given displacement.

    point =
        Point3d.fromCoordinates ( 3, 4, 5 )

    displacement =
        Vector3d.fromComponents ( 1, 2, 3 )

    Point3d.translateBy displacement point
    --> Point3d.fromCoordinates ( 4, 6, 8 )

In more mathematical terms, this is 'point plus vector'. For 'point minus point'
(giving the vector from one point to another), there is [`Vector3d.from`](Vector3d#from).

-}
translateBy : Vector3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
translateBy vector point =
    let
        ( vx, vy, vz ) =
            Vector3d.components vector

        ( px, py, pz ) =
            coordinates point
    in
    fromCoordinates
        ( px |> Quantity.plus vx
        , py |> Quantity.plus vy
        , pz |> Quantity.plus vz
        )


{-| Translate a point in a given direction by a given distance.

    point =
        Point3d.fromCoordinates ( 3, 4, 5 )

    point |> Point3d.translateIn Direction3d.x 2
    --> Point3d.fromCoordinates ( 5, 4, 5 )

    point |> Point3d.translateIn Direction3d.y 2
    --> Point3d.fromCoordinates ( 3, 6, 5 )

The distance can be negative:

    Point3d.translateIn Direction3d.x -2
    --> Point3d.fromCoordinates ( 1, 4, 5 )

-}
translateIn : Direction3d coordinates -> Quantity Float units -> Point3d units coordinates -> Point3d units coordinates
translateIn direction distance point =
    let
        ( dx, dy, dz ) =
            Direction3d.components direction

        ( px, py, pz ) =
            coordinates point
    in
    fromCoordinates
        ( px |> Quantity.plus (Quantity.multiplyBy dx distance)
        , py |> Quantity.plus (Quantity.multiplyBy dy distance)
        , pz |> Quantity.plus (Quantity.multiplyBy dz distance)
        )


{-| Mirror a point across a plane. The result will be the same distance from the
plane but on the opposite side.

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    -- Plane3d.xy is the plane Z=0
    Point3d.mirrorAcross Plane3d.xy point
    --> Point3d.fromCoordinates ( 1, 2, -3 )

    -- Plane3d.yz is the plane X=0
    Point3d.mirrorAcross Plane3d.yz point
    --> Point3d.fromCoordinates ( -1, 2, 3 )

The plane does not have to pass through the origin:

    -- offsetPlane is the plane Z=1
    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    -- The origin point is 1 unit below the offset
    -- plane, so its mirrored copy is one unit above
    Point3d.mirrorAcross offsetPlane Point3d.origin
    --> Point3d.fromCoordinates ( 0, 0, 2 )

-}
mirrorAcross : Plane3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
mirrorAcross plane point =
    let
        originPoint =
            Plane3d.originPoint plane
    in
    Vector3d.from originPoint point
        |> Vector3d.mirrorAcross plane
        |> addTo originPoint


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a point onto a plane:

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.projectOnto Plane3d.xy point
    --> Point3d.fromCoordinates ( 1, 2, 0 )

    Point3d.projectOnto Plane3d.yz point
    --> Point3d.fromCoordinates ( 0, 2, 3 )

The plane does not have to pass through the origin:

    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    Point3d.projectOnto offsetPlane point
    --> Point3d.fromCoordinates ( 1, 2, 1 )

-}
projectOnto : Plane3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
projectOnto plane point =
    let
        displacement =
            Vector3d.withLength
                (Quantity.negate (signedDistanceFrom plane point))
                (Plane3d.normalDirection plane)
    in
    translateBy displacement point


{-| Project a point perpendicularly onto an axis.

    point =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Point3d.projectOntoAxis Axis3d.x
    --> Point3d.fromCoordinates ( 1, 0, 0 )

    verticalAxis =
        Axis3d.withDirection Direction3d.z
            (Point3d.fromCoordinates ( 0, 1, 2 ))

    Point3d.projectOntoAxis verticalAxis
    --> Point3d.fromCoordinates ( 0, 1, 3 )

-}
projectOntoAxis : Axis3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
projectOntoAxis axis point =
    along axis (signedDistanceAlong axis point)


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Point3d.relativeTo localFrame
        (Point3d.fromCoordinates ( 4, 5, 6 ))
    --> Point3d.fromCoordinates ( 3, 3, 3 )

    Point3d.relativeTo localFrame
        (Point3d.fromCoordinates ( 1, 1, 1 ))
    --> Point3d.fromCoordinates ( 0, -1, -2 )

-}
relativeTo : Frame3d units globalCoordinates localCoordinates -> Point3d units globalCoordinates -> Point3d units localCoordinates
relativeTo frame point =
    Vector3d.from (Frame3d.originPoint frame) point
        |> Vector3d.relativeTo frame
        |> Vector3d.components
        |> fromCoordinates


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Point3d.placeIn localFrame
        (Point3d.fromCoordinates ( 3, 3, 3 ))
    --> Point3d.fromCoordinates ( 4, 5, 6 )

    Point3d.placeIn localFrame
        (Point3d.fromCoordinates ( 0, -1, -2 ))
    --> Point3d.fromCoordinates ( 1, 1, 1 )

-}
placeIn : Frame3d units globalCoordinates localCoordinates -> Point3d units localCoordinates -> Point3d units globalCoordinates
placeIn frame point =
    Vector3d.fromComponents (coordinates point)
        |> Vector3d.placeIn frame
        |> addTo (Frame3d.originPoint frame)


{-| Project a point into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the point onto the plane and then expresses the projected point in 2D sketch
coordinates.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

    Point3d.projectInto SketchPlane3d.xy point
    --> Point2d.fromCoordinates ( 2, 1 )

    Point3d.projectInto SketchPlane3d.yz point
    --> Point2d.fromCoordinates ( 1, 3 )

    Point3d.projectInto SketchPlane3d.zx point
    --> Point2d.fromCoordinates ( 3, 2 )

-}
projectInto : SketchPlane3d units coordinates coordinates2d -> Point3d units coordinates -> Point2d units coordinates2d
projectInto sketchPlane point =
    let
        ( x, y, z ) =
            coordinates point

        ( x0, y0, z0 ) =
            coordinates (SketchPlane3d.originPoint sketchPlane)

        ( ux, uy, uz ) =
            Direction3d.components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            Direction3d.components (SketchPlane3d.yDirection sketchPlane)

        dx =
            x |> Quantity.minus x0

        dy =
            y |> Quantity.minus y0

        dz =
            z |> Quantity.minus z0
    in
    Point2d.fromCoordinates
        ( Quantity.aXbYcZ ux dx uy dy uz dz
        , Quantity.aXbYcZ vx dx vy dy vz dz
        )
