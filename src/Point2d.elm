--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Point2d exposing
    ( Point2d
    , origin
    , fromCoordinates, fromCoordinatesIn, fromPolarCoordinates, fromPolarCoordinatesIn, midpoint, centroid, interpolateFrom, along, circumcenter
    , fromTuple, toTuple, fromRecord, toRecord
    , coordinates, xCoordinate, yCoordinate, polarCoordinates
    , equalWithin, lexicographicComparison
    , distanceFrom, squaredDistanceFrom, signedDistanceAlong, signedDistanceFrom
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto
    , relativeTo, placeIn
    )

{-| A `Point2d` represents a position in 2D space and is defined by its X and Y
coordinates. This module contains a variety of point-related functionality, such
as

  - Measuring distance between points
  - Scaling, rotating, translating, mirroring and projecting points
  - Converting points between different coordinate systems

Points are distinct from vectors but interact with them in well-defined ways;
you can translate a point by a vector to result in a new point, or you can
compute the vector from one point to another, but you cannot 'add' two points
like you can add two vectors.

@docs Point2d


# Constants

@docs origin


# Constructors

@docs fromCoordinates, fromCoordinatesIn, fromPolarCoordinates, fromPolarCoordinatesIn, midpoint, centroid, interpolateFrom, along, circumcenter


# Conversion

@docs fromTuple, toTuple, fromRecord, toRecord


# Properties

@docs coordinates, xCoordinate, yCoordinate, polarCoordinates


# Comparison

@docs equalWithin, lexicographicComparison


# Measurement

@docs distanceFrom, squaredDistanceFrom, signedDistanceAlong, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Bootstrap.Axis2d as Axis2d
import Bootstrap.Frame2d as Frame2d
import Direction2d exposing (Direction2d)
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis2d, Frame2d)
import Quantity exposing (Quantity(..), Squared, Unitless)
import Quantity.Extra as Quantity
import Vector2d exposing (Vector2d)


addTo : Point2d units coordinates -> Vector2d units coordinates -> Point2d units coordinates
addTo point vector =
    translateBy vector point


{-| -}
type alias Point2d units coordinates =
    Types.Point2d units coordinates


{-| The point (0, 0).

    Point2d.origin
    --> Point2d.fromCoordinates ( 0, 0 )

-}
origin : Point2d units coordinates
origin =
    fromCoordinates ( Quantity.zero, Quantity.zero )


{-| Construct a point from its X and Y coordinates.

    point =
        Point2d.fromCoordinates ( 2, 3 )

-}
fromCoordinates : ( Quantity Float units, Quantity Float units ) -> Point2d units coordinates
fromCoordinates givenCoordinates =
    Types.Point2d givenCoordinates


{-| Construct a point from a radius and angle. Radius is measured from the
origin and angle is measured counterclockwise from the positive X direction.

    Point2d.fromPolarCoordinates ( 2, degrees 135 )
    --> Point2d.fromCoordinates ( -1.4142, 1.4142 )

-}
fromPolarCoordinates : ( Quantity Float units, Angle ) -> Point2d units coordinates
fromPolarCoordinates ( givenRadius, givenAngle ) =
    fromCoordinates
        ( Quantity.rCosTheta givenRadius givenAngle
        , Quantity.rSinTheta givenRadius givenAngle
        )


{-| Construct a point halfway between two other points.

    p1 =
        Point2d.fromCoordinates ( 1, 1 )

    p2 =
        Point2d.fromCoordinates ( 3, 7 )

    Point2d.midpoint p1 p2
    --> Point2d.fromCoordinates ( 2, 4 )

-}
midpoint : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
midpoint firstPoint secondPoint =
    interpolateFrom firstPoint secondPoint 0.5


{-| Find the centroid of a list of points. Returns `Nothing` if the list is
empty.

    p0 =
        Point2d.origin

    p1 =
        Point2d.fromCoordinates ( 1, 0 )

    p2 =
        Point2d.fromCoordinates ( 1, 1 )

    Point2d.centroid [ p0, p1, p2 ]
    --> Just (Point2d.fromCoordinates ( 0.6667, 0.3333 ))

-}
centroid : List (Point2d units coordinates) -> Maybe (Point2d units coordinates)
centroid points =
    case points of
        [] ->
            Nothing

        first :: rest ->
            let
                ( x0, y0 ) =
                    coordinates first
            in
            Just (centroidHelp x0 y0 1 Quantity.zero Quantity.zero rest)


centroidHelp : Quantity Float units -> Quantity Float units -> Float -> Quantity Float units -> Quantity Float units -> List (Point2d units coordinates) -> Point2d units coordinates
centroidHelp x0 y0 count dx dy points =
    case points of
        point :: remaining ->
            let
                ( x, y ) =
                    coordinates point

                newDx =
                    dx |> Quantity.plus (x |> Quantity.minus x0)

                newDy =
                    dy |> Quantity.plus (y |> Quantity.minus y0)
            in
            centroidHelp x0 y0 (count + 1) newDx newDy remaining

        [] ->
            let
                scale =
                    1 / count
            in
            fromCoordinates
                ( x0 |> Quantity.plus (Quantity.multiplyBy scale dx)
                , y0 |> Quantity.plus (Quantity.multiplyBy scale dy)
                )


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point2d.origin

    endPoint =
        Point2d.fromCoordinates ( 8, 12 )

    Point2d.interpolateFrom startPoint endPoint 0.25
    --> Point2d.fromCoordinates ( 2, 3 )

Partial application may be useful:

    interpolatedPoint : Float -> Point2d
    interpolatedPoint =
        Point2d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point2d.fromCoordinates ( 0, 0 )
    --> , Point2d.fromCoordinates ( 4, 6 )
    --> , Point2d.fromCoordinates ( 8, 12 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point2d.fromCoordinates ( -4, -6 )

    interpolatedPoint 1.25
    --> Point2d.fromCoordinates ( 10, 15 )

-}
interpolateFrom : Point2d units coordinates -> Point2d units coordinates -> Float -> Point2d units coordinates
interpolateFrom p1 p2 t =
    let
        ( x1, y1 ) =
            coordinates p1

        ( x2, y2 ) =
            coordinates p2
    in
    fromCoordinates
        ( Quantity.interpolateFrom x1 x2 t
        , Quantity.interpolateFrom y1 y2 t
        )


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point2d.along Axis2d.y 3
    --> Point2d.fromCoordinates ( 0, 3 )

Positive and negative distances will be interpreted relative to the direction of
the axis:

    horizontalAxis =
        Axis2d.withDirection Direction2d.negativeX
            (Point2d.fromCoordinates ( 1, 1 ))

    Point2d.along horizontalAxis 3
    --> Point2d.fromCoordinates ( -2, 1 )

    Point2d.along horizontalAxis -3
    --> Point2d.fromCoordinates ( 4, 1 )

-}
along : Axis2d units coordinates -> Quantity Float units -> Point2d units coordinates
along axis distance =
    Axis2d.originPoint axis
        |> translateBy (Vector2d.withLength distance (Axis2d.direction axis))


{-| Construct a point given its local coordinates within a particular frame:

    rotatedFrame =
        Frame2d.atOrigin |> Frame2d.rotateBy (degrees 45)

    Point2d.fromCoordinatesIn rotatedFrame ( 2, 0 )
    --> Point2d.fromCoordinates ( 1.4142, 1.4142 )

-}
fromCoordinatesIn : Frame2d units globalCoordinates defines -> ( Quantity Float units, Quantity Float units ) -> Point2d units globalCoordinates
fromCoordinatesIn frame localCoordinates =
    let
        ( x, y ) =
            localCoordinates

        ( x0, y0 ) =
            coordinates (Frame2d.originPoint frame)

        ( x1, y1 ) =
            Direction2d.components (Frame2d.xDirection frame)

        ( x2, y2 ) =
            Direction2d.components (Frame2d.yDirection frame)
    in
    fromCoordinates
        ( x0 |> Quantity.plus (Quantity.aXbY x1 x x2 y)
        , y0 |> Quantity.plus (Quantity.aXbY y1 x y2 y)
        )


{-| Construct a point given its local polar coordinates within a particular
frame:

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 1 ))

    Point2d.fromPolarCoordinatesIn localFrame
        ( 2, degrees 45 )
    --> Point2d.fromCoordinates ( 3.4142, 2.4142 )

-}
fromPolarCoordinatesIn : Frame2d units globalCoordinates defines -> ( Quantity Float units, Angle ) -> Point2d units globalCoordinates
fromPolarCoordinatesIn frame localPolarCoordinates =
    let
        ( r, theta ) =
            localPolarCoordinates
    in
    fromCoordinatesIn frame
        ( Quantity.rCosTheta r theta
        , Quantity.rSinTheta r theta
        )


{-| Attempt to find the circumcenter of three points; this is the center of the
circle that passes through all three points. If the three given points are
collinear, returns `Nothing`.

    Point2d.circumcenter
        ( Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        , Point2d.fromCoordinates ( 0, 1 )
        )
    --> Just (Point2d.fromCoordinates ( 0.5, 0.5 ))

    Point2d.circumcenter
        ( Point2d.origin
        , Point2d.fromCoordinates ( 2, 1 )
        , Point2d.fromCoordinates ( 4, 0 )
        )
    --> Just (Point2d.fromCoordinates ( 2, -1.5 ))

    Point2d.circumCenter
        ( Point2d.origin
        , Point2d.fromCoordinates ( 2, 0 )
        , Point2d.fromCoordinates ( 4, 0 )
        )
    --> Nothing

    Point2d.circumCenter
        ( Point2d.origin
        , Point2d.origin
        , Point2d.fromCoordinates ( 1, 0 )
        )
    --> Nothing

-}
circumcenter : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Maybe (Point2d units coordinates)
circumcenter p1 p2 p3 =
    let
        (Quantity a2) =
            squaredDistanceFrom p1 p2

        (Quantity b2) =
            squaredDistanceFrom p2 p3

        (Quantity c2) =
            squaredDistanceFrom p3 p1

        t1 =
            a2 * (b2 + c2 - a2)

        t2 =
            b2 * (c2 + a2 - b2)

        t3 =
            c2 * (a2 + b2 - c2)

        sum =
            t1 + t2 + t3
    in
    if sum == 0 then
        Nothing

    else
        let
            w1 =
                t1 / sum

            w2 =
                t2 / sum

            w3 =
                t3 / sum

            ( Quantity x1, Quantity y1 ) =
                coordinates p1

            ( Quantity x2, Quantity y2 ) =
                coordinates p2

            ( Quantity x3, Quantity y3 ) =
                coordinates p3
        in
        Just <|
            fromCoordinates
                ( Quantity (w1 * x3 + w2 * x1 + w3 * x2)
                , Quantity (w1 * y3 + w2 * y1 + w3 * y2)
                )


fromTuple : ( Float, Float ) -> Point2d Unitless coordinates
fromTuple ( x, y ) =
    fromCoordinates ( Quantity.float x, Quantity.float y )


toTuple : Point2d Unitless coordinates -> ( Float, Float )
toTuple point =
    ( Quantity.toFloat (xCoordinate point)
    , Quantity.toFloat (yCoordinate point)
    )


fromRecord : { x : Float, y : Float } -> Point2d Unitless coordinates
fromRecord { x, y } =
    fromCoordinates ( Quantity.float x, Quantity.float y )


toRecord : Point2d Unitless coordinates -> { x : Float, y : Float }
toRecord point =
    { x = Quantity.toFloat (xCoordinate point)
    , y = Quantity.toFloat (yCoordinate point)
    }


{-| Get the coordinates of a point as a tuple.

    ( x, y ) =
        Point2d.coordinates point

-}
coordinates : Point2d units coordinates -> ( Quantity Float units, Quantity Float units )
coordinates (Types.Point2d pointCoordinates) =
    pointCoordinates


{-| Get the X coordinate of a point.

    Point2d.xCoordinate (Point2d.fromCoordinates ( 2, 3 ))
    --> 2

-}
xCoordinate : Point2d units coordinates -> Quantity Float units
xCoordinate (Types.Point2d ( x, _ )) =
    x


{-| Get the Y coordinate of a point.

    Point2d.yCoordinate (Point2d.fromCoordinates ( 2, 3 ))
    --> 3

-}
yCoordinate : Point2d units coordinates -> Quantity Float units
yCoordinate (Types.Point2d ( _, y )) =
    y


{-| Get the polar coordinates (radius and polar angle) of a point.

    Point2d.polarCoordinates
        (Point2d.fromCoordinates ( 1, 1 ))
    --> ( 1.4142, degrees 45 )

-}
polarCoordinates : Point2d units coordinates -> ( Quantity Float units, Angle )
polarCoordinates point =
    let
        ( x, y ) =
            coordinates point
    in
    ( distanceFrom origin point, Angle.atan2 y x )


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point2d.fromCoordinates ( 1, 2 )

    secondPoint =
        Point2d.fromCoordinates ( 0.9999, 2.0002 )

    Point2d.equalWithin 1e-3 firstPoint secondPoint
    --> True

    Point2d.equalWithin 1e-6 firstPoint secondPoint
    --> False

-}
equalWithin : Quantity Float units -> Point2d units coordinates -> Point2d units coordinates -> Bool
equalWithin tolerance firstPoint secondPoint =
    squaredDistanceFrom firstPoint secondPoint
        |> Quantity.lessThanOrEqualTo (Quantity.squared tolerance)


lexicographicComparison : Point2d units coordinates -> Point2d units coordinates -> Order
lexicographicComparison firstPoint secondPoint =
    let
        ( x1, y1 ) =
            coordinates firstPoint

        ( x2, y2 ) =
            coordinates secondPoint
    in
    if x1 /= x2 then
        Quantity.compare x1 x2

    else
        Quantity.compare y1 y2


{-| Find the distance from the first point to the second.

    p1 =
        Point2d.fromCoordinates ( 2, 3 )

    p2 =
        Point2d.fromCoordinates ( 5, 7 )

    Point2d.distanceFrom p1 p2
    --> 5

Partial application can be useful:

    points =
        [ Point2d.fromCoordinates ( 3, 4 )
        , Point2d.fromCoordinates ( 10, 0 )
        , Point2d.fromCoordinates ( -1, 2 )
        ]

    points
        |> List.sortBy
            (Point2d.distanceFrom Point2d.origin)
    --> [ Point2d.fromCoordinates ( -1, 2 )
    --> , Point2d.fromCoordinates ( 3, 4 )
    --> , Point2d.fromCoordinates ( 10, 0 )
    --> ]

-}
distanceFrom : Point2d units coordinates -> Point2d units coordinates -> Quantity Float units
distanceFrom firstPoint secondPoint =
    Quantity.sqrt (squaredDistanceFrom firstPoint secondPoint)


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point2d.squaredDistanceFrom p1 p2
        > (tolerance * tolerance)

is equivalent to but slightly more efficient than

    Point2d.distanceFrom p1 p2 > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `distanceFrom` is much more
readable!

-}
squaredDistanceFrom : Point2d units coordinates -> Point2d units coordinates -> Quantity Float (Squared units)
squaredDistanceFrom firstPoint secondPoint =
    Vector2d.squaredLength (Vector2d.from firstPoint secondPoint)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis2d.withDirection Direction2d.x
            (Point2d.fromCoordinates ( 1, 2 ))

    point =
        Point2d.fromCoordinates ( 3, 3 )

    Point2d.signedDistanceAlong axis point
    --> 2

    Point2d.signedDistanceAlong axis Point2d.origin
    --> -1

-}
signedDistanceAlong : Axis2d units coordinates -> Point2d units coordinates -> Quantity Float units
signedDistanceAlong axis point =
    Vector2d.from (Axis2d.originPoint axis) point
        |> Vector2d.componentIn (Axis2d.direction axis)


{-| Find the perpendicular distance of a point from an axis. The result
will be positive if the point is to the left of the axis and negative if it is
to the right, with the forwards direction defined by the direction of the axis.

    -- A horizontal axis through a point with a Y
    -- coordinate of 2 is effectively the line Y=2
    axis =
        Axis2d.withDirection Direction2d.x
            (Point2d.fromCoordinates ( 1, 2 ))

    point =
        Point2d.fromCoordinates ( 3, 3 )

    -- Since the axis is in the positive X direction,
    -- points above the axis are to the left (positive)
    Point2d.signedDistanceFrom axis point
    -->  1

    -- and points below are to the right (negative)
    Point2d.signedDistanceFrom axis Point2d.origin
    --> -2

This means that reversing an axis will also flip the sign of the result of this
function:

    -- Reversing an axis reverses its direction
    reversedAxis =
        Axis2d.reverse axis

    Point2d.signedDistanceFrom reversedAxis point
    --> -1

    Point2d.signedDistanceFrom reversedAxis Point2d.origin
    --> 2

-}
signedDistanceFrom : Axis2d units coordinates -> Point2d units coordinates -> Quantity Float units
signedDistanceFrom axis point =
    let
        perpendicularDirection =
            Direction2d.rotateCounterclockwise (Axis2d.direction axis)

        displacementVector =
            Vector2d.from (Axis2d.originPoint axis) point
    in
    displacementVector |> Vector2d.componentIn perpendicularDirection


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point2d.fromCoordinates ( 1, 1 )

    point =
        Point2d.fromCoordinates ( 2, 3 )

    Point2d.scaleAbout centerPoint 3 point
    --> Point2d.fromCoordinates ( 4, 7 )

    Point2d.scaleAbout centerPoint 0.5 point
    --> Point2d.fromCoordinates ( 1.5, 2 )

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point2d units coordinates -> Float -> Point2d units coordinates -> Point2d units coordinates
scaleAbout centerPoint scale point =
    let
        ( x0, y0 ) =
            coordinates centerPoint

        ( x, y ) =
            coordinates point
    in
    fromCoordinates
        ( Quantity.scaleAbout x0 scale x
        , Quantity.scaleAbout y0 scale y
        )


{-| Rotate around a given center point counterclockwise by a given angle (in
radians). The point to rotate around is given first and the point to rotate is
given last.

    centerPoint =
        Point2d.fromCoordinates ( 2, 0 )

    angle =
        degrees 45

    point =
        Point2d.fromCoordinates ( 3, 0 )

    Point2d.rotateAround centerPoint angle point
    --> Point2d.fromCoordinates ( 2.7071, 0.7071 )

-}
rotateAround : Point2d units coordinates -> Angle -> Point2d units coordinates -> Point2d units coordinates
rotateAround centerPoint angle point =
    Vector2d.from centerPoint point
        |> Vector2d.rotateBy angle
        |> addTo centerPoint


{-| Translate a point by a given displacement.

    point =
        Point2d.fromCoordinates ( 3, 4 )

    displacement =
        Vector2d.fromComponents ( 1, 2 )

    Point2d.translateBy displacement point
    --> Point2d.fromCoordinates ( 4, 6 )

In more mathematical terms, this is 'point plus vector'. For 'point minus point'
(giving the vector from one point to another), there is [`Vector2d.from`](Vector2d#from).

-}
translateBy : Vector2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
translateBy vector point =
    let
        ( vx, vy ) =
            Vector2d.components vector

        ( px, py ) =
            coordinates point
    in
    fromCoordinates ( px |> Quantity.plus vx, py |> Quantity.plus vy )


{-| Translate a point in a given direction by a given distance.

    point =
        Point2d.fromCoordinates ( 3, 4 )

    point |> Point2d.translateIn Direction2d.x 2
    --> Point2d.fromCoordinates ( 5, 4 )

    point |> Point2d.translateIn Direction2d.y 2
    --> Point2d.fromCoordinates ( 3, 6 )

    angledDirection =
        Direction2d.fromAngle (degrees 45)

    point |> Point2d.translateIn angledDirection 1
    --> Point2d.fromCoordinates ( 3.7071, 4.7071 )

The distance can be negative:

    Point2d.translateIn Direction2d.x -2
    --> Point2d.fromCoordinates ( 1, 4 )

-}
translateIn : Direction2d coordinates -> Quantity Float units -> Point2d units coordinates -> Point2d units coordinates
translateIn direction distance point =
    let
        ( dx, dy ) =
            Direction2d.components direction

        ( px, py ) =
            coordinates point
    in
    fromCoordinates
        ( px |> Quantity.plus (Quantity.multiplyBy dx distance)
        , py |> Quantity.plus (Quantity.multiplyBy dy distance)
        )


{-| Mirror a point across an axis. The result will be the same distance from the
axis but on the opposite side.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    Point2d.mirrorAcross Axis2d.x point
    --> Point2d.fromCoordinates ( 2, -3 )

    Point2d.mirrorAcross Axis2d.y point
    --> Point2d.fromCoordinates ( -2, 3 )

-}
mirrorAcross : Axis2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
mirrorAcross axis point =
    let
        originPoint =
            Axis2d.originPoint axis
    in
    Vector2d.from originPoint point
        |> Vector2d.mirrorAcross axis
        |> addTo originPoint


{-| Project a point perpendicularly onto an axis.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    Point2d.projectOnto Axis2d.x point
    --> Point2d.fromCoordinates ( 2, 0 )

    Point2d.projectOnto Axis2d.y point
    --> Point2d.fromCoordinates ( 0, 3 )

The axis does not have to pass through the origin:

    offsetYAxis =
        Axis2d.withDirection Direction2d.y
            (Point2d.fromCoordinates ( 1, 0 ))

    Point2d.projectOnto offsetYAxis point
    --> Point2d.fromCoordinates ( 1, 3 )

-}
projectOnto : Axis2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
projectOnto axis point =
    let
        originPoint =
            Axis2d.originPoint axis
    in
    Vector2d.from originPoint point
        |> Vector2d.projectOnto axis
        |> addTo originPoint


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Point2d.relativeTo localFrame
        (Point2d.fromCoordinates ( 4, 5 ))
    --> Point2d.fromCoordinates ( 3, 3 )

    Point2d.relativeTo localFrame
        (Point2d.fromCoordinates ( 1, 1 ))
    --> Point2d.fromCoordinates ( 0, -1 )

-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Point2d units globalCoordinates -> Point2d units localCoordinates
relativeTo frame point =
    Vector2d.from (Frame2d.originPoint frame) point
        |> Vector2d.relativeTo frame
        |> Vector2d.components
        |> fromCoordinates


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    Point2d.placeIn localFrame
        (Point2d.fromCoordinates ( 3, 3 ))
    --> Point2d.fromCoordinates ( 4, 5 )

    Point2d.placeIn localFrame
        (Point2d.fromCoordinates ( 0, 1 ))
    --> Point2d.fromCoordinates ( 1, 1 )

-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Point2d units localCoordinates -> Point2d units globalCoordinates
placeIn frame point =
    Vector2d.fromComponents (coordinates point)
        |> Vector2d.placeIn frame
        |> addTo (Frame2d.originPoint frame)
