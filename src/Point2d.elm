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
    , unitless
    , meters, pixels, millimeters, centimeters, inches, feet
    , xy, xyIn, rTheta, rThetaIn, midpoint, interpolateFrom, along, circumcenter
    , fromTuple, toTuple, fromRecord, toRecord
    , fromMeters, toMeters, fromPixels, toPixels, fromUnitless, toUnitless
    , coordinates, xCoordinate, yCoordinate, coordinatesIn, xCoordinateIn, yCoordinateIn
    , equalWithin, lexicographicComparison
    , distanceFrom, signedDistanceAlong, signedDistanceFrom
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto
    , at, at_
    , relativeTo, placeIn
    , centroid, centroidOf, centroid3, centroidN
    , unsafe, unwrap
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


# Literals

@docs unitless

The remaining functions all construct a `Point2d` from X and Y coordinates given
in specific units. Functions like `Point2d.xy` are more useful in generic code,
but these functions are useful for quickly creating hardcoded constant values,
e.g.

    point =
        Point2d.meters 2 3

These functions may also be useful when decoding points from JSON - for example
if you had some JSON where a point was encoded as an object with `x` and `y`
fields measured in meters then you could write a decoder like

    Decode.map2 Point2d.meters
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)

@docs meters, pixels, millimeters, centimeters, inches, feet


# Constructors

@docs xy, xyIn, rTheta, rThetaIn, midpoint, interpolateFrom, along, circumcenter


# Interop

These functions are useful for interoperability with other Elm code that uses
plain `Float` tuples or records to represent points.

@docs fromTuple, toTuple, fromRecord, toRecord


## Zero-copy conversions

These functions allow zero-overhead conversion of points to and from records
with `x` and `y` `Float` fields, useful for efficient interop with other code
that represents points as plain records.

@docs fromMeters, toMeters, fromPixels, toPixels, fromUnitless, toUnitless


# Properties

@docs coordinates, xCoordinate, yCoordinate, coordinatesIn, xCoordinateIn, yCoordinateIn


# Comparison

@docs equalWithin, lexicographicComparison


# Measurement

@docs distanceFrom, signedDistanceAlong, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn


# Centroid calculation

@docs centroid, centroidOf, centroid3, centroidN


# Advanced

These functions are unsafe because they require you to track units manually. In
general you should prefer other functions instead, but these functions may be
useful when writing generic/library code.

@docs unsafe, unwrap

-}

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis2d, BoundingBox2d, Frame2d)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity(..), Rate, Squared, Unitless)
import Quantity.Extra as Quantity
import Vector2d exposing (Vector2d)


{-| -}
type alias Point2d units coordinates =
    Types.Point2d units coordinates


{-| Construct a point from its raw X and Y coordinates as `Float` values. The
values must be in whatever units the resulting point is considered to use
(usually meters or pixels). You should generally use something safer such as
[`meters`](#meters), [`fromPixels`](#fromPixels), [`xy`](#xy),
[`fromRecord`](#fromRecord) etc.
-}
unsafe : { x : Float, y : Float } -> Point2d units coordinates
unsafe givenCoordinates =
    Types.Point2d givenCoordinates


{-| Extract a point's raw X and Y coordinates as `Float` values. These values
will be in whatever units the point has (usually meters or pixels). You should
generally use something safer such as [`toMeters`](#toMeters),
[`toRecord`](#toRecord), [`xCoordinate`](#xCoordinate) etc.
-}
unwrap : Point2d units coordinates -> { x : Float, y : Float }
unwrap (Types.Point2d pointCoordinates) =
    pointCoordinates


{-| The point with coordinates (0, 0).
-}
origin : Point2d units coordinates
origin =
    Types.Point2d { x = 0, y = 0 }


{-| Construct a point from its X and Y coordinates.

    point =
        Point2d.xy (Length.meters 2) (Length.meters 3)

-}
xy : Quantity Float units -> Quantity Float units -> Point2d units coordinates
xy (Quantity x) (Quantity y) =
    Types.Point2d { x = x, y = y }


{-| -}
millimeters : Float -> Float -> Point2d Meters coordinates
millimeters x y =
    xy (Length.millimeters x) (Length.millimeters y)


{-| -}
centimeters : Float -> Float -> Point2d Meters coordinates
centimeters x y =
    xy (Length.centimeters x) (Length.centimeters y)


{-| -}
meters : Float -> Float -> Point2d Meters coordinates
meters x y =
    Types.Point2d { x = x, y = y }


{-| -}
inches : Float -> Float -> Point2d Meters coordinates
inches x y =
    xy (Length.inches x) (Length.inches y)


{-| -}
feet : Float -> Float -> Point2d Meters coordinates
feet x y =
    xy (Length.feet x) (Length.feet y)


{-| -}
pixels : Float -> Float -> Point2d Pixels coordinates
pixels x y =
    Types.Point2d { x = x, y = y }


{-| Construct a unitless `Point2d` value from its X and Y coordinates. See also
[`fromUnitless`](#fromUnitless).
-}
unitless : Float -> Float -> Point2d Unitless coordinates
unitless x y =
    Types.Point2d { x = x, y = y }


{-| Construct a point from a radius and angle. Radius is measured from the
origin and angle is measured counterclockwise from the positive X direction.

    Point2d.rTheta (Length.meters 2) (Angle.degrees 135)
    --> Point2d.meters -1.4142 1.4142

-}
rTheta : Quantity Float units -> Angle -> Point2d units coordinates
rTheta (Quantity r) (Quantity theta) =
    Types.Point2d
        { x = r * cos theta
        , y = r * sin theta
        }


{-| Construct a point halfway between two other points.

    Point2d.midpoint
        (Point2d.meters 1 1)
        (Point2d.meters 3 7)
    --> Point2d.meters 2 4

-}
midpoint : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
midpoint (Types.Point2d p1) (Types.Point2d p2) =
    Types.Point2d
        { x = p1.x + 0.5 * (p2.x - p1.x)
        , y = p1.y + 0.5 * (p2.y - p1.y)
        }


{-| Find the centroid (average) of one or more points, by passing the first
point and then all remaining points. This allows this function to return a
`Point2d` instead of a `Maybe Point2d`. You would generally use `centroid`
within a `case` expression:

    case points of
        [] ->
            -- some default behavior

        first :: rest ->
            let
                centroid =
                    Point2d.centroid first rest
            in
            ...

Alternatively, you can use [`centroidN`](#centroidN) instead.

-}
centroid : Point2d units coordinates -> List (Point2d units coordinates) -> Point2d units coordinates
centroid (Types.Point2d p0) rest =
    centroidHelp p0.x p0.y 1 0 0 rest


centroidHelp : Float -> Float -> Float -> Float -> Float -> List (Point2d units coordinates) -> Point2d units coordinates
centroidHelp x0 y0 count dx dy points =
    case points of
        (Types.Point2d p) :: remaining ->
            centroidHelp
                x0
                y0
                (count + 1)
                (dx + (p.x - x0))
                (dy + (p.y - y0))
                remaining

        [] ->
            Types.Point2d
                { x = x0 + dx / count
                , y = y0 + dy / count
                }


{-| Like `centroid`, but lets you work with any kind of data as long as a point
can be extracted/constructed from it. For example, to get the centroid of a
bunch of vertices:

    type alias Vertex =
        { position : Point2d Meters World
        , color : Color
        , id : Int
        }

    vertexCentroid =
        Point2d.centroidOf .position
            firstVertex
            [ secondVertex
            , thirdVertex
            ]

-}
centroidOf : (a -> Point2d units coordinates) -> a -> List a -> Point2d units coordinates
centroidOf toPoint first rest =
    let
        (Types.Point2d p0) =
            toPoint first
    in
    centroidOfHelp toPoint p0.x p0.y 1 0 0 rest


centroidOfHelp : (a -> Point2d units coordinates) -> Float -> Float -> Float -> Float -> Float -> List a -> Point2d units coordinates
centroidOfHelp toPoint x0 y0 count dx dy values =
    case values of
        next :: remaining ->
            let
                (Types.Point2d p) =
                    toPoint next
            in
            centroidOfHelp
                toPoint
                x0
                y0
                (count + 1)
                (dx + (p.x - x0))
                (dy + (p.y - y0))
                remaining

        [] ->
            Types.Point2d
                { x = x0 + dx / count
                , y = y0 + dy / count
                }


{-| Find the centroid of three points;

    Point2d.centroid3d p1 p2 p3

is equivalent to

    Point2d.centroid p1 [ p2, p3 ]

but is more efficient.

-}
centroid3 : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
centroid3 (Types.Point2d p1) (Types.Point2d p2) (Types.Point2d p3) =
    Types.Point2d
        { x = p1.x + (p2.x - p1.x) / 3 + (p3.x - p1.x) / 3
        , y = p1.y + (p2.y - p1.y) / 3 + (p3.y - p1.y) / 3
        }


{-| Find the centroid of a list of _N_ points. If the list is empty, returns
`Nothing`. If you know you have at least one point, you can use
[`centroid`](#centroid) instead to avoid the `Maybe`.
-}
centroidN : List (Point2d units coordinates) -> Maybe (Point2d units coordinates)
centroidN points =
    case points of
        first :: rest ->
            Just (centroid first rest)

        [] ->
            Nothing


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point2d.origin

    endPoint =
        Point2d.meters 8 12

    Point2d.interpolateFrom startPoint endPoint 0.25
    --> Point2d.meters 2 3

Partial application may be useful:

    interpolatedPoint : Float -> Point2d
    interpolatedPoint =
        Point2d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point2d.meters 0 0
    --> , Point2d.meters 4 6
    --> , Point2d.meters 8 12
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point2d.meters -4 -6

    interpolatedPoint 1.25
    --> Point2d.meters 10 15

-}
interpolateFrom : Point2d units coordinates -> Point2d units coordinates -> Float -> Point2d units coordinates
interpolateFrom (Types.Point2d p1) (Types.Point2d p2) t =
    if t <= 0.5 then
        Types.Point2d
            { x = p1.x + t * (p2.x - p1.x)
            , y = p1.y + t * (p2.y - p1.y)
            }

    else
        Types.Point2d
            { x = p2.x + (1 - t) * (p1.x - p2.x)
            , y = p2.y + (1 - t) * (p1.y - p2.y)
            }


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point2d.along Axis2d.y (Length.meters 3)
    --> Point2d.meters 0 3

Positive and negative distances will be interpreted relative to the direction of
the axis:

    horizontalAxis =
        Axis2d.withDirection Direction2d.negativeX
            (Point2d.meters 1 1)

    Point2d.along horizontalAxis (Length.meters 3)
    --> Point2d.meters -2 1

    Point2d.along horizontalAxis (Length.meters -3)
    --> Point2d.meters 4 1

-}
along : Axis2d units coordinates -> Quantity Float units -> Point2d units coordinates
along (Types.Axis2d axis) (Quantity distance) =
    let
        (Types.Point2d p0) =
            axis.originPoint

        (Types.Direction2d d) =
            axis.direction
    in
    Types.Point2d
        { x = p0.x + distance * d.x
        , y = p0.y + distance * d.y
        }


{-| Construct a point given its local coordinates within a particular frame:

    rotatedFrame =
        Frame2d.atOrigin
            |> Frame2d.rotateBy (Angle.degrees 45)

    Point2d.xyIn rotatedFrame
        (Length.meters 2)
        (Length.meters 0)
    --> Point2d.meters 1.4142 1.4142

-}
xyIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Quantity Float units -> Quantity Float units -> Point2d units globalCoordinates
xyIn (Types.Frame2d frame) (Quantity x) (Quantity y) =
    let
        (Types.Point2d p0) =
            frame.originPoint

        (Types.Direction2d i) =
            frame.xDirection

        (Types.Direction2d j) =
            frame.yDirection
    in
    Types.Point2d
        { x = p0.x + x * i.x + y * j.x
        , y = p0.y + x * i.y + y * j.y
        }


{-| Construct a point given its local polar coordinates within a particular
frame:

    localFrame =
        Frame2d.atPoint (Point2d.meters 2 1)

    Point2d.rThetaIn localFrame
        (Length.meters 2)
        (Angle.degrees 45)
    --> Point2d.meters 3.4142 2.4142

-}
rThetaIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Quantity Float units -> Angle -> Point2d units globalCoordinates
rThetaIn (Types.Frame2d frame) (Quantity r) (Quantity theta) =
    let
        (Types.Point2d p0) =
            frame.originPoint

        (Types.Direction2d i) =
            frame.xDirection

        (Types.Direction2d j) =
            frame.yDirection

        x =
            r * cos theta

        y =
            r * sin theta
    in
    Types.Point2d
        { x = p0.x + x * i.x + y * j.x
        , y = p0.y + x * i.y + y * j.y
        }


{-| Attempt to find the circumcenter of three points; this is the center of the
circle that passes through all three points. If the three given points are
collinear, returns `Nothing`.

    Point2d.circumcenter
        Point2d.origin
        (Point2d.meters 1 0)
        (Point2d.meters 0 1)
    --> Just (Point2d.meters 0.5 0.5)

    -- Ambiguous
    Point2d.circumCenter
        Point2d.origin
        Point2d.origin
        (Point2d.meters 1 0)
    --> Nothing

    -- Impossible
    Point2d.circumCenter
        Point2d.origin
        (Point2d.meters 2 0)
        (Point2d.meters 4 0)
    --> Nothing

-}
circumcenter : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Maybe (Point2d units coordinates)
circumcenter p1 p2 p3 =
    let
        (Quantity a) =
            distanceFrom p1 p2

        (Quantity b) =
            distanceFrom p2 p3

        (Quantity c) =
            distanceFrom p3 p1
    in
    if a >= b then
        if a >= c then
            circumenterHelp p1 p2 p3 a b c

        else
            circumenterHelp p3 p1 p2 c a b

    else if b >= c then
        circumenterHelp p2 p3 p1 b c a

    else
        circumenterHelp p3 p1 p2 c a b


circumenterHelp : Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates -> Float -> Float -> Float -> Maybe (Point2d units coordinates)
circumenterHelp (Types.Point2d p1) (Types.Point2d p2) (Types.Point2d p3) a b c =
    let
        bc =
            b * c
    in
    if bc == 0 then
        Nothing

    else
        let
            bx =
                p3.x - p2.x

            by =
                p3.y - p2.y

            cx =
                p1.x - p3.x

            cy =
                p1.y - p3.y

            sinA =
                (bx * cy - by * cx) / bc
        in
        if sinA == 0 then
            Nothing

        else
            let
                ax =
                    p2.x - p1.x

                ay =
                    p2.y - p1.y

                cosA =
                    (bx * cx + by * cy) / bc

                scale =
                    cosA / (2 * sinA)
            in
            Just <|
                Types.Point2d
                    { x = p1.x + 0.5 * ax + scale * ay
                    , y = p1.y + 0.5 * ay - scale * ax
                    }


{-| Construct a `Point2d` from a tuple of `Float` values, by specifying what
units those values are in.

    Point2d.fromTuple Length.meters ( 2, 3 )
    --> Point2d.meters 2 3

-}
fromTuple : (Float -> Quantity Float units) -> ( Float, Float ) -> Point2d units coordinates
fromTuple toQuantity ( x, y ) =
    xy (toQuantity x) (toQuantity y)


{-| Convert a `Point2d` to a tuple of `Float` values, by specifying what units you want the result
to be in.

    point =
        Point2d.feet 2 3

    Point2d.toTuple Length.inInches point
    --> ( 24, 36 )

-}
toTuple : (Quantity Float units -> Float) -> Point2d units coordinates -> ( Float, Float )
toTuple fromQuantity point =
    ( fromQuantity (xCoordinate point)
    , fromQuantity (yCoordinate point)
    )


{-| Construct a `Point2d` from a record with `Float` fields, by specifying what units those fields
are in.

    Point2d.fromRecord Length.inches { x = 24, y = 36 }
    --> Point2d.inches 24 36

-}
fromRecord : (Float -> Quantity Float units) -> { x : Float, y : Float } -> Point2d units coordinates
fromRecord toQuantity { x, y } =
    xy (toQuantity x) (toQuantity y)


{-| Convert a `Point2d` to a record with `Float` fields, by specifying what units you want the
result to be in.

    point =
        Point2d.meters 2 3

    Point2d.toRecord Length.inCentimeters point
    --> { x = 200, y = 300 }

-}
toRecord : (Quantity Float units -> Float) -> Point2d units coordinates -> { x : Float, y : Float }
toRecord fromQuantity point =
    { x = fromQuantity (xCoordinate point)
    , y = fromQuantity (yCoordinate point)
    }


{-| -}
fromMeters : { x : Float, y : Float } -> Point2d Meters coordinates
fromMeters givenCoordinates =
    Types.Point2d givenCoordinates


{-| -}
toMeters : Point2d Meters coordinates -> { x : Float, y : Float }
toMeters (Types.Point2d pointCoordinates) =
    pointCoordinates


{-| -}
fromPixels : { x : Float, y : Float } -> Point2d Pixels coordinates
fromPixels givenCoordinates =
    Types.Point2d givenCoordinates


{-| -}
toPixels : Point2d Pixels coordinates -> { x : Float, y : Float }
toPixels (Types.Point2d pointCoordinates) =
    pointCoordinates


{-| -}
fromUnitless : { x : Float, y : Float } -> Point2d Unitless coordinates
fromUnitless givenCoordinates =
    Types.Point2d givenCoordinates


{-| -}
toUnitless : Point2d Unitless coordinates -> { x : Float, y : Float }
toUnitless (Types.Point2d pointCoordinates) =
    pointCoordinates


{-| Convert a point from one units type to another, by providing a conversion factor given as a
rate of change of destination units with respect to source units.

    worldPoint =
        Point2d.meters 2 3

    resolution : Quantity Float (Rate Pixels Meters)
    resolution =
        Pixels.pixels 100 |> Quantity.per (Length.meters 1)

    worldPoint |> Point2d.at resolution
    --> Point2d.pixels 200 300

-}
at : Quantity Float (Rate destinationUnits sourceUnits) -> Point2d sourceUnits coordinates -> Point2d destinationUnits coordinates
at (Quantity rate) (Types.Point2d p) =
    Types.Point2d
        { x = rate * p.x
        , y = rate * p.y
        }


{-| Convert a point from one units type to another, by providing an 'inverse' conversion factor
given as a rate of change of source units with respect to destination units.

    screenPoint =
        Point2d.pixels 200 300

    resolution : Quantity Float (Rate Pixels Meters)
    resolution =
        Pixels.pixels 50 |> Quantity.per (Length.meters 1)

    screenPoint |> Point2d.at_ resolution
    --> Point2d.meters 4 6

-}
at_ : Quantity Float (Rate sourceUnits destinationUnits) -> Point2d sourceUnits coordinates -> Point2d destinationUnits coordinates
at_ (Quantity rate) (Types.Point2d p) =
    Types.Point2d
        { x = p.x / rate
        , y = p.y / rate
        }


{-| Find the X coordinate of a point relative to a given frame.
-}
xCoordinateIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Point2d units globalCoordinates -> Quantity Float units
xCoordinateIn (Types.Frame2d frame) (Types.Point2d p) =
    let
        (Types.Point2d p0) =
            frame.originPoint

        (Types.Direction2d d) =
            frame.xDirection
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y)


{-| Find the Y coordinate of a point relative to a given frame.
-}
yCoordinateIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Point2d units globalCoordinates -> Quantity Float units
yCoordinateIn (Types.Frame2d frame) (Types.Point2d p) =
    let
        (Types.Point2d p0) =
            frame.originPoint

        (Types.Direction2d d) =
            frame.yDirection
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y)


{-| Get the X and Y coordinates of a point as a tuple.

    Point2d.coordinates (Point2d.meters 2 3)
    --> ( Length.meters 2, Length.meters 3 )

-}
coordinates :
    Point2d units coordinates
    -> ( Quantity Float units, Quantity Float units )
coordinates (Types.Point2d p) =
    ( Quantity p.x, Quantity p.y )


{-| Get the X and Y coordinates of a point relative to a given frame, as a
tuple; these are the coordinates the point would have as viewed by an observer
in that frame.
-}
coordinatesIn :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Point2d units globalCoordinates
    -> ( Quantity Float units, Quantity Float units )
coordinatesIn (Types.Frame2d frame) (Types.Point2d p) =
    let
        (Types.Point2d p0) =
            frame.originPoint

        (Types.Direction2d dx) =
            frame.xDirection

        (Types.Direction2d dy) =
            frame.yDirection

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y
    in
    ( Quantity (deltaX * dx.x + deltaY * dx.y)
    , Quantity (deltaX * dy.x + deltaY * dy.y)
    )


{-| Get the X coordinate of a point.

    Point2d.xCoordinate (Point2d.meters 2 3)
    --> Length.meters 2

-}
xCoordinate : Point2d units coordinates -> Quantity Float units
xCoordinate (Types.Point2d p) =
    Quantity p.x


{-| Get the Y coordinate of a point.

    Point2d.yCoordinate (Point2d.meters 2 3)
    --> Length.meters 3

-}
yCoordinate : Point2d units coordinates -> Quantity Float units
yCoordinate (Types.Point2d p) =
    Quantity p.y


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point2d.meters 1 2

    secondPoint =
        Point2d.meters 0.9999 2.0002

    Point2d.equalWithin (Length.millimeters 1)
        firstPoint
        secondPoint
    --> True

    Point2d.equalWithin (Length.microns 1)
        firstPoint
        secondPoint
    --> False

-}
equalWithin : Quantity Float units -> Point2d units coordinates -> Point2d units coordinates -> Bool
equalWithin (Quantity eps) (Types.Point2d p1) (Types.Point2d p2) =
    if eps > 0 then
        let
            nx =
                (p2.x - p1.x) / eps

            ny =
                (p2.y - p1.y) / eps
        in
        nx * nx + ny * ny <= 1

    else if eps == 0 then
        p1.x == p2.x && p1.y == p2.y

    else
        False


{-| Compare two `Point2d` values lexicographically: first by X coordinate, then
by Y. Can be used to provide a sort order for `Point2d` values.
-}
lexicographicComparison : Point2d units coordinates -> Point2d units coordinates -> Order
lexicographicComparison (Types.Point2d p1) (Types.Point2d p2) =
    if p1.x /= p2.x then
        compare p1.x p2.x

    else
        compare p1.y p2.y


{-| Find the distance from the first point to the second.

    Point2d.distanceFrom
        (Point2d.meters 2 3)
        (Point2d.meters 5 7)
    --> Length.meters 5

Partial application can be useful:

    points =
        [ Point2d.meters 3 4
        , Point2d.meters 10 0
        , Point2d.meters -1 2
        ]

    points
        |> Quantity.sortBy
            (Point2d.distanceFrom Point2d.origin)
    --> [ Point2d.meters -1 2
    --> , Point2d.meters 3 4
    --> , Point2d.meters 10 0
    --> ]

-}
distanceFrom : Point2d units coordinates -> Point2d units coordinates -> Quantity Float units
distanceFrom (Types.Point2d p1) (Types.Point2d p2) =
    let
        deltaX =
            p2.x - p1.x

        deltaY =
            p2.y - p1.y

        largestComponent =
            max (abs deltaX) (abs deltaY)
    in
    if largestComponent == 0 then
        Quantity.zero

    else
        let
            scaledX =
                deltaX / largestComponent

            scaledY =
                deltaY / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)
        in
        Quantity (scaledLength * largestComponent)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis2d.withDirection Direction2d.x
            (Point2d.meters 1 2)

    point =
        Point2d.meters 3 3

    Point2d.signedDistanceAlong axis point
    --> Length.meters 2

    Point2d.signedDistanceAlong axis Point2d.origin
    --> Length.meters -1

-}
signedDistanceAlong : Axis2d units coordinates -> Point2d units coordinates -> Quantity Float units
signedDistanceAlong (Types.Axis2d axis) (Types.Point2d p) =
    let
        (Types.Point2d p0) =
            axis.originPoint

        (Types.Direction2d d) =
            axis.direction
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y)


{-| Find the perpendicular distance of a point from an axis. The result
will be positive if the point is to the left of the axis and negative if it is
to the right, with the forwards direction defined by the direction of the axis.

    -- A horizontal axis through a point with a Y
    -- coordinate of 2 is effectively the line Y=2
    axis =
        Axis2d.withDirection Direction2d.x
            (Point2d.meters 1 2)

    point =
        Point2d.meters 3 3

    -- Since the axis is in the positive X direction,
    -- points above the axis are to the left (positive)
    Point2d.signedDistanceFrom axis point
    -->  Length.meters 1

    -- and points below are to the right (negative)
    Point2d.signedDistanceFrom axis Point2d.origin
    --> Length.meters -2

This means that reversing an axis will also flip the sign of the result of this
function:

    -- Reversing an axis reverses its direction
    reversedAxis =
        Axis2d.reverse axis

    Point2d.signedDistanceFrom reversedAxis point
    --> Length.meters -1

    Point2d.signedDistanceFrom reversedAxis Point2d.origin
    --> Length.meters 2

-}
signedDistanceFrom : Axis2d units coordinates -> Point2d units coordinates -> Quantity Float units
signedDistanceFrom (Types.Axis2d axis) (Types.Point2d p) =
    let
        (Types.Point2d p0) =
            axis.originPoint

        (Types.Direction2d d) =
            axis.direction
    in
    Quantity ((p.y - p0.y) * d.x - (p.x - p0.x) * d.y)


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point2d.meters 1 1

    point =
        Point2d.meters 2 3

    Point2d.scaleAbout centerPoint 3 point
    --> Point2d.meters 4 7

    Point2d.scaleAbout centerPoint 0.5 point
    --> Point2d.meters 1.5 2

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point2d units coordinates -> Float -> Point2d units coordinates -> Point2d units coordinates
scaleAbout (Types.Point2d p0) k (Types.Point2d p) =
    Types.Point2d
        { x = p0.x + k * (p.x - p0.x)
        , y = p0.y + k * (p.y - p0.y)
        }


{-| Rotate around a given center point counterclockwise by a given angle. The
point to rotate around is given first and the point to rotate is given last.

    centerPoint =
        Point2d.meters 2 0

    angle =
        Angle.degrees 45

    point =
        Point2d.meters 3 0

    Point2d.rotateAround centerPoint angle point
    --> Point2d.meters 2.7071 0.7071

-}
rotateAround : Point2d units coordinates -> Angle -> Point2d units coordinates -> Point2d units coordinates
rotateAround (Types.Point2d p0) (Quantity theta) (Types.Point2d p) =
    let
        c =
            cos theta

        s =
            sin theta

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y
    in
    Types.Point2d
        { x = p0.x + c * deltaX - s * deltaY
        , y = p0.y + s * deltaX + c * deltaY
        }


{-| Translate a point by a given displacement.

    point =
        Point2d.meters 3 4

    displacement =
        Vector2d.meters 1 2

    Point2d.translateBy displacement point
    --> Point2d.meters 4 6

In more mathematical terms, this is 'point plus vector'. For 'point minus point'
(giving the vector from one point to another), there is [`Vector2d.from`](Vector2d#from).

-}
translateBy : Vector2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
translateBy (Types.Vector2d v) (Types.Point2d p) =
    Types.Point2d
        { x = p.x + v.x
        , y = p.y + v.y
        }


{-| Translate a point in a given direction by a given distance.

    point =
        Point2d.meters 3 4

    point
        |> Point2d.translateIn Direction2d.x
            (Length.meters 2)
    --> Point2d.meters 5 4

    point
        |> Point2d.translateIn Direction2d.y
            (Length.meters 2)
    --> Point2d.meters 3 6

    angledDirection =
        Direction2d.degrees 45

    point
        |> Point2d.translateIn angledDirection
            (Length.meters 1)
    --> Point2d.meters 3.7071 4.7071

The distance can be negative:

    point
        |> Point2d.translateIn Direction2d.x
            (Length.meters -2)
    --> Point2d.meters 1 4

-}
translateIn : Direction2d coordinates -> Quantity Float units -> Point2d units coordinates -> Point2d units coordinates
translateIn (Types.Direction2d d) (Quantity distance) (Types.Point2d p) =
    Types.Point2d
        { x = p.x + distance * d.x
        , y = p.y + distance * d.y
        }


{-| Mirror a point across an axis. The result will be the same distance from the
axis but on the opposite side.

    point =
        Point2d.meters 2 3

    Point2d.mirrorAcross Axis2d.x point
    --> Point2d.meters 2 -3

    Point2d.mirrorAcross Axis2d.y point
    --> Point2d.meters -2 3

-}
mirrorAcross : Axis2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
mirrorAcross (Types.Axis2d axis) (Types.Point2d p) =
    let
        (Types.Direction2d d) =
            axis.direction

        (Types.Point2d p0) =
            axis.originPoint

        a =
            1 - 2 * d.y * d.y

        b =
            2 * d.x * d.y

        c =
            1 - 2 * d.x * d.x

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y
    in
    Types.Point2d
        { x = p0.x + a * deltaX + b * deltaY
        , y = p0.y + b * deltaX + c * deltaY
        }


{-| Project a point perpendicularly onto an axis.

    point =
        Point2d.meters 2 3

    Point2d.projectOnto Axis2d.x point
    --> Point2d.meters 2 0

    Point2d.projectOnto Axis2d.y point
    --> Point2d.meters 0 3

The axis does not have to pass through the origin:

    offsetYAxis =
        Axis2d.withDirection Direction2d.y
            (Point2d.meters 1 0)

    Point2d.projectOnto offsetYAxis point
    --> Point2d.meters 1 3

-}
projectOnto : Axis2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
projectOnto (Types.Axis2d axis) (Types.Point2d p) =
    let
        (Types.Point2d p0) =
            axis.originPoint

        (Types.Direction2d d) =
            axis.direction

        distance =
            (p.x - p0.x) * d.x + (p.y - p0.y) * d.y
    in
    Types.Point2d
        { x = p0.x + distance * d.x
        , y = p0.y + distance * d.y
        }


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.meters 1 2)

    Point2d.relativeTo localFrame (Point2d.meters 4 5)
    --> Point2d.meters 3 3

    Point2d.relativeTo localFrame (Point2d.meters 1 1)
    --> Point2d.meters 0 -1

-}
relativeTo :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Point2d units globalCoordinates
    -> Point2d units localCoordinates
relativeTo (Types.Frame2d frame) (Types.Point2d p) =
    let
        (Types.Point2d p0) =
            frame.originPoint

        (Types.Direction2d i) =
            frame.xDirection

        (Types.Direction2d j) =
            frame.yDirection

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y
    in
    Types.Point2d
        { x = deltaX * i.x + deltaY * i.y
        , y = deltaX * j.x + deltaY * j.y
        }


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.meters 1 2)

    Point2d.placeIn localFrame (Point2d.meters 3 3)
    --> Point2d.meters 4 5

    Point2d.placeIn localFrame (Point2d.meters 0 1)
    --> Point2d.meters 1 1

-}
placeIn :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Point2d units localCoordinates
    -> Point2d units globalCoordinates
placeIn (Types.Frame2d frame) (Types.Point2d p) =
    let
        (Types.Point2d p0) =
            frame.originPoint

        (Types.Direction2d i) =
            frame.xDirection

        (Types.Direction2d j) =
            frame.yDirection
    in
    Types.Point2d
        { x = p0.x + p.x * i.x + p.y * j.x
        , y = p0.y + p.x * i.y + p.y * j.y
        }
