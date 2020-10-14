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
    , unitless
    , meters, pixels, millimeters, centimeters, inches, feet
    , xyz, xyzIn, midpoint, interpolateFrom, along, on, xyOn, rThetaOn, circumcenter
    , fromTuple, toTuple, fromRecord, toRecord
    , fromMeters, toMeters, fromPixels, toPixels, fromUnitless, toUnitless
    , coordinates, xCoordinate, yCoordinate, zCoordinate, coordinatesIn, xCoordinateIn, yCoordinateIn, zCoordinateIn
    , equalWithin, lexicographicComparison
    , distanceFrom, signedDistanceAlong, distanceFromAxis, signedDistanceFrom
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectOntoAxis
    , at, at_
    , relativeTo, placeIn, projectInto
    , centroid, centroidOf, centroid3, centroidN
    , unsafe, unwrap
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


# Literals

@docs unitless

The remaining functions all construct a `Point3d` from X, Y and Z coordinates
given in specific units. Functions like `Point3d.xyz` are more useful in generic
code, but these functions are useful for quickly creating hardcoded constant
values, e.g.

    point =
        Point3d.meters 2 3 1

These functions may also be useful when decoding points from JSON - for example
if you had some JSON where a point was encoded as an object with `x`, `y` and
`z` fields measured in meters then you could write a decoder like

    Decode.map3 Point3d.meters
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "z" Decode.float)

@docs meters, pixels, millimeters, centimeters, inches, feet


# Constructors

@docs xyz, xyzIn, midpoint, interpolateFrom, along, on, xyOn, rThetaOn, circumcenter


# Interop

These functions are useful for interoperability with other Elm code that uses
plain `Float` tuples or records to represent points.

@docs fromTuple, toTuple, fromRecord, toRecord


## Zero-copy conversions

These functions allow zero-overhead conversion of points to and from records
with `x`, `y` and `z` `Float` fields, useful for efficient interop with other
code that represents points as plain records.

@docs fromMeters, toMeters, fromPixels, toPixels, fromUnitless, toUnitless


# Properties

@docs coordinates, xCoordinate, yCoordinate, zCoordinate, coordinatesIn, xCoordinateIn, yCoordinateIn, zCoordinateIn


# Comparison

@docs equalWithin, lexicographicComparison


# Measurement

@docs distanceFrom, signedDistanceAlong, distanceFromAxis, signedDistanceFrom


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectOntoAxis


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn, projectInto


# Centroid calculation

@docs centroid, centroidOf, centroid3, centroidN


# Advanced

These functions are unsafe because they require you to track units manually. In
general you should prefer other functions instead, but these functions may be
useful when writing generic/library code.

@docs unsafe, unwrap

-}

import Angle exposing (Angle)
import Direction3d exposing (Direction3d)
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis3d, BoundingBox3d, Frame3d, Plane3d, SketchPlane3d)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate, Squared, Unitless)
import Quantity.Extra as Quantity
import Vector3d exposing (Vector3d)


{-| -}
type alias Point3d units coordinates =
    Types.Point3d units coordinates


{-| Construct a point from its raw X, Y and Z coordinates as `Float` values. The
values must be in whatever units the resulting point is considered to use
(usually meters or pixels). You should generally use something safer such as
[`meters`](#meters), [`fromPixels`](#fromPixels), [`xyz`](#xyz),
[`fromRecord`](#fromRecord) etc.
-}
unsafe : { x : Float, y : Float, z : Float } -> Point3d units coordinates
unsafe givenCoordinates =
    Types.Point3d givenCoordinates


{-| Extract a point's raw X, Y and Z coordinates as `Float` values. These values
will be in whatever units the point has (usually meters or pixels). You should
generally use something safer such as [`toMeters`](#toMeters),
[`toRecord`](#toRecord), [`xCoordinate`](#xCoordinate) etc.
-}
unwrap : Point3d units coordinates -> { x : Float, y : Float, z : Float }
unwrap (Types.Point3d pointCoordinates) =
    pointCoordinates


{-| The point with coordinates (0, 0, 0).
-}
origin : Point3d units coordinates
origin =
    Types.Point3d
        { x = 0
        , y = 0
        , z = 0
        }


{-| Construct a point from its X, Y and Z coordinates.

    point =
        Point3d.xyz
            (Length.meters 2)
            (Length.meters 1)
            (Length.meters 3)

-}
xyz : Quantity Float units -> Quantity Float units -> Quantity Float units -> Point3d units coordinates
xyz (Quantity x) (Quantity y) (Quantity z) =
    Types.Point3d
        { x = x
        , y = y
        , z = z
        }


{-| -}
millimeters : Float -> Float -> Float -> Point3d Meters coordinates
millimeters x y z =
    xyz (Length.millimeters x) (Length.millimeters y) (Length.millimeters z)


{-| -}
centimeters : Float -> Float -> Float -> Point3d Meters coordinates
centimeters x y z =
    xyz (Length.centimeters x) (Length.centimeters y) (Length.centimeters z)


{-| -}
meters : Float -> Float -> Float -> Point3d Meters coordinates
meters x y z =
    Types.Point3d
        { x = x
        , y = y
        , z = z
        }


{-| -}
inches : Float -> Float -> Float -> Point3d Meters coordinates
inches x y z =
    xyz (Length.inches x) (Length.inches y) (Length.inches z)


{-| -}
feet : Float -> Float -> Float -> Point3d Meters coordinates
feet x y z =
    xyz (Length.feet x) (Length.feet y) (Length.feet z)


{-| -}
pixels : Float -> Float -> Float -> Point3d Pixels coordinates
pixels x y z =
    Types.Point3d
        { x = x
        , y = y
        , z = z
        }


{-| Construct a unitless `Point3d` value from its X, Y and Z coordinates. See
also [`fromUnitless`](#fromUnitless).
-}
unitless : Float -> Float -> Float -> Point3d Unitless coordinates
unitless x y z =
    Types.Point3d
        { x = x
        , y = y
        , z = z
        }


{-| Construct a point halfway between two other points.

    Point3d.midpoint
        (Point3d.meters 1 1 1)
        (Point3d.meters 3 7 9)
    --> Point3d.meters 2 4 5

-}
midpoint : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
midpoint (Types.Point3d p1) (Types.Point3d p2) =
    Types.Point3d
        { x = p1.x + 0.5 * (p2.x - p1.x)
        , y = p1.y + 0.5 * (p2.y - p1.y)
        , z = p1.z + 0.5 * (p2.z - p1.z)
        }


{-| Find the centroid (average) of one or more points, by passing the first
point and then all remaining points. This allows this function to return a
`Point3d` instead of a `Maybe Point3d`. You would generally use `centroid`
within a `case` expression:

    case points of
        [] ->
            -- some default behavior

        first :: rest ->
            let
                centroid =
                    Point3d.centroid first rest
            in
            ...

Alternatively, you can use [`centroidN`](#centroidN) instead.

-}
centroid : Point3d units coordinates -> List (Point3d units coordinates) -> Point3d units coordinates
centroid (Types.Point3d p0) rest =
    centroidHelp p0.x p0.y p0.z 1 0 0 0 rest


centroidHelp : Float -> Float -> Float -> Float -> Float -> Float -> Float -> List (Point3d units coordinates) -> Point3d units coordinates
centroidHelp x0 y0 z0 count dx dy dz points =
    case points of
        (Types.Point3d p) :: remaining ->
            centroidHelp
                x0
                y0
                z0
                (count + 1)
                (dx + (p.x - x0))
                (dy + (p.y - y0))
                (dz + (p.z - z0))
                remaining

        [] ->
            Types.Point3d
                { x = x0 + dx / count
                , y = y0 + dy / count
                , z = z0 + dz / count
                }


{-| Like `centroid`, but lets you work with any kind of data as long as a point
can be extracted/constructed from it. For example, to get the centroid of a
bunch of vertices:

    type alias Vertex =
        { position : Point3d Meters World
        , color : Color
        , id : Int
        }

    vertexCentroid =
        Point3d.centroidOf .position
            firstVertex
            [ secondVertex
            , thirdVertex
            ]

-}
centroidOf : (a -> Point3d units coordinates) -> a -> List a -> Point3d units coordinates
centroidOf toPoint first rest =
    let
        (Types.Point3d p0) =
            toPoint first
    in
    centroidOfHelp toPoint p0.x p0.y p0.z 1 0 0 0 rest


centroidOfHelp : (a -> Point3d units coordinates) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List a -> Point3d units coordinates
centroidOfHelp toPoint x0 y0 z0 count dx dy dz values =
    case values of
        next :: remaining ->
            let
                (Types.Point3d p) =
                    toPoint next
            in
            centroidOfHelp
                toPoint
                x0
                y0
                z0
                (count + 1)
                (dx + (p.x - x0))
                (dy + (p.y - y0))
                (dz + (p.z - z0))
                remaining

        [] ->
            Types.Point3d
                { x = x0 + dx / count
                , y = y0 + dy / count
                , z = z0 + dz / count
                }


{-| Find the centroid of three points;

    Point3d.centroid3 p1 p2 p3

is equivalent to

    Point3d.centroid p1 [ p2, p3 ]

but is more efficient.

-}
centroid3 : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
centroid3 (Types.Point3d p1) (Types.Point3d p2) (Types.Point3d p3) =
    Types.Point3d
        { x = p1.x + (p2.x - p1.x) / 3 + (p3.x - p1.x) / 3
        , y = p1.y + (p2.y - p1.y) / 3 + (p3.y - p1.y) / 3
        , z = p1.z + (p2.z - p1.z) / 3 + (p3.z - p1.z) / 3
        }


{-| Find the centroid of a list of _N_ points. If the list is empty, returns
`Nothing`. If you know you have at least one point, you can use
[`centroid`](#centroid) instead to avoid the `Maybe`.
-}
centroidN : List (Point3d units coordinates) -> Maybe (Point3d units coordinates)
centroidN points =
    case points of
        first :: rest ->
            Just (centroid first rest)

        [] ->
            Nothing


{-| Construct a point by interpolating from the first given point to the second,
based on a parameter that ranges from zero to one.

    startPoint =
        Point3d.meters 1 2 4

    endPoint =
        Point3d.meters 1 2 8

    Point3d.interpolateFrom startPoint endPoint 0.25
    --> Point3d.meters 1 2 5

Partial application may be useful:

    interpolatedPoint : Float -> Point3d
    interpolatedPoint =
        Point3d.interpolateFrom startPoint endPoint

    List.map interpolatedPoint [ 0, 0.5, 1 ]
    --> [ Point3d.meters 1 2 4
    --> , Point3d.meters 1 2 6
    --> , Point3d.meters 1 2 8
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedPoint -0.5
    --> Point3d.meters 1 2 2

    interpolatedPoint 1.25
    --> Point3d.meters 1 2 9

-}
interpolateFrom : Point3d units coordinates -> Point3d units coordinates -> Float -> Point3d units coordinates
interpolateFrom (Types.Point3d p1) (Types.Point3d p2) t =
    if t <= 0.5 then
        Types.Point3d
            { x = p1.x + t * (p2.x - p1.x)
            , y = p1.y + t * (p2.y - p1.y)
            , z = p1.z + t * (p2.z - p1.z)
            }

    else
        Types.Point3d
            { x = p2.x + (1 - t) * (p1.x - p2.x)
            , y = p2.y + (1 - t) * (p1.y - p2.y)
            , z = p2.z + (1 - t) * (p1.z - p2.z)
            }


{-| Construct a point along an axis at a particular distance from the axis'
origin point.

    Point3d.along Axis3d.z (Length.meters 2)
    --> Point3d.meters 0 0 2

Positive and negative distances are interpreted relative to the direction of the
axis:

    horizontalAxis =
        Axis3d.withDirection Direction3d.negativeX
            (Point3d.meters 1 1 1)

    Point3d.along horizontalAxis (Length.meters 3)
    --> Point3d.meters -2 1 1

    Point3d.along horizontalAxis (Length.meters -3)
    --> Point3d.meters 4 1 1

-}
along : Axis3d units coordinates -> Quantity Float units -> Point3d units coordinates
along (Types.Axis3d axis) (Quantity distance) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction
    in
    Types.Point3d
        { x = p0.x + distance * d.x
        , y = p0.y + distance * d.y
        , z = p0.z + distance * d.z
        }


{-| Construct a 3D point lying _on_ a sketch plane by providing a 2D point
specified in XY coordinates _within_ the sketch plane.

    Point3d.on SketchPlane3d.xy (Point2d.meters 2 1)
    --> Point3d.meters 2 1 0

    Point3d.on SketchPlane3d.xz (Point2d.meters 2 1)
    --> Point3d.meters 2 0 1

The sketch plane can have any position and orientation:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x
                (Angle.degrees 45)
            |> SketchPlane3d.moveTo
                (Point3d.meters 10 10 10)

    Point3d.on tiltedSketchPlane (Point2d.meters 2 1)
    --> Point3d.meters 12 10.7071 10.7071

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Point2d units coordinates2d -> Point3d units coordinates3d
on (Types.SketchPlane3d sketchPlane) (Types.Point2d p) =
    let
        (Types.Point3d p0) =
            sketchPlane.originPoint

        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection
    in
    Types.Point3d
        { x = p0.x + p.x * i.x + p.y * j.x
        , y = p0.y + p.x * i.y + p.y * j.y
        , z = p0.z + p.x * i.z + p.y * j.z
        }


{-| Construct a 3D point lying on a sketch plane by providing its 2D coordinates
within that sketch plane:

    Point3d.xyOn SketchPlane3d.xy
        (Length.meters 2)
        (Length.meters 1)
    --> Point3d.meters 2 1 0

    Point3d.xyOn SketchPlane3d.xz
        (Length.meters 2)
        (Length.meters 1)
    --> Point3d.meters 2 0 1

-}
xyOn : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Quantity Float units -> Quantity Float units -> Point3d units coordinates
xyOn (Types.SketchPlane3d sketchPlane) (Quantity x) (Quantity y) =
    let
        (Types.Point3d p0) =
            sketchPlane.originPoint

        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection
    in
    Types.Point3d
        { x = p0.x + x * i.x + y * j.x
        , y = p0.y + x * i.y + y * j.y
        , z = p0.z + x * i.z + y * j.z
        }


{-| Construct a 3D point lying on a sketch plane by providing its 2D polar
coordinates within that sketch plane:

    Point3d.rThetaOn SketchPlane3d.xy
        (Length.meters 2)
        (Angle.degrees 45)
    --> Point3d.meters 1.4142 1.4142 0

    Point3d.rThetaOn SketchPlane3d.yz
        (Length.meters 2)
        (Angle.degrees 30)
    --> Point3d.meters 0 1.732 1

-}
rThetaOn : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Quantity Float units -> Angle -> Point3d units coordinates
rThetaOn (Types.SketchPlane3d sketchPlane) (Quantity r) (Quantity theta) =
    let
        (Types.Point3d p0) =
            sketchPlane.originPoint

        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        x =
            r * cos theta

        y =
            r * sin theta
    in
    Types.Point3d
        { x = p0.x + x * i.x + y * j.x
        , y = p0.y + x * i.y + y * j.y
        , z = p0.z + x * i.z + y * j.z
        }


{-| Construct a point given its local coordinates within a particular frame:

    frame =
        Frame3d.atPoint (Point3d.meters 1 1 1)

    Point3d.xyzIn frame
        (Length.meters 1)
        (Length.meters 2)
        (Length.meters 3)
    --> Point3d.meters 2 3 4

-}
xyzIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Point3d units globalCoordinates
xyzIn (Types.Frame3d frame) (Quantity x) (Quantity y) (Quantity z) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Point3d
        { x = p0.x + x * i.x + y * j.x + z * k.x
        , y = p0.y + x * i.y + y * j.y + z * k.y
        , z = p0.z + x * i.z + y * j.z + z * k.z
        }


{-| Attempt to find the circumcenter of three points; this is the center of the
circle that passes through all three points. If the three given points are
collinear, returns `Nothing`.

    Point3d.circumcenter
        (Point3d.meters 1 0 0)
        (Point3d.meters 0 1 0)
        (Point3d.meters 0 0 1)
    --> Just (Point3d.meters 0.33 0.33 0.33)

    Point3d.circumcenter
        Point3d.origin
        (Point3d.meters 1 0 0)
        (Point3d.meters 2 0 0)
    --> Nothing

-}
circumcenter : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Maybe (Point3d units coordinates)
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


circumenterHelp : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Float -> Float -> Float -> Maybe (Point3d units coordinates)
circumenterHelp (Types.Point3d p1) (Types.Point3d p2) (Types.Point3d p3) a b c =
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

            bz =
                p3.z - p2.z

            cx =
                p1.x - p3.x

            cy =
                p1.y - p3.y

            cz =
                p1.z - p3.z

            crossX =
                (by * cz - bz * cy) / bc

            crossY =
                (bz * cx - bx * cz) / bc

            crossZ =
                (bx * cy - by * cx) / bc

            sinA =
                sqrt (crossX * crossX + crossY * crossY + crossZ * crossZ)
        in
        if sinA == 0 then
            Nothing

        else
            let
                ax =
                    p2.x - p1.x

                ay =
                    p2.y - p1.y

                az =
                    p2.z - p1.z

                cosA =
                    (bx * cx + by * cy + bz * cz) / bc

                scale =
                    cosA / (2 * sinA * sinA)

                offsetX =
                    scale * (ay * crossZ - az * crossY)

                offsetY =
                    scale * (az * crossX - ax * crossZ)

                offsetZ =
                    scale * (ax * crossY - ay * crossX)
            in
            Just <|
                Types.Point3d
                    { x = p1.x + 0.5 * ax + offsetX
                    , y = p1.y + 0.5 * ay + offsetY
                    , z = p1.z + 0.5 * az + offsetZ
                    }


{-| Construct a `Point3d` from a tuple of `Float` values, by specifying what units those values are
in.

    Point3d.fromTuple Length.meters ( 2, 3, 1 )
    --> Point3d.meters 2 3 1

-}
fromTuple : (Float -> Quantity Float units) -> ( Float, Float, Float ) -> Point3d units coordinates
fromTuple toQuantity ( x, y, z ) =
    xyz (toQuantity x) (toQuantity y) (toQuantity z)


{-| Convert a `Point3d` to a tuple of `Float` values, by specifying what units you want the result
to be in.

    point =
        Point3d.feet 2 3 1

    Point3d.toTuple Length.inInches point
    --> ( 24, 36, 12 )

-}
toTuple : (Quantity Float units -> Float) -> Point3d units coordinates -> ( Float, Float, Float )
toTuple fromQuantity point =
    ( fromQuantity (xCoordinate point)
    , fromQuantity (yCoordinate point)
    , fromQuantity (zCoordinate point)
    )


{-| Construct a `Point3d` from a record with `Float` fields, by specifying what
units those fields are in.

    Point3d.fromRecord Length.inches
        { x = 24, y = 36, z = 12 }
    --> Point3d.feet 2 3 1

-}
fromRecord : (Float -> Quantity Float units) -> { x : Float, y : Float, z : Float } -> Point3d units coordinates
fromRecord toQuantity { x, y, z } =
    xyz (toQuantity x) (toQuantity y) (toQuantity z)


{-| Convert a `Point3d` to a record with `Float` fields, by specifying what units you want the
result to be in.

    point =
        Point3d.meters 2 3 1

    Point3d.toRecord Length.inCentimeters point
    --> { x = 200, y = 300, z = 100 }

-}
toRecord : (Quantity Float units -> Float) -> Point3d units coordinates -> { x : Float, y : Float, z : Float }
toRecord fromQuantity point =
    { x = fromQuantity (xCoordinate point)
    , y = fromQuantity (yCoordinate point)
    , z = fromQuantity (zCoordinate point)
    }


{-| -}
fromMeters : { x : Float, y : Float, z : Float } -> Point3d Meters coordinates
fromMeters givenCoordinates =
    Types.Point3d givenCoordinates


{-| -}
toMeters : Point3d Meters coordinates -> { x : Float, y : Float, z : Float }
toMeters (Types.Point3d pointCoordinates) =
    pointCoordinates


{-| -}
fromPixels : { x : Float, y : Float, z : Float } -> Point3d Pixels coordinates
fromPixels givenCoordinates =
    Types.Point3d givenCoordinates


{-| -}
toPixels : Point3d Pixels coordinates -> { x : Float, y : Float, z : Float }
toPixels (Types.Point3d pointCoordinates) =
    pointCoordinates


{-| -}
fromUnitless : { x : Float, y : Float, z : Float } -> Point3d Unitless coordinates
fromUnitless givenCoordinates =
    Types.Point3d givenCoordinates


{-| -}
toUnitless : Point3d Unitless coordinates -> { x : Float, y : Float, z : Float }
toUnitless (Types.Point3d pointCoordinates) =
    pointCoordinates


{-| Convert a point from one units type to another, by providing a conversion factor given as a
rate of change of destination units with respect to source units.

    worldPoint =
        Point3d.meters 2 3 1

    resolution : Quantity Float (Rate Pixels Meters)
    resolution =
        Pixels.pixels 100 |> Quantity.per (Length.meters 1)

    worldPoint |> Point3d.at resolution
    --> Point3d.pixels 200 300 100

-}
at : Quantity Float (Rate destinationUnits sourceUnits) -> Point3d sourceUnits coordinates -> Point3d destinationUnits coordinates
at (Quantity rate) (Types.Point3d p) =
    Types.Point3d
        { x = rate * p.x
        , y = rate * p.y
        , z = rate * p.z
        }


{-| Convert a point from one units type to another, by providing an 'inverse' conversion factor
given as a rate of change of source units with respect to destination units.

    screenPoint =
        Point3d.pixels 200 300 100

    resolution : Quantity Float (Rate Pixels Meters)
    resolution =
        Pixels.pixels 50 |> Quantity.per (Length.meters 1)

    screenPoint |> Point3d.at_ resolution
    --> Point3d.meters 4 6 2

-}
at_ : Quantity Float (Rate sourceUnits destinationUnits) -> Point3d sourceUnits coordinates -> Point3d destinationUnits coordinates
at_ (Quantity rate) (Types.Point3d p) =
    Types.Point3d
        { x = p.x / rate
        , y = p.y / rate
        , z = p.z / rate
        }


{-| Find the X coordinate of a point relative to a given frame.
-}
xCoordinateIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Point3d units globalCoordinates -> Quantity Float units
xCoordinateIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d d) =
            frame.xDirection
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z)


{-| Find the Y coordinate of a point relative to a given frame.
-}
yCoordinateIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Point3d units globalCoordinates -> Quantity Float units
yCoordinateIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d d) =
            frame.yDirection
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z)


{-| Find the Z coordinate of a point relative to a given frame.
-}
zCoordinateIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Point3d units globalCoordinates -> Quantity Float units
zCoordinateIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d d) =
            frame.zDirection
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z)


{-| Get the X, Y and Z coordinates of a point as a tuple.

    Point3d.coordinates (Point3d.meters 2 3 1)
    --> ( Length.meters 2
    --> , Length.meters 3
    --> , Length.meters 1
    --> )

-}
coordinates :
    Point3d units coordinates
    -> ( Quantity Float units, Quantity Float units, Quantity Float units )
coordinates (Types.Point3d p) =
    ( Quantity p.x, Quantity p.y, Quantity p.z )


{-| Get the X, Y and Z coordinates of a point relative to a given frame, as a
tuple; these are the coordinates the point would have as viewed by an observer
in that frame.
-}
coordinatesIn :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Point3d units globalCoordinates
    -> ( Quantity Float units, Quantity Float units, Quantity Float units )
coordinatesIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d dx) =
            frame.xDirection

        (Types.Direction3d dy) =
            frame.yDirection

        (Types.Direction3d dz) =
            frame.zDirection

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z
    in
    ( Quantity (deltaX * dx.x + deltaY * dx.y + deltaZ * dx.z)
    , Quantity (deltaX * dy.x + deltaY * dy.y + deltaZ * dy.z)
    , Quantity (deltaX * dz.x + deltaY * dz.y + deltaZ * dz.z)
    )


{-| Get the X coordinate of a point.

    Point3d.xCoordinate (Point3d.meters 2 1 3)
    --> Length.meters 2

-}
xCoordinate : Point3d units coordinates -> Quantity Float units
xCoordinate (Types.Point3d p) =
    Quantity p.x


{-| Get the Y coordinate of a point.

    Point3d.yCoordinate (Point3d.meters 2 1 3)
    --> Length.meters 1

-}
yCoordinate : Point3d units coordinates -> Quantity Float units
yCoordinate (Types.Point3d p) =
    Quantity p.y


{-| Get the Z coordinate of a point.

    Point3d.zCoordinate (Point3d.meters 2 1 3)
    --> Length.meters 3

-}
zCoordinate : Point3d units coordinates -> Quantity Float units
zCoordinate (Types.Point3d p) =
    Quantity p.z


{-| Compare two points within a tolerance. Returns true if the distance
between the two given points is less than the given tolerance.

    firstPoint =
        Point3d.meters 2 1 3

    secondPoint =
        Point3d.meters 2.0002 0.9999 3.0001

    Point3d.equalWithin (Length.millimeters 1)
        firstPoint
        secondPoint
    --> True

    Point3d.equalWithin (Length.microns 1)
        firstPoint
        secondPoint
    --> False

-}
equalWithin : Quantity Float units -> Point3d units coordinates -> Point3d units coordinates -> Bool
equalWithin (Quantity eps) (Types.Point3d p1) (Types.Point3d p2) =
    if eps > 0 then
        let
            nx =
                (p2.x - p1.x) / eps

            ny =
                (p2.y - p1.y) / eps

            nz =
                (p2.z - p1.z) / eps
        in
        nx * nx + ny * ny + nz * nz <= 1

    else if eps == 0 then
        p1.x == p2.x && p1.y == p2.y && p1.z == p2.z

    else
        False


{-| Compare two `Point3d` values lexicographically: first by X coordinate, then
by Y, then by Z. Can be used to provide a sort order for `Point3d` values.
-}
lexicographicComparison : Point3d units coordinates -> Point3d units coordinates -> Order
lexicographicComparison (Types.Point3d p1) (Types.Point3d p2) =
    if p1.x /= p2.x then
        compare p1.x p2.x

    else if p1.y /= p2.y then
        compare p1.y p2.y

    else
        compare p1.z p2.z


{-| Find the distance from the first point to the second.

    Point3d.distanceFrom
        (Point3d.meters 1 1 2)
        (Point3d.meters 2 3 4)
    --> Length.meters 3

Partial application can be useful:

    points =
        [ Point3d.meters 3 4 5
        , Point3d.meters 10 10 10
        , Point3d.meters -1 2 -3
        ]

    points
        |> Quantity.sortBy
            (Point3d.distanceFrom Point3d.origin)
    --> [ Point3d.meters -1 2 -3
    --> , Point3d.meters 3 4 5
    --> , Point3d.meters 10 10 10
    --> ]

-}
distanceFrom : Point3d units coordinates -> Point3d units coordinates -> Quantity Float units
distanceFrom (Types.Point3d p1) (Types.Point3d p2) =
    let
        deltaX =
            p2.x - p1.x

        deltaY =
            p2.y - p1.y

        deltaZ =
            p2.z - p1.z

        largestComponent =
            max (abs deltaX) (max (abs deltaY) (abs deltaZ))
    in
    if largestComponent == 0 then
        Quantity.zero

    else
        let
            scaledX =
                deltaX / largestComponent

            scaledY =
                deltaY / largestComponent

            scaledZ =
                deltaZ / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Quantity (scaledLength * largestComponent)


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis3d.withDirection Direction3d.x
            (Point3d.meters 1 0 0)

    Point3d.signedDistanceAlong axis (Point3d.meters 3 3 3)
    --> Length.meters 2

    Point3d.signedDistanceAlong axis Point3d.origin
    --> Length.meters -1

-}
signedDistanceAlong : Axis3d units coordinates -> Point3d units coordinates -> Quantity Float units
signedDistanceAlong (Types.Axis3d axis) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction
    in
    Quantity ((p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z)


{-| Find the perpendicular (nearest) distance of a point from an axis.

    point =
        Point3d.meters -3 4 0

    Point3d.distanceFromAxis Axis3d.x point
    --> Length.meters 4

    Point3d.distanceFromAxis Axis3d.y point
    --> Length.meters 3

    Point3d.distanceFromAxis Axis3d.z point
    --> Length.meters 5

Note that unlike in 2D, the result is always positive (unsigned) since there is
no such thing as the left or right side of an axis in 3D.

-}
distanceFromAxis : Axis3d units coordinates -> Point3d units coordinates -> Quantity Float units
distanceFromAxis (Types.Axis3d axis) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z

        projection =
            deltaX * d.x + deltaY * d.y + deltaZ * d.z

        perpX =
            deltaX - projection * d.x

        perpY =
            deltaY - projection * d.y

        perpZ =
            deltaZ - projection * d.z

        largestComponent =
            max (abs perpX) (max (abs perpY) (abs perpZ))
    in
    if largestComponent == 0 then
        Quantity.zero

    else
        let
            scaledX =
                perpX / largestComponent

            scaledY =
                perpY / largestComponent

            scaledZ =
                perpZ / largestComponent

            scaledDistance =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Quantity (scaledDistance * largestComponent)


{-| Find the perpendicular distance of a point from a plane. The result will be
positive if the point is 'above' the plane and negative if it is 'below', with
'up' defined by the normal direction of the plane.

    plane =
        Plane3d.withNormalDirection Direction3d.y
            (Point3d.meters 1 2 3)

    Point3d.signedDistanceFrom plane (Point3d.meters 3 3 3)
    --> Length.meters 1

    Point3d.signedDistanceFrom plane Point3d.origin
    --> Length.meters -2

This means that flipping a plane (reversing its normal direction) will also flip
the sign of the result of this function:

    flippedPlane =
        Plane3d.reverseNormal plane

    Point3d.signedDistanceFrom flippedPlane point
    --> Length.meters -1

    Point3d.signedDistanceFrom flippedPlane Point3d.origin
    --> Length.meters 2

-}
signedDistanceFrom : Plane3d units coordinates -> Point3d units coordinates -> Quantity Float units
signedDistanceFrom (Types.Plane3d plane) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            plane.originPoint

        (Types.Direction3d n) =
            plane.normalDirection
    in
    Quantity ((p.x - p0.x) * n.x + (p.y - p0.y) * n.y + (p.z - p0.z) * n.z)


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    centerPoint =
        Point3d.meters 1 1 1

    point =
        Point3d.meters 1 2 3

    Point3d.scaleAbout centerPoint 3 point
    --> Point3d.meters 1 4 7

    Point3d.scaleAbout centerPoint 0.5 point
    --> Point3d.meters 1 1.5 2

Avoid scaling by a negative scaling factor - while this may sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.

-}
scaleAbout : Point3d units coordinates -> Float -> Point3d units coordinates -> Point3d units coordinates
scaleAbout (Types.Point3d p0) k (Types.Point3d p) =
    Types.Point3d
        { x = p0.x + k * (p.x - p0.x)
        , y = p0.y + k * (p.y - p0.y)
        , z = p0.z + k * (p.z - p0.z)
        }


{-| Rotate a point around an axis by a given angle.

    axis =
        Axis3d.x

    angle =
        Angle.degrees 45

    point =
        Point3d.meters 3 1 0

    Point3d.rotateAround axis angle point
    --> Point3d.meters 3 0.7071 0.7071

Rotation direction is given by the right-hand rule, counterclockwise around the
direction of the axis.

-}
rotateAround : Axis3d units coordinates -> Angle -> Point3d units coordinates -> Point3d units coordinates
rotateAround (Types.Axis3d axis) (Quantity angle) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction

        halfAngle =
            0.5 * angle

        sinHalfAngle =
            sin halfAngle

        qx =
            d.x * sinHalfAngle

        qy =
            d.y * sinHalfAngle

        qz =
            d.z * sinHalfAngle

        qw =
            cos halfAngle

        wx =
            qw * qx

        wy =
            qw * qy

        wz =
            qw * qz

        xx =
            qx * qx

        xy =
            qx * qy

        xz =
            qx * qz

        yy =
            qy * qy

        yz =
            qy * qz

        zz =
            qz * qz

        a00 =
            1 - 2 * (yy + zz)

        a10 =
            2 * (xy + wz)

        a20 =
            2 * (xz - wy)

        a01 =
            2 * (xy - wz)

        a11 =
            1 - 2 * (xx + zz)

        a21 =
            2 * (yz + wx)

        a02 =
            2 * (xz + wy)

        a12 =
            2 * (yz - wx)

        a22 =
            1 - 2 * (xx + yy)

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z
    in
    Types.Point3d
        { x = p0.x + a00 * deltaX + a01 * deltaY + a02 * deltaZ
        , y = p0.y + a10 * deltaX + a11 * deltaY + a12 * deltaZ
        , z = p0.z + a20 * deltaX + a21 * deltaY + a22 * deltaZ
        }


{-| Translate a point by a given displacement.

    point =
        Point3d.meters 3 4 5

    displacement =
        Vector3d.meters 1 2 3

    Point3d.translateBy displacement point
    --> Point3d.meters 4 6 8

In more mathematical terms, this is 'point plus vector'. For 'point minus point'
(giving the vector from one point to another), there is [`Vector3d.from`](Vector3d#from).

-}
translateBy : Vector3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
translateBy (Types.Vector3d v) (Types.Point3d p) =
    Types.Point3d
        { x = p.x + v.x
        , y = p.y + v.y
        , z = p.z + v.z
        }


{-| Translate a point in a given direction by a given distance.

    point =
        Point3d.meters 3 4 5

    point
        |> Point3d.translateIn Direction3d.x
            (Length.meters 2)
    --> Point3d.meters 5 4 5

    point
        |> Point3d.translateIn Direction3d.y
            (Length.meters 2)
    --> Point3d.meters 3 6 5

The distance can be negative:

    point
        |> Point3d.translateIn Direction3d.x
            (Length.meters -2)
    --> Point3d.meters 1 4 5

-}
translateIn : Direction3d coordinates -> Quantity Float units -> Point3d units coordinates -> Point3d units coordinates
translateIn (Types.Direction3d d) (Quantity distance) (Types.Point3d p) =
    Types.Point3d
        { x = p.x + distance * d.x
        , y = p.y + distance * d.y
        , z = p.z + distance * d.z
        }


{-| Mirror a point across a plane. The result will be the same distance from the
plane but on the opposite side.

    point =
        Point3d.meters 1 2 3

    -- Plane3d.xy is the plane Z=0
    Point3d.mirrorAcross Plane3d.xy point
    --> Point3d.meters 1 2 -3

    -- Plane3d.yz is the plane X=0
    Point3d.mirrorAcross Plane3d.yz point
    --> Point3d.meters -1 2 3

The plane does not have to pass through the origin:

    -- offsetPlane is the plane  Z=1
    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    -- The origin point is 1 unit below the offset
    -- plane, so its mirrored copy is one unit above
    Point3d.mirrorAcross offsetPlane Point3d.origin
    --> Point3d.meters 0 0 2

-}
mirrorAcross : Plane3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
mirrorAcross (Types.Plane3d plane) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            plane.originPoint

        (Types.Direction3d n) =
            plane.normalDirection

        a00 =
            1 - 2 * n.x * n.x

        a11 =
            1 - 2 * n.y * n.y

        a22 =
            1 - 2 * n.z * n.z

        a12 =
            -2 * n.y * n.z

        a02 =
            -2 * n.x * n.z

        a01 =
            -2 * n.x * n.y

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z
    in
    Types.Point3d
        { x = p0.x + a00 * deltaX + a01 * deltaY + a02 * deltaZ
        , y = p0.y + a01 * deltaX + a11 * deltaY + a12 * deltaZ
        , z = p0.z + a02 * deltaX + a12 * deltaY + a22 * deltaZ
        }


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a point onto a plane:

    point =
        Point3d.meters 1 2 3

    Point3d.projectOnto Plane3d.xy point
    --> Point3d.meters 1 2 0

    Point3d.projectOnto Plane3d.yz point
    --> Point3d.meters 0 2 3

The plane does not have to pass through the origin:

    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    Point3d.projectOnto offsetPlane point
    --> Point3d.meters 1 2 1

-}
projectOnto : Plane3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
projectOnto (Types.Plane3d plane) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            plane.originPoint

        (Types.Direction3d n) =
            plane.normalDirection

        distance =
            (p.x - p0.x) * n.x + (p.y - p0.y) * n.y + (p.z - p0.z) * n.z
    in
    Types.Point3d
        { x = p.x - distance * n.x
        , y = p.y - distance * n.y
        , z = p.z - distance * n.z
        }


{-| Project a point perpendicularly onto an axis.

    point =
        Point3d.meters 1 2 3

    Point3d.projectOntoAxis Axis3d.x
    --> Point3d.meters 1 0 0

    verticalAxis =
        Axis3d.withDirection Direction3d.z
            (Point3d.meters 0 1 2)

    Point3d.projectOntoAxis verticalAxis
    --> Point3d.meters 0 1 3

-}
projectOntoAxis : Axis3d units coordinates -> Point3d units coordinates -> Point3d units coordinates
projectOntoAxis (Types.Axis3d axis) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            axis.originPoint

        (Types.Direction3d d) =
            axis.direction

        distance =
            (p.x - p0.x) * d.x + (p.y - p0.y) * d.y + (p.z - p0.z) * d.z
    in
    Types.Point3d
        { x = p0.x + distance * d.x
        , y = p0.y + distance * d.y
        , z = p0.z + distance * d.z
        }


{-| Take a point defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint (Point3d.meters 1 2 3)

    Point3d.relativeTo localFrame (Point3d.meters 4 5 6)
    --> Point3d.meters 3 3 3

    Point3d.relativeTo localFrame (Point3d.meters 1 1 1)
    --> Point3d.meters 0 -1 -2

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Point3d units globalCoordinates -> Point3d units localCoordinates
relativeTo (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z
    in
    Types.Point3d
        { x = deltaX * i.x + deltaY * i.y + deltaZ * i.z
        , y = deltaX * j.x + deltaY * j.y + deltaZ * j.z
        , z = deltaX * k.x + deltaY * k.y + deltaZ * k.z
        }


{-| Take a point defined in local coordinates relative to a given reference
frame, and return that point expressed in global coordinates.

    localFrame =
        Frame3d.atPoint (Point3d.meters 1 2 3)

    Point3d.placeIn localFrame (Point3d.meters 3 3 3)
    --> Point3d.meters 4 5 6

    Point3d.placeIn localFrame (Point3d.meters 0 -1 -2)
    --> Point3d.meters 1 1 1

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Point3d units localCoordinates -> Point3d units globalCoordinates
placeIn (Types.Frame3d frame) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            frame.originPoint

        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Point3d
        { x = p0.x + p.x * i.x + p.y * j.x + p.z * k.x
        , y = p0.y + p.x * i.y + p.y * j.y + p.z * k.y
        , z = p0.z + p.x * i.z + p.y * j.z + p.z * k.z
        }


{-| Project a point into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the point onto the plane and then expresses the projected point in 2D sketch
coordinates.

    point =
        Point3d.meters 2 1 3

    Point3d.projectInto SketchPlane3d.xy point
    --> Point2d.meters 2 1

    Point3d.projectInto SketchPlane3d.yz point
    --> Point2d.meters 1 3

    Point3d.projectInto SketchPlane3d.zx point
    --> Point2d.meters 3 2

-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Point3d units coordinates3d -> Point2d units coordinates2d
projectInto (Types.SketchPlane3d sketchPlane) (Types.Point3d p) =
    let
        (Types.Point3d p0) =
            sketchPlane.originPoint

        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        deltaX =
            p.x - p0.x

        deltaY =
            p.y - p0.y

        deltaZ =
            p.z - p0.z
    in
    Types.Point2d
        { x = deltaX * i.x + deltaY * i.y + deltaZ * i.z
        , y = deltaX * j.x + deltaY * j.y + deltaZ * j.z
        }
