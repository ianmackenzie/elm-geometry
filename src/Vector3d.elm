--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Vector3d exposing
    ( Vector3d
    , zero
    , unitless
    , meters, pixels, millimeters, centimeters, inches, feet
    , xyz, xyzIn, from, withLength, on, xyOn, rThetaOn, perpendicularTo, interpolateFrom
    , fromTuple, toTuple, fromRecord, toRecord
    , fromMeters, toMeters, fromPixels, toPixels, fromUnitless, toUnitless
    , per, for
    , components, xComponent, yComponent, zComponent, componentIn, length, direction
    , equalWithin, lexicographicComparison
    , plus, minus, dot, cross, sum, twice, half
    , product, times, over, over_
    , reverse, normalize, scaleBy, scaleTo, rotateAround, mirrorAcross, projectionIn, projectOnto
    , at, at_
    , relativeTo, placeIn, projectInto
    , metersPerSecond, feetPerSecond, kilometersPerHour, milesPerHour
    , metersPerSecondSquared, feetPerSecondSquared, gees
    , newtons, kilonewtons, meganewtons, pounds, kips
    , unsafe, unwrap
    )

{-| A `Vector3d` represents a quantity such as a displacement or velocity in 3D,
and is defined by its X, Y and Z components. This module contains a variety of
vector-related functionality, such as

  - Adding or subtracting vectors
  - Finding the lengths of vectors
  - Rotating vectors
  - Converting vectors between different coordinate systems

Note that unlike in many other geometry packages where vectors are used as a
general-purpose data type, `elm-geometry` has separate data types for vectors,
directions and points. In most code it is actually more common to use `Point3d`
and `Direction3d` than `Vector3d`, and much code can avoid working directly with
`Vector3d` values at all!

@docs Vector3d


# Predefined vectors

@docs zero

Although there are no predefined constants for the vectors with components
(1,0,0), (0,1,0) and (0,0,1), in most cases you will actually want their
`Direction3d` versions [`Direction3d.x`](Direction3d#x),
[`Direction3d.y`](Direction3d#y) and [`Direction3d.z`](Direction3d#z).


# Literals

@docs unitless

The remaining functions all construct a `Vector3d` from X, Y and Z components
given in specific units. Functions like `Vector3d.xyz` are more useful in
generic code, but these functions are useful for quickly creating hardcoded
constant values, e.g.

    vector =
        Vector3d.meters 2 3 1

These functions may also be useful when decoding vectors from JSON - for example
if you had some JSON where a vector was encoded as an object with `x`, `y` and
`z` fields measured in meters then you could write a decoder like

    Decode.map3 Vector3d.meters
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "z" Decode.float)

@docs meters, pixels, millimeters, centimeters, inches, feet

There are some [additional constructors](#physics) below for vectors with
physics-related units (speed, acceleration and force).


# Constructors

@docs xyz, xyzIn, from, withLength, on, xyOn, rThetaOn, perpendicularTo, interpolateFrom


# Interop

These functions are useful for interoperability with other Elm code that uses
plain `Float` tuples or records to represent vectors.

@docs fromTuple, toTuple, fromRecord, toRecord


## Zero-copy conversions

These functions allow zero-overhead conversion of vectors to and from records
with `x`, `y` and `z` `Float` fields, useful for efficient interop with other
code that represents vectors as plain records.

@docs fromMeters, toMeters, fromPixels, toPixels, fromUnitless, toUnitless


# Rates of change

@docs per, for


# Properties

@docs components, xComponent, yComponent, zComponent, componentIn, length, direction


# Comparison

@docs equalWithin, lexicographicComparison


# Arithmetic

@docs plus, minus, dot, cross, sum, twice, half


## Vector/scalar products

@docs product, times, over, over_


# Transformations

Note that for all transformations, only the orientation of the given axis or
plane is relevant, since vectors are position-independent. Think of transforming
a vector as placing its tail on the relevant axis or plane and then transforming
its tip.

@docs reverse, normalize, scaleBy, scaleTo, rotateAround, mirrorAcross, projectionIn, projectOnto


# Unit conversions

@docs at, at_


# Coordinate conversions

Like other transformations, coordinate transformations of vectors depend only on
the orientations of the relevant frames/sketch planes, not their positions.

For the examples, assume the following definition of a local coordinate frame,
one that is rotated 30 degrees counterclockwise around the Z axis from the
global XYZ frame:

    rotatedFrame =
        Frame3d.atOrigin |> Frame3d.rotateAround Axis3d.z (Angle.degrees 30)

@docs relativeTo, placeIn, projectInto


# Physics

These constructors let you conveniently create vectors with physics-related
units such as speed, acceleration and force. For example, a speed of 5 feet per
second in the positive Y direction could be written as

    Vector3d.feetPerSecond 0 5 0

and a force of 10 newtons in the negative X direction could be written as

    Vector3d.newtons -10 0 0


## Speed

@docs metersPerSecond, feetPerSecond, kilometersPerHour, milesPerHour


## Acceleration

@docs metersPerSecondSquared, feetPerSecondSquared, gees


## Force

@docs newtons, kilonewtons, meganewtons, pounds, kips


# Advanced

These functions are unsafe because they require you to track units manually. In
general you should prefer other functions instead, but these functions may be
useful when writing generic/library code.

@docs unsafe, unwrap

-}

import Acceleration exposing (MetersPerSecondSquared)
import Angle exposing (Angle)
import Float.Extra as Float
import Force exposing (Newtons)
import Geometry.Types as Types exposing (Axis3d, Direction3d, Frame3d, Plane3d, Point3d, SketchPlane3d)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Quantity exposing (Cubed, Product, Quantity(..), Rate, Squared, Unitless)
import Quantity.Extra as Quantity
import Speed exposing (MetersPerSecond)
import Vector2d exposing (Vector2d)


{-| -}
type alias Vector3d units coordinates =
    Types.Vector3d units coordinates


{-| Construct a vector from its raw X, Y and Z components as `Float` values. The
values must be in whatever units the resulting vector is considered to use
(usually meters or pixels). You should generally use something safer such as
[`meters`](#meters), [`fromPixels`](#fromPixels), [`xyz`](#xyz),
[`fromRecord`](#fromRecord) etc.
-}
unsafe : { x : Float, y : Float, z : Float } -> Vector3d units coordinates
unsafe givenComponents =
    Types.Vector3d givenComponents


{-| Extract a vector's raw X, Y and Z coordinates as `Float` values. These
values will be in whatever units the vector has (usually meters or pixels). You
should generally use something safer such as [`toMeters`](#toMeters),
[`toRecord`](#toRecord), [`xComponent`](#xComponent) etc.
-}
unwrap : Vector3d units coordinates -> { x : Float, y : Float, z : Float }
unwrap (Types.Vector3d givenComponents) =
    givenComponents


{-| The vector with components (0,0,0).
-}
zero : Vector3d units coordinates
zero =
    Types.Vector3d
        { x = 0
        , y = 0
        , z = 0
        }


{-| -}
millimeters : Float -> Float -> Float -> Vector3d Meters coordinates
millimeters x y z =
    xyz (Length.millimeters x) (Length.millimeters y) (Length.millimeters z)


{-| -}
centimeters : Float -> Float -> Float -> Vector3d Meters coordinates
centimeters x y z =
    xyz (Length.centimeters x) (Length.centimeters y) (Length.centimeters z)


{-| -}
meters : Float -> Float -> Float -> Vector3d Meters coordinates
meters x y z =
    Types.Vector3d { x = x, y = y, z = z }


{-| -}
inches : Float -> Float -> Float -> Vector3d Meters coordinates
inches x y z =
    xyz (Length.inches x) (Length.inches y) (Length.inches z)


{-| -}
feet : Float -> Float -> Float -> Vector3d Meters coordinates
feet x y z =
    xyz (Length.feet x) (Length.feet y) (Length.feet z)


{-| -}
pixels : Float -> Float -> Float -> Vector3d Pixels coordinates
pixels x y z =
    Types.Vector3d { x = x, y = y, z = z }


{-| Construct a unitless `Vector3d` value from its X, Y and Z components. See
also [`fromUnitless`](#fromUnitless).
-}
unitless : Float -> Float -> Float -> Vector3d Unitless coordinates
unitless x y z =
    Types.Vector3d { x = x, y = y, z = z }


{-| Construct a vector from its X, Y and Z components.

    vector =
        Vector3d.xyz
            (Length.meters 2)
            (Length.meters 1)
            (Length.meters 3)

-}
xyz : Quantity Float units -> Quantity Float units -> Quantity Float units -> Vector3d units coordinates
xyz (Quantity x) (Quantity y) (Quantity z) =
    Types.Vector3d
        { x = x
        , y = y
        , z = z
        }


{-| Construct a vector given its local components within a particular frame:

    frame =
        Frame3d.atOrigin
            |> Frame3d.rotateAround Axis3d.z
                (Angle.degrees 45)

    Vector3d.xyzIn frame
        (Speed.feetPerSecond 1)
        (Speed.feetPerSecond 0)
        (Speed.feetPerSecond 2)
    --> Vector3d.xyz
    -->     (Speed.feetPerSecond 0.7071)
    -->     (Speed.feetPerSecond 0.7071)
    -->     (Speed.feetPerSecond 2)

-}
xyzIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Vector3d units globalCoordinates
xyzIn (Types.Frame3d frame) (Quantity x) (Quantity y) (Quantity z) =
    let
        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Vector3d
        { x = x * i.x + y * j.x + z * k.x
        , y = x * i.y + y * j.y + z * k.y
        , z = x * i.z + y * j.z + z * k.z
        }


{-| Construct a vector from the first given point to the second.

    startPoint =
        Point3d.meters 1 1 1

    endPoint =
        Point3d.meters 4 5 6

    Vector3d.from startPoint endPoint
    --> Vector3d.meters 3 4 5

-}
from : Point3d units coordinates -> Point3d units coordinates -> Vector3d units coordinates
from (Types.Point3d p1) (Types.Point3d p2) =
    Types.Vector3d
        { x = p2.x - p1.x
        , y = p2.y - p1.y
        , z = p2.z - p1.z
        }


{-| Construct a vector with the given length in the given direction.

    Vector3d.withLength (Length.meters 5) Direction3d.y
    --> Vector3d.meters 0 5 0

-}
withLength : Quantity Float units -> Direction3d coordinates -> Vector3d units coordinates
withLength (Quantity a) (Types.Direction3d d) =
    Types.Vector3d
        { x = a * d.x
        , y = a * d.y
        , z = a * d.z
        }


{-| Construct a 3D vector lying _on_ a sketch plane by providing a 2D vector
specified in XY coordinates _within_ the sketch plane.

    vector2d =
        Vector2d.meters 2 3

    Vector3d.on SketchPlane3d.xy vector2d
    --> Vector3d.meters 2 3 0

    Vector3d.on SketchPlane3d.yz vector2d
    --> Vector3d.meters 0 2 3

    Vector3d.on SketchPlane3d.zx vector2d
    --> Vector3d.meters 3 0 2

A slightly more complex example:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x
                (Angle.degrees 45)

    Vector3d.on tiltedSketchPlane <|
        Vector2d.meters 1 1
    --> Vector3d.meters 1 0.7071 0.7071

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Vector2d units coordinates2d -> Vector3d units coordinates3d
on (Types.SketchPlane3d sketchPlane) (Types.Vector2d v) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection
    in
    Types.Vector3d
        { x = v.x * i.x + v.y * j.x
        , y = v.x * i.y + v.y * j.y
        , z = v.x * i.z + v.y * j.z
        }


{-| Construct a 3D vector lying on a sketch plane by providing its 2D components
within the sketch plane:

    Vector3d.xyOn SketchPlane3d.xy
        (Length.meters 2)
        (Length.meters 3)
    --> Vector3d.meters 2 3 0

    Vector3d.xyOn SketchPlane3d.zx
        (Length.meters 2)
        (Length.meters 3)
    --> Vector3d.meters 3 0 2

-}
xyOn : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Quantity Float units -> Quantity Float units -> Vector3d units coordinates3d
xyOn (Types.SketchPlane3d sketchPlane) (Quantity x) (Quantity y) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection
    in
    Types.Vector3d
        { x = x * i.x + y * j.x
        , y = x * i.y + y * j.y
        , z = x * i.z + y * j.z
        }


{-| Construct a 3D vector lying on a sketch plane by providing its 2D polar
components within the sketch plane:

    Vector3d.rThetaOn SketchPlane3d.xy
        (Length.meters 2)
        (Angle.degrees 45)
    --> Vector3d.meters 1.4142 1.4142 0

    Vector3d.rThetaOn SketchPlane3d.yz
        (Length.meters 2)
        (Angle.degrees 30)
    --> Vector3d.meters 0 1.732 1

-}
rThetaOn : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Quantity Float units -> Angle -> Vector3d units coordinates3d
rThetaOn (Types.SketchPlane3d sketchPlane) (Quantity r) (Quantity theta) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        x =
            r * cos theta

        y =
            r * sin theta
    in
    Types.Vector3d
        { x = x * i.x + y * j.x
        , y = x * i.y + y * j.y
        , z = x * i.z + y * j.z
        }


{-| Construct an arbitrary vector perpendicular to the given vector. The exact
length and direction of the resulting vector are not specified, but it is
guaranteed to be perpendicular to the given vector and non-zero (unless the
given vector is itself zero).

    Vector3d.perpendicularTo (Vector3d.meters 3 0 0)
    --> Vector3d.meters 0 0 -3

    Vector3d.perpendicularTo (Vector3d.meters 1 2 3)
    --> Vector3d.meters 0 -3 2

    Vector3d.perpendicularTo Vector3d.zero
    --> Vector3d.zero

-}
perpendicularTo : Vector3d units coordinates -> Vector3d units coordinates
perpendicularTo (Types.Vector3d v) =
    let
        absX =
            abs v.x

        absY =
            abs v.y

        absZ =
            abs v.z
    in
    if absX <= absY then
        if absX <= absZ then
            Types.Vector3d { x = 0, y = -v.z, z = v.y }

        else
            Types.Vector3d { x = -v.y, y = v.x, z = 0 }

    else if absY <= absZ then
        Types.Vector3d { x = v.z, y = 0, z = -v.x }

    else
        Types.Vector3d { x = -v.y, z = v.x, y = 0 }


{-| Construct a vector by interpolating from the first given vector to the
second, based on a parameter that ranges from zero to one.

    startVector =
        Vector3d.meters 1 2 4

    endVector =
        Vector3d.meters 1 3 8

    Vector3d.interpolateFrom startVector endVector 0.25
    --> Vector3d.meters 1 2.25 5

Partial application may be useful:

    interpolatedVector : Float -> Vector3d
    interpolatedVector =
        Vector3d.interpolateFrom startVector endVector

    List.map interpolatedVector [ 0, 0.5, 1 ]
    --> [ Vector3d.meters 1 2 4
    --> , Vector3d.meters 1 2 6
    --> , Vector3d.meters 1 2 8
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedVector -0.5
    --> Vector3d.meters 1 2 2

    interpolatedVector 1.25
    --> Vector3d.meters 1 2 9

-}
interpolateFrom : Vector3d units coordinates -> Vector3d units coordinates -> Float -> Vector3d units coordinates
interpolateFrom (Types.Vector3d v1) (Types.Vector3d v2) t =
    if t <= 0.5 then
        Types.Vector3d
            { x = v1.x + t * (v2.x - v1.x)
            , y = v1.y + t * (v2.y - v1.y)
            , z = v1.z + t * (v2.z - v1.z)
            }

    else
        Types.Vector3d
            { x = v2.x + (1 - t) * (v1.x - v2.x)
            , y = v2.y + (1 - t) * (v1.y - v2.y)
            , z = v2.z + (1 - t) * (v1.z - v2.z)
            }


{-| Construct a `Vector3d` from a tuple of `Float` values, by specifying what units those values are
in.

    Vector3d.fromTuple Length.meters ( 2, 3, 1 )
    --> Vector3d.meters 2 3 1

-}
fromTuple : (Float -> Quantity Float units) -> ( Float, Float, Float ) -> Vector3d units coordinates
fromTuple toQuantity ( x, y, z ) =
    xyz (toQuantity x) (toQuantity y) (toQuantity z)


{-| Convert a `Vector3d` to a tuple of `Float` values, by specifying what units you want the result
to be in.

    vector =
        Vector3d.feet 2 3 1

    Vector3d.toTuple Length.inInches vector
    --> ( 24, 36, 12 )

-}
toTuple : (Quantity Float units -> Float) -> Vector3d units coordinates -> ( Float, Float, Float )
toTuple fromQuantity vector =
    ( fromQuantity (xComponent vector)
    , fromQuantity (yComponent vector)
    , fromQuantity (zComponent vector)
    )


{-| Construct a `Vector3d` from a record with `Float` fields, by specifying what
units those fields are in.

    Vector3d.fromRecord Length.inches { x = 24, y = 36, z = 12 }
    --> Vector3d.feet 2 3 1

-}
fromRecord : (Float -> Quantity Float units) -> { x : Float, y : Float, z : Float } -> Vector3d units coordinates
fromRecord toQuantity { x, y, z } =
    xyz (toQuantity x) (toQuantity y) (toQuantity z)


{-| Convert a `Vector3d` to a record with `Float` fields, by specifying what units you want the
result to be in.

    vector =
        Vector3d.meters 2 3 1

    Vector3d.toRecord Length.inCentimeters vector
    --> { x = 200, y = 300, z = 100 }

-}
toRecord : (Quantity Float units -> Float) -> Vector3d units coordinates -> { x : Float, y : Float, z : Float }
toRecord fromQuantity vector =
    { x = fromQuantity (xComponent vector)
    , y = fromQuantity (yComponent vector)
    , z = fromQuantity (zComponent vector)
    }


{-| -}
fromMeters : { x : Float, y : Float, z : Float } -> Vector3d Meters coordinates
fromMeters givenComponents =
    Types.Vector3d givenComponents


{-| -}
toMeters : Vector3d Meters coordinates -> { x : Float, y : Float, z : Float }
toMeters (Types.Vector3d vectorComponents) =
    vectorComponents


{-| -}
fromPixels : { x : Float, y : Float, z : Float } -> Vector3d Pixels coordinates
fromPixels givenComponents =
    Types.Vector3d givenComponents


{-| -}
toPixels : Vector3d Pixels coordinates -> { x : Float, y : Float, z : Float }
toPixels (Types.Vector3d vectorComponents) =
    vectorComponents


{-| -}
fromUnitless : { x : Float, y : Float, z : Float } -> Vector3d Unitless coordinates
fromUnitless givenComponents =
    Types.Vector3d givenComponents


{-| -}
toUnitless : Vector3d Unitless coordinates -> { x : Float, y : Float, z : Float }
toUnitless (Types.Vector3d vectorComponents) =
    vectorComponents


{-| Convert a vector from one units type to another, by providing a conversion factor given as a
rate of change of destination units with respect to source units.

    worldVector =
        Vector3d.meters 2 3 1

    resolution : Quantity Float (Rate Pixels Meters)
    resolution =
        Pixels.pixels 100 |> Quantity.per (Length.meters 1)

    worldVector |> Vector3d.at resolution
    --> Vector3d.pixels 200 300 100

-}
at : Quantity Float (Rate destinationUnits sourceUnits) -> Vector3d sourceUnits coordinates -> Vector3d destinationUnits coordinates
at (Quantity rate) (Types.Vector3d v) =
    Types.Vector3d
        { x = rate * v.x
        , y = rate * v.y
        , z = rate * v.z
        }


{-| Convert a vector from one units type to another, by providing an 'inverse' conversion factor
given as a rate of change of source units with respect to destination units.

    screenVector =
        Vector3d.pixels 200 300 100

    resolution : Quantity Float (Rate Pixels Meters)
    resolution =
        Pixels.pixels 50 |> Quantity.per (Length.meters 1)

    screenVector |> Vector3d.at_ resolution
    --> Vector3d.meters 4 6 2

-}
at_ : Quantity Float (Rate sourceUnits destinationUnits) -> Vector3d sourceUnits coordinates -> Vector3d destinationUnits coordinates
at_ (Quantity rate) (Types.Vector3d v) =
    Types.Vector3d
        { x = v.x / rate
        , y = v.y / rate
        , z = v.z / rate
        }


{-| Construct a vector representing a rate of change such as a speed:

    displacement =
        Vector3d.meters 6 8 4

    displacement |> Vector3d.per (Duration.seconds 2)
    --> Vector3d.xyz
    -->     (Speed.metersPerSecond 3)
    -->     (Speed.metersPerSecond 4)
    -->     (Speed.metersPerSecond 2)

-}
per : Quantity Float independentUnits -> Vector3d dependentUnits coordinates -> Vector3d (Rate dependentUnits independentUnits) coordinates
per (Quantity a) (Types.Vector3d v) =
    Types.Vector3d
        { x = v.x / a
        , y = v.y / a
        , z = v.z / a
        }


{-| Multiply a rate of change vector by an independent quantity to get a total vector. For example,
multiply a velocity by a duration to get a total displacement:

    velocity =
        Vector3d.xy
            (Pixels.pixelsPerSecond 200)
            (Pixels.pixelsPerSecond 50)
            (Pixels.pixelsPerSecond 100)

    velocity |> Vector3d.for (Duration.seconds 0.1)
    --> Vector3d.pixels 20 5 10

-}
for : Quantity Float independentUnits -> Vector3d (Rate dependentUnits independentUnits) coordinates -> Vector3d dependentUnits coordinates
for (Quantity a) (Types.Vector3d v) =
    Types.Vector3d
        { x = v.x * a
        , y = v.y * a
        , z = v.z * a
        }


{-| Get the X, Y and Z components of a vector as a tuple.

    Vector3d.components (Vector3d.meters 2 3 1)
    --> ( Length.meters 2
    --> , Length.meters 3
    --> , Length.meters 1
    --> )

-}
components :
    Vector3d units coordinates
    -> ( Quantity Float units, Quantity Float units, Quantity Float units )
components (Types.Vector3d v) =
    ( Quantity v.x, Quantity v.y, Quantity v.z )


{-| Get the X component of a vector.

    Vector3d.xComponent (Vector3d.meters 1 2 3)
    --> Length.meters 1

-}
xComponent : Vector3d units coordinates -> Quantity Float units
xComponent (Types.Vector3d v) =
    Quantity v.x


{-| Get the Y component of a vector.

    Vector3d.yComponent (Vector3d.meters 1 2 3)
    --> Length.meters 2

-}
yComponent : Vector3d units coordinates -> Quantity Float units
yComponent (Types.Vector3d v) =
    Quantity v.y


{-| Get the Z component of a vector.

    Vector3d.zComponent (Vector3d.meters 1 2 3)
    --> Length.meters 3

-}
zComponent : Vector3d units coordinates -> Quantity Float units
zComponent (Types.Vector3d v) =
    Quantity v.z


{-| Find the component of a vector in an arbitrary direction, for example

    verticalSpeed =
        Vector3d.componentIn upDirection velocity

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`; for
example,

    Vector3d.zComponent vector

is equivalent to

    Vector3d.componentIn Direction3d.z vector

-}
componentIn : Direction3d coordinates -> Vector3d units coordinates -> Quantity Float units
componentIn (Types.Direction3d d) (Types.Vector3d v) =
    Quantity (v.x * d.x + v.y * d.y + v.z * d.z)


{-| Compare two vectors within a tolerance. Returns true if the difference
between the two given vectors has magnitude less than the given tolerance.

    firstVector =
        Vector3d.meters 2 1 3

    secondVector =
        Vector3d.meters 2.0002 0.9999 3.0001

    Vector3d.equalWithin (Length.millimeters 1)
        firstVector
        secondVector
    --> True

    Vector3d.equalWithin (Length.microns 1)
        firstVector
        secondVector
    --> False

-}
equalWithin : Quantity Float units -> Vector3d units coordinates -> Vector3d units coordinates -> Bool
equalWithin givenTolerance firstVector secondVector =
    length (secondVector |> minus firstVector) |> Quantity.lessThanOrEqualTo givenTolerance


{-| Compare two `Vector3d` values lexicographically: first by X component, then
by Y, then by Z. Can be used to provide a sort order for `Vector3d` values.
-}
lexicographicComparison : Vector3d units coordinates -> Vector3d units coordinates -> Order
lexicographicComparison (Types.Vector3d v1) (Types.Vector3d v2) =
    if v1.x /= v2.x then
        compare v1.x v2.x

    else if v1.y /= v2.y then
        compare v1.y v2.y

    else
        compare v1.z v2.z


{-| Get the length (magnitude) of a vector.

    Vector3d.length (Vector3d.meters 2 1 2)
    --> Length.meters 3

-}
length : Vector3d units coordinates -> Quantity Float units
length (Types.Vector3d v) =
    let
        largestComponent =
            max (abs v.x) (max (abs v.y) (abs v.z))
    in
    if largestComponent == 0 then
        Quantity.zero

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledZ =
                v.z / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Quantity (scaledLength * largestComponent)


{-| Attempt to find the direction of a vector. In the case of a zero vector,
returns `Nothing`.

    Vector3d.direction (Vector3d.meters 3 0 3)
    --> Just (Direction3d.xz (Angle.degrees 45))

    Vector3d.direction Vector3d.zero
    --> Nothing

-}
direction : Vector3d units coordinates -> Maybe (Direction3d coordinates)
direction (Types.Vector3d v) =
    let
        largestComponent =
            max (abs v.x) (max (abs v.y) (abs v.z))
    in
    if largestComponent == 0 then
        Nothing

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledZ =
                v.z / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Just <|
            Types.Direction3d
                { x = scaledX / scaledLength
                , y = scaledY / scaledLength
                , z = scaledZ / scaledLength
                }


{-| Normalize a vector to have a length of one. Zero vectors are left as-is.

    vector =
        Vector3d.meters 3 0 4

    Vector3d.normalize vector
    --> Vector3d.meters 0.6 0 0.8

    Vector3d.normalize Vector3d.zero
    --> Vector3d.zero

**Warning**: `Vector3d.direction` is safer since it forces you to explicitly
consider the case where the given vector is zero. `Vector3d.normalize` is
primarily useful for cases like generating WebGL meshes, where defaulting to a
zero vector for degenerate cases is acceptable, and the overhead of something
like

    Vector3d.direction vector
        |> Maybe.map Direction3d.toVector
        |> Maybe.withDefault Vector3d.zero

(which is functionally equivalent to `Vector3d.normalize vector`) is too high.

-}
normalize : Vector3d units coordinates -> Vector3d Unitless coordinates
normalize =
    scaleTo (Quantity.float 1)


{-| Find the sum of two vectors.

    firstVector =
        Vector3d.meters 1 2 3

    secondVector =
        Vector3d.meters 4 5 6

    firstVector |> Vector3d.plus secondVector
    --> Vector3d.meters 5 7 9

-}
plus : Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
plus (Types.Vector3d v2) (Types.Vector3d v1) =
    Types.Vector3d
        { x = v1.x + v2.x
        , y = v1.y + v2.y
        , z = v1.z + v2.z
        }


{-| Find the difference between two vectors (the second vector minus the first).

    firstVector =
        Vector3d.meters 5 6 7

    secondVector =
        Vector3d.meters 1 1 1

    firstVector |> Vector3d.minus secondVector
    --> Vector3d.meters 4 5 6

Note the argument order: `v1 - v2` would be written as

    v1 |> Vector3d.minus v2

which is the same as

    Vector3d.minus v2 v1

but the _opposite_ of

    Vector3d.minus v1 v2

-}
minus : Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
minus (Types.Vector3d v2) (Types.Vector3d v1) =
    Types.Vector3d
        { x = v1.x - v2.x
        , y = v1.y - v2.y
        , z = v1.z - v2.z
        }


{-| Find the dot product of two vectors.

    firstVector =
        Vector3d.meters 1 0 2

    secondVector =
        Vector3d.meters 3 4 5

    firstVector |> Vector3d.dot secondVector
    --> Area.squareMeters 13

-}
dot : Vector3d units2 coordinates -> Vector3d units1 coordinates -> Quantity Float (Product units1 units2)
dot (Types.Vector3d v2) (Types.Vector3d v1) =
    Quantity (v1.x * v2.x + v1.y * v2.y + v1.z * v2.z)


{-| Find the cross product of two vectors.

    firstVector =
        Vector3d.meters 2 0 0

    secondVector =
        Vector3d.meters 0 3 0

    firstVector |> Vector3d.cross secondVector
    --> Vector3d.xyz
    -->     Quantity.zero
    -->     Quantity.zero
    -->     (Area.squareMeters 6)

Note the argument order: `v1 x v2` would be written as

    v1 |> Vector3d.cross v2

which is the same as

    Vector3d.cross v2 v1

but the _opposite_ of

    Vector3d.cross v1 v2

Note that the cross product of two vectors with length units will be a vector
with area units!

-}
cross : Vector3d units2 coordinates -> Vector3d units1 coordinates -> Vector3d (Product units1 units2) coordinates
cross (Types.Vector3d v2) (Types.Vector3d v1) =
    Types.Vector3d
        { x = v1.y * v2.z - v1.z * v2.y
        , y = v1.z * v2.x - v1.x * v2.z
        , z = v1.x * v2.y - v1.y * v2.x
        }


{-| Find the sum of a list of vectors.
-}
sum : List (Vector3d units coordinates) -> Vector3d units coordinates
sum vectors =
    sumHelp 0 0 0 vectors


sumHelp : Float -> Float -> Float -> List (Vector3d units coordinates) -> Vector3d units coordinates
sumHelp sumX sumY sumZ vectors =
    case vectors of
        (Types.Vector3d { x, y, z }) :: rest ->
            sumHelp (sumX + x) (sumY + y) (sumZ + z) rest

        [] ->
            Types.Vector3d { x = sumX, y = sumY, z = sumZ }


{-| Shorthand for `Vector3d.scaleBy 2`.
-}
twice : Vector3d units coordinates -> Vector3d units coordinates
twice vector =
    scaleBy 2 vector


{-| Shorthand for `Vector3d.scaleBy 0.5`.
-}
half : Vector3d units coordinates -> Vector3d units coordinates
half vector =
    scaleBy 0.5 vector


{-| Multiply a scalar and a vector, resulting in a vector with units `Product
scalarUnits vectorUnits`:

    forceVector =
        Vector3d.product mass accelerationVector

If you just want to scale a vector by a certain amount, you can use
[`scaleBy`](#scaleBy) instead.

-}
product :
    Quantity Float scalarUnits
    -> Vector3d vectorUnits coordinates
    -> Vector3d (Product scalarUnits vectorUnits) coordinates
product (Quantity a) (Types.Vector3d v) =
    Types.Vector3d
        { x = a * v.x
        , y = a * v.y
        , z = a * v.z
        }


{-| Multiply a vector by a scalar, resulting in a vector with units `Product
vectorUnits scalarUnits`. (To the compiler `Product a b` and `Product b a`
are different unit types, so sometimes you will have to swap from `product` to
`times` or vice versa to make the types work out.)
-}
times :
    Quantity Float scalarUnits
    -> Vector3d vectorUnits coordinates
    -> Vector3d (Product vectorUnits scalarUnits) coordinates
times (Quantity a) (Types.Vector3d v) =
    Types.Vector3d
        { x = a * v.x
        , y = a * v.y
        , z = a * v.z
        }


{-| Divide a vector with units `Product units1 units2` by a scalar with units
`units1`, resulting in a vector with units `units2`.

    accelerationVector =
        forceVector |> Vector3d.over mass

-}
over :
    Quantity Float units1
    -> Vector3d (Product units1 units2) coordinates
    -> Vector3d units2 coordinates
over (Quantity a) (Types.Vector3d v) =
    Types.Vector3d
        { x = v.x / a
        , y = v.y / a
        , z = v.z / a
        }


{-| Divide a vector with units `Product units1 units2` by a scalar with units
`units2`, resulting in a vector with units `units1`. Provided for consistency
with `elm-units` but shouldn't be needed in most cases.
-}
over_ :
    Quantity Float units2
    -> Vector3d (Product units1 units2) coordinates
    -> Vector3d units1 coordinates
over_ (Quantity a) (Types.Vector3d v) =
    Types.Vector3d
        { x = v.x / a
        , y = v.y / a
        , z = v.z / a
        }


{-| Reverse the direction of a vector, negating its components.

    Vector3d.reverse (Vector3d.meters 1 -3 2)
    --> Vector3d.meters -1 3 -2

(This could have been called `negate`, but `reverse` is more consistent with
the naming used in other modules.)

-}
reverse : Vector3d units coordinates -> Vector3d units coordinates
reverse (Types.Vector3d v) =
    Types.Vector3d
        { x = -v.x
        , y = -v.y
        , z = -v.z
        }


{-| Scale the length of a vector by a given scale.

    Vector3d.scaleBy 3 (Vector3d.meters 1 2 3)
    --> Vector3d.meters 3 6 9

(This could have been called `multiply` or `times`, but `scaleBy` was chosen as
a more geometrically meaningful name and to be consistent with the `scaleAbout`
name used in other modules.)

-}
scaleBy : Float -> Vector3d units coordinates -> Vector3d units coordinates
scaleBy k (Types.Vector3d v) =
    Types.Vector3d
        { x = k * v.x
        , y = k * v.y
        , z = k * v.z
        }


{-| Scale a vector to a given length.

    Vector3d.scaleTo (Length.meters 25) (Vector3d.meters 0 3 4)
    --> Vector3d.meters 0 15 20

Scaling a zero vector will always result in a zero vector.

-}
scaleTo : Quantity Float units2 -> Vector3d units1 coordinates -> Vector3d units2 coordinates
scaleTo (Quantity q) (Types.Vector3d v) =
    let
        largestComponent =
            max (abs v.x) (max (abs v.y) (abs v.z))
    in
    if largestComponent == 0 then
        zero

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledZ =
                v.z / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Types.Vector3d
            { x = q * scaledX / scaledLength
            , y = q * scaledY / scaledLength
            , z = q * scaledZ / scaledLength
            }


{-| Rotate a vector around a given axis by a given angle.

    vector =
        Vector3d.meters 2 0 1

    vector
        |> Vector3d.rotateAround Axis3d.x
            (Angle.degrees 90)
    --> Vector3d.meters 2 -1 0

    vector
        |> Vector3d.rotateAround Axis3d.z
            (Angle.degrees 45)
    --> Vector3d.meters 1.4142 1.4142 1

-}
rotateAround : Axis3d units coordinates -> Angle -> Vector3d units coordinates -> Vector3d units coordinates
rotateAround (Types.Axis3d axis) (Quantity angle) (Types.Vector3d v) =
    let
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
    in
    Types.Vector3d
        { x = a00 * v.x + a01 * v.y + a02 * v.z
        , y = a10 * v.x + a11 * v.y + a12 * v.z
        , z = a20 * v.x + a21 * v.y + a22 * v.z
        }


{-| Mirror a vector across a plane.

    vector =
        Vector3d.meters 1 2 3

    Vector3d.mirrorAcross Plane3d.xy vector
    --> Vector3d.meters 1 2 -3

    Vector3d.mirrorAcross Plane3d.yz vector
    --> Vector3d.meters -1 2 3

-}
mirrorAcross : Plane3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
mirrorAcross (Types.Plane3d plane) (Types.Vector3d v) =
    let
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
    in
    Types.Vector3d
        { x = a00 * v.x + a01 * v.y + a02 * v.z
        , y = a01 * v.x + a11 * v.y + a12 * v.z
        , z = a02 * v.x + a12 * v.y + a22 * v.z
        }


{-| Find the projection of a vector in a particular direction. Conceptually,
this means splitting the original vector into a portion parallel to the given
direction and a portion perpendicular to it, then returning the parallel
portion.

    vector =
        Vector3d.meters 1 2 3

    Vector3d.projectionIn Direction3d.x vector
    --> Vector3d.meters 1 0 0

    Vector3d.projectionIn Direction3d.z vector
    --> Vector3d.meters 0 0 3

-}
projectionIn : Direction3d coordinates -> Vector3d units coordinates -> Vector3d units coordinates
projectionIn (Types.Direction3d d) (Types.Vector3d v) =
    let
        projectedLength =
            v.x * d.x + v.y * d.y + v.z * d.z
    in
    Types.Vector3d
        { x = d.x * projectedLength
        , y = d.y * projectedLength
        , z = d.z * projectedLength
        }


{-| Project a vector [orthographically](https://en.wikipedia.org/wiki/Orthographic_projection)
onto a plane. Conceptually, this means splitting the original vector into a
portion parallel to the plane (perpendicular to the plane's normal direction)
and a portion perpendicular to it (parallel to its normal direction), then
returning the parallel (in-plane) portion.

    vector =
        Vector3d.meters 2 1 3

    Vector3d.projectOnto Plane3d.xy vector
    --> Vector3d.meters 2 1 0

    Vector3d.projectOnto Plane3d.xz vector
    --> Vector3d.meters 2 0 3

-}
projectOnto : Plane3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
projectOnto (Types.Plane3d plane) (Types.Vector3d v) =
    let
        (Types.Direction3d n) =
            plane.normalDirection

        normalProjection =
            v.x * n.x + v.y * n.y + v.z * n.z
    in
    Types.Vector3d
        { x = v.x - normalProjection * n.x
        , y = v.y - normalProjection * n.y
        , z = v.z - normalProjection * n.z
        }


{-| Take a vector defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    vector =
        Vector3d.meters 2 0 3

    Vector3d.relativeTo rotatedFrame vector
    --> Vector3d.meters 1.732 -1 3

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Vector3d units globalCoordinates -> Vector3d units localCoordinates
relativeTo (Types.Frame3d frame) (Types.Vector3d v) =
    let
        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Vector3d
        { x = v.x * i.x + v.y * i.y + v.z * i.z
        , y = v.x * j.x + v.y * j.y + v.z * j.z
        , z = v.x * k.x + v.y * k.y + v.z * k.z
        }


{-| Take a vector defined in local coordinates relative to a given reference
frame, and return that vector expressed in global coordinates.

    vector =
        Vector3d.meters 2 0 3

    Vector3d.placeIn rotatedFrame vector
    --> Vector3d.meters 1.732 1 3

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Vector3d units localCoordinates -> Vector3d units globalCoordinates
placeIn (Types.Frame3d frame) (Types.Vector3d v) =
    let
        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Vector3d
        { x = i.x * v.x + j.x * v.y + k.x * v.z
        , y = i.y * v.x + j.y * v.y + k.y * v.z
        , z = i.z * v.x + j.z * v.y + k.z * v.z
        }


{-| Project a vector into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the vector onto the plane and then expresses the projected vector in 2D
sketch coordinates.

    vector =
        Vector3d.meters 2 1 3

    Vector3d.projectInto SketchPlane3d.xy vector
    --> Vector2d.meters 2 1

    Vector3d.projectInto SketchPlane3d.yz vector
    --> Vector2d.meters 1 3

    Vector3d.projectInto SketchPlane3d.zx vector
    --> Vector2d.meters 3 2

-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Vector3d units coordinates3d -> Vector2d units coordinates2d
projectInto (Types.SketchPlane3d sketchPlane) (Types.Vector3d v) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection
    in
    Types.Vector2d
        { x = v.x * i.x + v.y * i.y + v.z * i.z
        , y = v.x * j.x + v.y * j.y + v.z * j.z
        }


{-| -}
metersPerSecond : Float -> Float -> Float -> Vector3d MetersPerSecond coordinates
metersPerSecond x y z =
    xyz
        (Speed.metersPerSecond x)
        (Speed.metersPerSecond y)
        (Speed.metersPerSecond z)


{-| -}
feetPerSecond : Float -> Float -> Float -> Vector3d MetersPerSecond coordinates
feetPerSecond x y z =
    xyz
        (Speed.feetPerSecond x)
        (Speed.feetPerSecond y)
        (Speed.feetPerSecond z)


{-| -}
kilometersPerHour : Float -> Float -> Float -> Vector3d MetersPerSecond coordinates
kilometersPerHour x y z =
    xyz
        (Speed.kilometersPerHour x)
        (Speed.kilometersPerHour y)
        (Speed.kilometersPerHour z)


{-| -}
milesPerHour : Float -> Float -> Float -> Vector3d MetersPerSecond coordinates
milesPerHour x y z =
    xyz
        (Speed.milesPerHour x)
        (Speed.milesPerHour y)
        (Speed.milesPerHour z)


{-| -}
metersPerSecondSquared : Float -> Float -> Float -> Vector3d MetersPerSecondSquared coordinates
metersPerSecondSquared x y z =
    xyz
        (Acceleration.metersPerSecondSquared x)
        (Acceleration.metersPerSecondSquared y)
        (Acceleration.metersPerSecondSquared z)


{-| -}
feetPerSecondSquared : Float -> Float -> Float -> Vector3d MetersPerSecondSquared coordinates
feetPerSecondSquared x y z =
    xyz
        (Acceleration.feetPerSecondSquared x)
        (Acceleration.feetPerSecondSquared y)
        (Acceleration.feetPerSecondSquared z)


{-| -}
gees : Float -> Float -> Float -> Vector3d MetersPerSecondSquared coordinates
gees x y z =
    xyz
        (Acceleration.gees x)
        (Acceleration.gees y)
        (Acceleration.gees z)


{-| -}
newtons : Float -> Float -> Float -> Vector3d Newtons coordinates
newtons x y z =
    xyz
        (Force.newtons x)
        (Force.newtons y)
        (Force.newtons z)


{-| -}
kilonewtons : Float -> Float -> Float -> Vector3d Newtons coordinates
kilonewtons x y z =
    xyz
        (Force.kilonewtons x)
        (Force.kilonewtons y)
        (Force.kilonewtons z)


{-| -}
meganewtons : Float -> Float -> Float -> Vector3d Newtons coordinates
meganewtons x y z =
    xyz
        (Force.meganewtons x)
        (Force.meganewtons y)
        (Force.meganewtons z)


{-| -}
pounds : Float -> Float -> Float -> Vector3d Newtons coordinates
pounds x y z =
    xyz
        (Force.pounds x)
        (Force.pounds y)
        (Force.pounds z)


{-| -}
kips : Float -> Float -> Float -> Vector3d Newtons coordinates
kips x y z =
    xyz
        (Force.kips x)
        (Force.kips y)
        (Force.kips z)
