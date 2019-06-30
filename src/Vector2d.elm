--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Vector2d exposing
    ( Vector2d
    , zero
    , xy, xyIn, rTheta, rThetaIn, from, withLength, perpendicularTo, interpolateFrom
    , fromTuple, toTuple, fromRecord, toRecord
    , at, at_
    , per, for
    , xComponent, yComponent, componentIn, length, direction
    , equalWithin, lexicographicComparison
    , plus, minus, dot, cross
    , reverse, normalize, scaleBy, rotateBy, rotateClockwise, rotateCounterclockwise, mirrorAcross, projectionIn, projectOnto
    , relativeTo, placeIn
    )

{-| A `Vector2d` represents a quantity such as a displacement or velocity in 2D,
and is defined by its X and Y components. This module contains a variety of
vector-related functionality, such as

  - Adding or subtracting vectors
  - Finding the lengths of vectors
  - Rotating vectors
  - Converting vectors between different coordinate systems

Note that unlike in many other geometry packages where vectors are used as a
general-purpose data type, `elm-geometry` has separate data types for vectors,
directions and points. In most code it is actually more common to use `Point2d`
and `Direction2d` than `Vector2d`, and much code can avoid working directly with
`Vector2d` values at all!

@docs Vector2d


# Constants

@docs zero

Although there are no predefined constants for the vectors with components
(1,&nbsp;0) and (0,&nbsp;1), in most cases you will actually want their
`Direction2d` versions [`Direction2d.x`](Direction2d#x) and [`Direction2d.y`](Direction2d#y).


# Constructors

@docs xy, xyIn, rTheta, rThetaIn, from, withLength, perpendicularTo, interpolateFrom


# Interop

These functions are useful for interoperability with other Elm code that uses
plain `Float` tuples or records to represent vectors. The resulting `Vector2d`
values will have [unitless](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity#unitless-quantities)
components.

@docs fromTuple, toTuple, fromRecord, toRecord


# Unit conversion

@docs at, at_


# Rates of change

@docs per, for


# Properties

@docs xComponent, yComponent, componentIn, length, direction


# Comparison

@docs equalWithin, lexicographicComparison


# Arithmetic

@docs plus, minus, dot, cross


# Transformations

Note that for `mirrorAcross` and `projectOnto`, only the direction of the axis
affects the result, since vectors are position-independent. Think of
mirroring/projecting a vector across/onto an axis as moving the vector so its
tail is on the axis, then mirroring/projecting its tip across/onto the axis.

@docs reverse, normalize, scaleBy, rotateBy, rotateClockwise, rotateCounterclockwise, mirrorAcross, projectionIn, projectOnto


# Coordinate conversions

Like other transformations, coordinate conversions of vectors depend only on the
orientations of the relevant frames, not the positions of their origin points.

For the examples, assume the following frame has been defined:

    rotatedFrame =
        Frame2d.atOrigin |> Frame2d.rotateBy (degrees 30)

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis2d, Direction2d, Frame2d, Point2d)
import Quantity exposing (Product, Quantity(..), Rate, Squared, Unitless)
import Quantity.Extra as Quantity


{-| -}
type alias Vector2d units coordinates =
    Types.Vector2d units coordinates


{-| The zero vector.

    Vector2d.zero
    --> Vector2d.fromComponents ( 0, 0 )

-}
zero : Vector2d units coordinates
zero =
    Types.Vector2d { x = 0, y = 0 }


{-| Construct a vector from its X and Y components.

    vector =
        Vector2d.fromComponents ( 2, 3 )

-}
xy : Quantity Float units -> Quantity Float units -> Vector2d units coordinates
xy (Quantity x) (Quantity y) =
    Types.Vector2d { x = x, y = y }


{-| Construct a vector given its local components within a particular frame:

    rotatedFrame =
        Frame2d.atOrigin
            |> Frame2d.rotateBy (Angle.degrees 45)

    Vector2d.fromComponentsIn rotatedFrame
        ( Length.meters 2
        , Length.meters 0
        )
    --> Vector2d.fromComponents
    -->     ( Length.meters 1.4142
    -->     , Length.meters 1.4142
    -->     )

-}
xyIn : Frame2d units globalCoordinates localCoordinates -> Quantity Float units -> Quantity Float units -> Vector2d units globalCoordinates
xyIn (Types.Frame2d frame) (Quantity x) (Quantity y) =
    let
        (Types.Direction2d i) =
            frame.xDirection

        (Types.Direction2d j) =
            frame.yDirection
    in
    Types.Vector2d
        { x = x * i.x + y * j.x
        , y = x * i.y + y * j.y
        }


{-| Construct a vector from a length and angle. The angle is measured
counterclockwise from the positive X direction.

    Vector2d.fromPolarComponents ( 2, degrees 135 )
    -->Vector2d.fromComponents ( -1.4142, 1.4142 )

-}
rTheta : Quantity Float units -> Angle -> Vector2d units coordinates
rTheta (Quantity r) (Quantity theta) =
    Types.Vector2d
        { x = r * cos theta
        , y = r * sin theta
        }


{-| Construct a vector given its local polar components within a particular
frame:

    rotatedFrame =
        Frame2d.atOrigin
            |> Frame2d.rotateBy (Angle.degrees 45)

    Vector2d.fromPolarComponentsIn rotatedFrame
        ( Length.meters 1
        , Angle.degrees 0
        )
    --> Vector2d.fromComponents
    -->     ( Length.meters 0.7071
    -->     , Length.meters 0.7071
    -->     )

-}
rThetaIn : Frame2d units globalCoordinates localCoordinates -> Quantity Float units -> Angle -> Vector2d units globalCoordinates
rThetaIn (Types.Frame2d frame) (Quantity r) (Quantity theta) =
    let
        (Types.Direction2d i) =
            frame.xDirection

        (Types.Direction2d j) =
            frame.yDirection

        cosTheta =
            cos theta

        sinTheta =
            sin theta
    in
    Types.Vector2d
        { x = r * (cosTheta * i.x + sinTheta * j.x)
        , y = r * (cosTheta * i.y + sinTheta * j.y)
        }


{-| Construct a vector from the first given point to the second.

    startPoint =
        Point2d.fromCoordinates ( 1, 1 )

    endPoint =
        Point2d.fromCoordinates ( 4, 5 )

    Vector2d.from startPoint endPoint
    --> Vector2d.fromComponents ( 3, 4 )

-}
from : Point2d units coordinates -> Point2d units coordinates -> Vector2d units coordinates
from (Types.Point2d p1) (Types.Point2d p2) =
    Types.Vector2d
        { x = p2.x - p1.x
        , y = p2.y - p1.y
        }


{-| Construct a vector with the given length in the given direction.

    Vector2d.withLength 5 Direction2d.y
    --> Vector2d.fromComponents ( 0, 5 )

-}
withLength : Quantity Float units -> Direction2d coordinates -> Vector2d units coordinates
withLength (Quantity a) (Types.Direction2d d) =
    Types.Vector2d
        { x = a * d.x
        , y = a * d.y
        }


{-| Construct a vector perpendicular to the given vector, by rotating the given
vector 90 degrees counterclockwise. The constructed vector will have the same
length as the given vector. Alias for `Vector2d.rotateCounterclockwise`.

    Vector2d.perpendicularTo
        (Vector2d.fromComponents ( 1, 0 ))
    --> Vector2d.fromComponents ( 0, 1 )

    Vector2d.perpendicularTo
        (Vector2d.fromComponents ( 0, 2 ))
    --> Vector2d.fromComponents ( -2, 0 )

    Vector2d.perpendicularTo
        (Vector2d.fromComponents ( 3, 1 ))
    --> Vector2d.fromComponents ( -1, 3 )

    Vector2d.perpendicularTo Vector2d.zero
    --> Vector2d.zero

-}
perpendicularTo : Vector2d units coordinates -> Vector2d units coordinates
perpendicularTo givenVector =
    rotateCounterclockwise givenVector


{-| Construct a vector by interpolating from the first given vector to the
second, based on a parameter that ranges from zero to one.

    startVector =
        Vector2d.zero

    endVector =
        Vector2d.fromComponents ( 8, 12 )

    Vector2d.interpolateFrom startVector endVector 0.25
    --> Vector2d.fromComponents ( 2, 3 )

Partial application may be useful:

    interpolatedVector : Float -> Vector2d
    interpolatedVector =
        Vector2d.interpolateFrom startVector endVector

    List.map interpolatedVector [ 0, 0.5, 1 ]
    --> [ Vector2d.fromComponents ( 0, 0 )
    --> , Vector2d.fromComponents ( 4, 6 )
    --> , Vector2d.fromComponents ( 8, 12 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedVector -0.5
    --> Vector2d.fromComponents ( -4, -6 )

    interpolatedVector 1.25
    --> Vector2d.fromComponents ( 10, 15 )

-}
interpolateFrom : Vector2d units coordinates -> Vector2d units coordinates -> Float -> Vector2d units coordinates
interpolateFrom (Types.Vector2d v1) (Types.Vector2d v2) t =
    if t <= 0.5 then
        Types.Vector2d
            { x = v1.x + t * (v2.x - v1.x)
            , y = v1.y + t * (v2.y - v1.y)
            }

    else
        Types.Vector2d
            { x = v2.x + (1 - t) * (v1.x - v2.x)
            , y = v2.y + (1 - t) * (v1.y - v2.y)
            }


{-| Construct a `Vector2d` from a tuple of `Float` values, by specifying what units those values are
in.

    Vector2d.fromTuple Length.meters ( 2, 3 )
    --> Vector2d.fromComponents
    -->     (Length.meters 2)
    -->     (Length.meters 3)

-}
fromTuple : (Float -> Quantity Float units) -> ( Float, Float ) -> Vector2d units coordinates
fromTuple toQuantity ( x, y ) =
    xy (toQuantity x) (toQuantity y)


{-| Convert a `Vector2d` to a tuple of `Float` values, by specifying what units you want the result
to be in.

    vector =
        Vector2d.fromComponents
            (Length.feet 2)
            (Length.feet 3)

    Vector2d.toTuple Length.inInches vector
    --> ( 24, 36 )

-}
toTuple : (Quantity Float units -> Float) -> Vector2d units coordinates -> ( Float, Float )
toTuple fromQuantity vector =
    ( fromQuantity (xComponent vector)
    , fromQuantity (yComponent vector)
    )


{-| Construct a `Vector2d` from a record with `Float` fields, by specifying what units those fields
are in.

    Vector2d.fromRecord Length.inches { x = 24, y = 36 }
    --> Vector2d.fromComponents
    -->     (Length.feet 2)
    -->     (Length.feet 3)

-}
fromRecord : (Float -> Quantity Float units) -> { x : Float, y : Float } -> Vector2d units coordinates
fromRecord toQuantity { x, y } =
    xy (toQuantity x) (toQuantity y)


{-| Convert a `Vector2d` to a record with `Float` fields, by specifying what units you want the
result to be in.

    vector =
        Vector2d.fromComponents
            (Length.meters 2)
            (Length.meters 3)

    Vector2d.toRecord Length.inCentimeters vector
    --> { x = 200, y = 300 }

-}
toRecord : (Quantity Float units -> Float) -> Vector2d units coordinates -> { x : Float, y : Float }
toRecord fromQuantity vector =
    { x = fromQuantity (xComponent vector)
    , y = fromQuantity (yComponent vector)
    }


{-| Convert a vector from one units type to another, by providing a conversion factor given as a
rate of change of destination units with respect to source units.

    worldVector =
        Vector2d.fromComponents
            ( Length.meters 2
            , Length.meters 3
            )

    resolution : Quantity Float (Rate Pixels Meters)
    resolution =
        Pixels.pixels 100 |> Quantity.per (Length.meters 1)

    worldVector |> Vector2d.at resolution
    --> Vector2d.fromComponents
    -->     ( Pixels.pixels 200
    -->     , Pixels.pixels 300
    -->     )

-}
at : Quantity Float (Rate destinationUnits sourceUnits) -> Vector2d sourceUnits coordinates -> Vector2d destinationUnits coordinates
at (Quantity rate) (Types.Vector2d v) =
    Types.Vector2d
        { x = rate * v.x
        , y = rate * v.y
        }


{-| Convert a vector from one units type to another, by providing an 'inverse' conversion factor
given as a rate of change of source units with respect to destination units.

    screenVector =
        Vector2d.fromComponents
            ( Pixels.pixels 200
            , Pixels.pixels 300
            )

    resolution : Quantity Float (Rate Pixels Meters)
    resolution =
        Pixels.pixels 50 |> Quantity.per (Length.meters 1)

    screenVector |> Vector2d.at_ resolution
    --> Vector2d.fromComponents
    -->     ( Length.meters 4
    -->     , Length.meters 6
    -->     )

-}
at_ : Quantity Float (Rate sourceUnits destinationUnits) -> Vector2d sourceUnits coordinates -> Vector2d destinationUnits coordinates
at_ (Quantity rate) (Types.Vector2d v) =
    Types.Vector2d
        { x = v.x / rate
        , y = v.y / rate
        }


{-| Construct a vector representing a rate of change such as a speed:

    displacement =
        Vector2d.fromComponents
            ( Length.meters 6
            , Length.meters 8
            )

    velocity =
        displacement |> Vector2d.per (Duration.seconds 2)

    -- Get the magnitude of the velocity (the speed)
    Vector2d.length velocity
    --> Speed.metersPerSecond 5

-}
per : Quantity Float independentUnits -> Vector2d dependentUnits coordinates -> Vector2d (Rate dependentUnits independentUnits) coordinates
per (Quantity a) (Types.Vector2d v) =
    Types.Vector2d
        { x = v.x / a
        , y = v.y / a
        }


{-| Multiply a rate of change vector by an independent quantity to get a total vector. For example,
multiply a velocity by a duration to get a total displacement:

    velocity =
        Vector2d.fromComponents
            ( Pixels.pixelsPerSecond 200
            , Pixels.pixelsPerSecond 50
            )

    velocity |> Vector2d.for (Duration.seconds 0.1)
    --> Vector2d.fromComponents
    -->     ( Pixels.pixels 20
    -->     , Pixels.pixels 5
    -->     )

-}
for : Quantity Float independentUnits -> Vector2d (Rate dependentUnits independentUnits) coordinates -> Vector2d dependentUnits coordinates
for (Quantity a) (Types.Vector2d v) =
    Types.Vector2d
        { x = v.x * a
        , y = v.y * a
        }


{-| Get the X component of a vector.

    Vector2d.xComponent (Vector2d.fromComponents ( 2, 3 ))
    --> 2

-}
xComponent : Vector2d units coordinates -> Quantity Float units
xComponent (Types.Vector2d v) =
    Quantity v.x


{-| Get the Y component of a vector.

    Vector2d.yComponent (Vector2d.fromComponents ( 2, 3 ))
    --> 3

-}
yComponent : Vector2d units coordinates -> Quantity Float units
yComponent (Types.Vector2d v) =
    Quantity v.y


{-| Find the component of a vector in an arbitrary direction, for example

    forwardSpeed =
        Vector2d.componentIn forwardDirection velocity

This is more general and flexible than using `xComponent` or `yComponent`, both
of which can be expressed in terms of `componentIn`; for example,

    Vector2d.xComponent vector

is equivalent to

    Vector2d.componentIn Direction2d.x vector

-}
componentIn : Direction2d coordinates -> Vector2d units coordinates -> Quantity Float units
componentIn (Types.Direction2d d) (Types.Vector2d v) =
    Quantity (v.x * d.x + v.y * d.y)


{-| Compare two vectors within a tolerance. Returns true if the difference
between the two given vectors has magnitude less than the given tolerance.

    firstVector =
        Vector2d.fromComponents ( 1, 2 )

    secondVector =
        Vector2d.fromComponents ( 0.9999, 2.0002 )

    Vector2d.equalWithin 1e-3 firstVector secondVector
    --> True

    Vector2d.equalWithin 1e-6 firstVector secondVector
    --> False

-}
equalWithin : Quantity Float units -> Vector2d units coordinates -> Vector2d units coordinates -> Bool
equalWithin (Quantity eps) (Types.Vector2d v1) (Types.Vector2d v2) =
    if eps > 0 then
        let
            nx =
                (v2.x - v1.x) / eps

            ny =
                (v2.y - v1.y) / eps
        in
        nx * nx + ny * ny <= 1

    else if eps == 0 then
        v1.x == v2.x && v1.y == v2.y

    else
        False


{-| Compare two `Vector2d` values lexicographically: first by X component, then
by Y. Can be used to provide a sort order for `Vector2d` values.
-}
lexicographicComparison : Vector2d units coordinates -> Vector2d units coordinates -> Order
lexicographicComparison (Types.Vector2d v1) (Types.Vector2d v2) =
    if v1.x /= v2.x then
        compare v1.x v2.x

    else
        compare v1.y v2.y


{-| Get the length (magnitude) of a vector.

    Vector2d.length (Vector2d.fromComponents ( 3, 4 ))
    --> 5

-}
length : Vector2d units coordinates -> Quantity Float units
length (Types.Vector2d v) =
    let
        largestComponent =
            max (abs v.x) (abs v.y)
    in
    if largestComponent == 0 then
        Quantity.zero

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)
        in
        Quantity (scaledLength * largestComponent)


{-| Attempt to find the direction of a vector. In the case of a zero vector,
return `Nothing`.

    Vector2d.direction (Vector2d.fromComponents ( 3, 3 ))
    --> Just (Direction2d.fromAngle (degrees 45))

    Vector2d.direction Vector2d.zero
    --> Nothing

-}
direction : Vector2d units coordinates -> Maybe (Direction2d coordinates)
direction (Types.Vector2d v) =
    let
        largestComponent =
            max (abs v.x) (abs v.y)
    in
    if largestComponent == 0 then
        Nothing

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)
        in
        Just <|
            Types.Direction2d
                { x = scaledX / scaledLength
                , y = scaledY / scaledLength
                }


{-| Normalize a vector to have a length of one. Zero vectors are left as-is.

    vector =
        Vector2d.fromComponents ( 3, 4 )

    Vector2d.normalize vector
    --> Vector2d.fromComponents ( 0.6, 0.8 )

    Vector2d.normalize Vector2d.zero
    --> Vector2d.zero

**Warning**: `Vector2d.direction` is safer since it forces you to explicitly
consider the case where the given vector is zero. `Vector2d.normalize` is
primarily useful for cases like generating WebGL meshes, where defaulting to a
zero vector for degenerate cases is acceptable, and the overhead of something
like

    Vector2d.direction vector
        |> Maybe.map Direction2d.toVector
        |> Maybe.withDefault Vector2d.zero

(which is functionally equivalent to `Vector2d.normalize vector`) is too high.

-}
normalize : Vector2d units coordinates -> Vector2d Unitless coordinates
normalize (Types.Vector2d v) =
    let
        largestComponent =
            max (abs v.x) (abs v.y)
    in
    if largestComponent == 0 then
        zero

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)
        in
        Types.Vector2d
            { x = scaledX / scaledLength
            , y = scaledY / scaledLength
            }


{-| Find the sum of two vectors.

    firstVector =
        Vector2d.fromComponents ( 1, 2 )

    secondVector =
        Vector2d.fromComponents ( 3, 4 )

    Vector2d.sum firstVector secondVector
    --> Vector2d.fromComponents ( 4, 6 )

-}
plus : Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates
plus (Types.Vector2d v2) (Types.Vector2d v1) =
    Types.Vector2d
        { x = v1.x + v2.x
        , y = v1.y + v2.y
        }


{-| Find the difference between two vectors (the first vector minus the second).

    firstVector =
        Vector2d.fromComponents ( 5, 6 )

    secondVector =
        Vector2d.fromComponents ( 1, 3 )

    Vector2d.difference firstVector secondVector
    --> Vector2d.fromComponents ( 4, 3 )

-}
minus : Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates
minus (Types.Vector2d v2) (Types.Vector2d v1) =
    Types.Vector2d
        { x = v1.x - v2.x
        , y = v1.y - v2.y
        }


{-| Find the dot product of two vectors.

    firstVector =
        Vector2d.fromComponents
            ( Length.meters 1
            , Length.meters 2
            )

    secondVector =
        Vector2d.fromComponents
            ( Length.meters 3
            , Length.meters 4
            )

    firstVector |> Vector2d.dot secondVector
    --> Area.squareMeters 11

-}
dot : Vector2d units2 coordinates -> Vector2d units1 coordinates -> Quantity Float (Product units1 units2)
dot (Types.Vector2d v2) (Types.Vector2d v1) =
    Quantity (v1.x * v2.x + v1.y * v2.y)


{-| Find the scalar 'cross product' of two vectors in 2D. This is useful in many
of the same ways as the 3D cross product:

  - Its length is equal to the product of the lengths of the two given vectors
    and the sine of the angle between them, so it can be used as a metric to
    determine if two vectors are nearly parallel.
  - The sign of the result indicates the direction of rotation from the first
    vector to the second (positive indicates a counterclockwise rotation and
    negative indicates a clockwise rotation), similar to how the direction of
    the 3D cross product indicates the direction of rotation.

Note the argument order - `v1 x v2` would be written as

    v1 |> Vector2d.cross v2

which is the same as

    Vector2d.cross v2 v1

but the _opposite_ of

    Vector2d.cross v1 v2

Some examples:

    firstVector =
        Vector2d.fromComponents
            ( Length.feet 2
            , Length.feet 0
            )

    secondVector =
        Vector2d.fromComponents
            ( Length.feet 0
            , Length.feet 3
            )

    firstVector |> Vector2d.cross secondVector
    --> Area.squareFeet 6

    secondVector |> Vector2d.cross firstVector
    --> Area.squareFeet -6

    firstVector |> Vector2d.cross firstVector
    --> Area.squareFeet 0

-}
cross : Vector2d units2 coordinates -> Vector2d units1 coordinates -> Quantity Float (Product units1 units2)
cross (Types.Vector2d v2) (Types.Vector2d v1) =
    Quantity (v1.x * v2.y - v1.y * v2.x)


{-| Reverse the direction of a vector, negating its components.

    Vector2d.reverse (Vector2d.fromComponents ( -1, 2 ))
    --> Vector2d.fromComponents ( 1, -2 )

(This could have been called `negate`, but `reverse` is more consistent with
the naming used in other modules.)

-}
reverse : Vector2d units coordinates -> Vector2d units coordinates
reverse (Types.Vector2d v) =
    Types.Vector2d
        { x = -v.x
        , y = -v.y
        }


{-| Scale the length of a vector by a given scale.

    Vector2d.scaleBy 3 (Vector2d.fromComponents ( 1, 2 ))
    --> Vector2d.fromComponents ( 3, 6 )

(This could have been called `multiply` or `times`, but `scaleBy` was chosen as
a more geometrically meaningful name and to be consistent with the `scaleAbout`
name used in other modules.)

-}
scaleBy : Float -> Vector2d units coordinates -> Vector2d units coordinates
scaleBy k (Types.Vector2d v) =
    Types.Vector2d
        { x = k * v.x
        , y = k * v.y
        }


{-| Rotate a vector counterclockwise by a given angle (in radians).

    Vector2d.fromComponents ( 1, 1 )
        |> Vector2d.rotateBy (degrees 45)
    --> Vector2d.fromComponents ( 0, 1.4142 )

    Vector2d.fromComponents ( 1, 0 )
        |> Vector2d.rotateBy pi
    --> Vector2d.fromComponents ( -1, 0 )

-}
rotateBy : Angle -> Vector2d units coordinates -> Vector2d units coordinates
rotateBy (Quantity theta) (Types.Vector2d v) =
    let
        c =
            cos theta

        s =
            sin theta
    in
    Types.Vector2d
        { x = c * v.x - s * v.y
        , y = s * v.x + c * v.y
        }


{-| Rotate the given vector 90 degrees counterclockwise;

    Vector2d.rotateCounterclockwise vector

is equivalent to

    Vector2d.rotateBy (degrees 90) vector

but is more efficient.

-}
rotateCounterclockwise : Vector2d units coordinates -> Vector2d units coordinates
rotateCounterclockwise (Types.Vector2d v) =
    Types.Vector2d
        { x = -v.y
        , y = v.x
        }


{-| Rotate the given vector 90 degrees clockwise;

    Vector2d.rotateClockwise vector

is equivalent to

    Vector2d.rotateBy (degrees -90) vector

but is more efficient.

-}
rotateClockwise : Vector2d units coordinates -> Vector2d units coordinates
rotateClockwise (Types.Vector2d v) =
    Types.Vector2d
        { x = v.y
        , y = -v.x
        }


{-| Mirror a vector across a given axis.

    vector =
        Vector2d.fromComponents ( 2, 3 )

    Vector2d.mirrorAcross Axis2d.y vector
    --> Vector2d.fromComponents ( -2, 3 )

The position of the axis doesn't matter, only its orientation:

    horizontalAxis =
        Axis2d.withDirection Direction2d.x
            (Point2d.fromCoordinates ( 100, 200 ))

    Vector2d.mirrorAcross horizontalAxis vector
    --> Vector2d.fromComponents ( 2, -3 )

-}
mirrorAcross : Axis2d axisUnits coordinates -> Vector2d units coordinates -> Vector2d units coordinates
mirrorAcross (Types.Axis2d axis) (Types.Vector2d v) =
    let
        (Types.Direction2d d) =
            axis.direction

        a =
            1 - 2 * d.y * d.y

        b =
            2 * d.x * d.y

        c =
            1 - 2 * d.x * d.x
    in
    Types.Vector2d
        { x = a * v.x + b * v.y
        , y = b * v.x + c * v.y
        }


{-| Find the projection of a vector in a particular direction. Conceptually,
this means splitting the original vector into a portion parallel to the given
direction and a portion perpendicular to it, then returning the parallel
portion.

    vector =
        Vector2d.fromComponents ( 2, 3 )

    Vector2d.projectionIn Direction2d.x vector
    --> Vector2d.fromComponents ( 2, 0 )

    Vector2d.projectionIn Direction2d.y vector
    --> Vector2d.fromComponents ( 0, 3 )

-}
projectionIn : Direction2d coordinates -> Vector2d units coordinates -> Vector2d units coordinates
projectionIn (Types.Direction2d d) (Types.Vector2d v) =
    let
        projectedLength =
            v.x * d.x + v.y * d.y
    in
    Types.Vector2d
        { x = projectedLength * d.x
        , y = projectedLength * d.y
        }


{-| Project a vector onto an axis.

    Vector2d.projectOnto Axis2d.y
        (Vector2d.fromComponents ( 3, 4 ))
    --> Vector2d.fromComponents ( 0, 4 )

    Vector2d.projectOnto Axis2d.x
        (Vector2d.fromComponents ( -1, 2 ))
    --> Vector2d.fromComponents ( -1, 0 )

This is equivalent to finding the projection in the axis' direction.

-}
projectOnto : Axis2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates
projectOnto (Types.Axis2d axis) (Types.Vector2d v) =
    let
        (Types.Direction2d d) =
            axis.direction

        projectedLength =
            v.x * d.x + v.y * d.y
    in
    Types.Vector2d
        { x = projectedLength * d.x
        , y = projectedLength * d.y
        }


{-| Take a vector defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Vector2d.fromComponents ( 2, 0 )
        |> Vector2d.relativeTo rotatedFrame
    --> Vector2d.fromComponents ( 1.732, -1 )

-}
relativeTo : Frame2d frameUnits globalCoordinates localCoordinates -> Vector2d units globalCoordinates -> Vector2d units localCoordinates
relativeTo (Types.Frame2d frame) (Types.Vector2d v) =
    let
        (Types.Direction2d dx) =
            frame.xDirection

        (Types.Direction2d dy) =
            frame.yDirection
    in
    Types.Vector2d
        { x = v.x * dx.x + v.y * dx.y
        , y = v.x * dy.x + v.y * dy.y
        }


{-| Take a vector defined in local coordinates relative to a given reference
frame, and return that vector expressed in global coordinates.

    Vector2d.fromComponents ( 2, 0 )
        |> Vector2d.placeIn rotatedFrame
    --> Vector2d.fromComponents ( 1.732, 1 )

-}
placeIn : Frame2d frameUnits globalCoordinates localCoordinates -> Vector2d units localCoordinates -> Vector2d units globalCoordinates
placeIn (Types.Frame2d frame) (Types.Vector2d v) =
    let
        (Types.Direction2d dx) =
            frame.xDirection

        (Types.Direction2d dy) =
            frame.yDirection
    in
    Types.Vector2d
        { x = v.x * dx.x + v.y * dy.x
        , y = v.x * dx.y + v.y * dy.y
        }
