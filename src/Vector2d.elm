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
    , fromComponents, fromComponentsIn, fromPolarComponents, fromPolarComponentsIn, from, withLength, perpendicularTo, interpolateFrom
    , fromTuple, toTuple, fromRecord, toRecord
    , components, componentsIn, xComponent, yComponent, componentIn, polarComponents, length, squaredLength, direction, lengthAndDirection
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

@docs fromComponents, fromComponentsIn, fromPolarComponents, fromPolarComponentsIn, from, withLength, perpendicularTo, interpolateFrom


# Conversion

@docs fromTuple, toTuple, fromRecord, toRecord


# Properties

@docs components, componentsIn, xComponent, yComponent, componentIn, polarComponents, length, squaredLength, direction, lengthAndDirection


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
import Bootstrap.Axis2d as Axis2d
import Bootstrap.Direction2d as Direction2d
import Bootstrap.Frame2d as Frame2d
import Bootstrap.Point2d as Point2d
import Geometry.Types as Types exposing (Axis2d, Direction2d, Frame2d, Point2d)
import Quantity exposing (Product, Quantity, Squared, Unitless)
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
    fromComponents ( Quantity.zero, Quantity.zero )


{-| Construct a vector from its X and Y components.

    vector =
        Vector2d.fromComponents ( 2, 3 )

-}
fromComponents : ( Quantity Float units, Quantity Float units ) -> Vector2d units coordinates
fromComponents givenComponents =
    Types.Vector2d givenComponents


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
fromComponentsIn : Frame2d units globalCoordinates localCoordinates -> ( Quantity Float units, Quantity Float units ) -> Vector2d units globalCoordinates
fromComponentsIn frame localComponents =
    let
        ( x, y ) =
            localComponents

        ( x1, y1 ) =
            Direction2d.components (Frame2d.xDirection frame)

        ( x2, y2 ) =
            Direction2d.components (Frame2d.yDirection frame)
    in
    fromComponents
        ( Quantity.aXbY x1 x x2 y
        , Quantity.aXbY y1 x y2 y
        )


{-| Construct a vector from a length and angle. The angle is measured
counterclockwise from the positive X direction.

    Vector2d.fromPolarComponents ( 2, degrees 135 )
    -->Vector2d.fromComponents ( -1.4142, 1.4142 )

-}
fromPolarComponents : ( Quantity Float units, Angle ) -> Vector2d units coordinates
fromPolarComponents ( givenRadius, givenAngle ) =
    fromComponents
        ( Quantity.rCosTheta givenRadius givenAngle
        , Quantity.rSinTheta givenRadius givenAngle
        )


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
fromPolarComponentsIn : Frame2d units globalCoordinates localCoordinates -> ( Quantity Float units, Angle ) -> Vector2d units globalCoordinates
fromPolarComponentsIn frame localPolarComponents =
    let
        ( r, theta ) =
            localPolarComponents
    in
    fromComponentsIn frame
        ( Quantity.rCosTheta r theta
        , Quantity.rSinTheta r theta
        )


{-| Construct a vector from the first given point to the second.

    startPoint =
        Point2d.fromCoordinates ( 1, 1 )

    endPoint =
        Point2d.fromCoordinates ( 4, 5 )

    Vector2d.from startPoint endPoint
    --> Vector2d.fromComponents ( 3, 4 )

-}
from : Point2d units coordinates -> Point2d units coordinates -> Vector2d units coordinates
from firstPoint secondPoint =
    let
        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
    fromComponents
        ( x2 |> Quantity.minus x1
        , y2 |> Quantity.minus y1
        )


{-| Construct a vector with the given length in the given direction.

    Vector2d.withLength 5 Direction2d.y
    --> Vector2d.fromComponents ( 0, 5 )

-}
withLength : Quantity Float units -> Direction2d coordinates -> Vector2d units coordinates
withLength givenLength givenDirection =
    let
        ( dx, dy ) =
            Direction2d.components givenDirection
    in
    fromComponents
        ( Quantity.multiplyBy dx givenLength
        , Quantity.multiplyBy dy givenLength
        )


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
interpolateFrom firstVector secondVector givenParameter =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    fromComponents
        ( Quantity.interpolateFrom x1 x2 givenParameter
        , Quantity.interpolateFrom y1 y2 givenParameter
        )


fromTuple : ( Float, Float ) -> Vector2d Unitless coordinates
fromTuple ( x, y ) =
    fromComponents ( Quantity.float x, Quantity.float y )


toTuple : Vector2d Unitless coordinates -> ( Float, Float )
toTuple vector =
    ( Quantity.toFloat (xComponent vector)
    , Quantity.toFloat (yComponent vector)
    )


fromRecord : { x : Float, y : Float } -> Vector2d Unitless coordinates
fromRecord { x, y } =
    fromComponents ( Quantity.float x, Quantity.float y )


toRecord : Vector2d Unitless coordinates -> { x : Float, y : Float }
toRecord vector =
    { x = Quantity.toFloat (xComponent vector)
    , y = Quantity.toFloat (yComponent vector)
    }


{-| Extract the components of a vector.

    Vector2d.components (Vector2d.fromComponents ( 2, 3 ))
    --> ( 2, 3 )

This combined with Elm's built-in tuple destructuring provides a convenient way
to extract both the X and Y components of a vector in one line of code:

    ( x, y ) =
        Vector2d.components vector

-}
components : Vector2d units coordinates -> ( Quantity Float units, Quantity Float units )
components (Types.Vector2d vectorComponents) =
    vectorComponents


{-| Find the components of a vector in a given frame;

    Vector2d.componentsIn frame vector

is equivalent to

    ( Vector2d.componentIn (Frame2d.xDirection frame) vector
    , Vector2d.componentIn (Frame2d.yDirection frame) vector
    )

-}
componentsIn : Frame2d units globalCoordinates localCoordinates -> Vector2d units globalCoordinates -> ( Quantity Float units, Quantity Float units )
componentsIn frame vector =
    ( vector |> componentIn (Frame2d.xDirection frame)
    , vector |> componentIn (Frame2d.yDirection frame)
    )


{-| Get the X component of a vector.

    Vector2d.xComponent (Vector2d.fromComponents ( 2, 3 ))
    --> 2

-}
xComponent : Vector2d units coordinates -> Quantity Float units
xComponent (Types.Vector2d ( x, _ )) =
    x


{-| Get the Y component of a vector.

    Vector2d.yComponent (Vector2d.fromComponents ( 2, 3 ))
    --> 3

-}
yComponent : Vector2d units coordinates -> Quantity Float units
yComponent (Types.Vector2d ( _, y )) =
    y


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
componentIn givenDirection givenVector =
    let
        ( dx, dy ) =
            Direction2d.components givenDirection

        ( vx, vy ) =
            components givenVector
    in
    Quantity.aXbY dx vx dy vy


{-| Get the polar components (length, polar angle) of a vector.

    Vector2d.polarComponents
        (Vector2d.fromComponents ( 1, 1 ))
    --> ( 1.4142, degrees 45 )

-}
polarComponents : Vector2d units coordinates -> ( Quantity Float units, Angle )
polarComponents givenVector =
    let
        ( x, y ) =
            components givenVector
    in
    ( length givenVector, Angle.atan2 y x )


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
equalWithin givenTolerance firstVector secondVector =
    if givenTolerance |> Quantity.lessThan Quantity.zero then
        False

    else
        let
            difference =
                secondVector |> minus firstVector
        in
        squaredLength difference
            |> Quantity.lessThanOrEqualTo (Quantity.squared givenTolerance)


lexicographicComparison : Vector2d units coordinates -> Vector2d units coordinates -> Order
lexicographicComparison firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    if x1 /= x2 then
        Quantity.compare x1 x2

    else
        Quantity.compare y1 y2


{-| Get the length (magnitude) of a vector.

    Vector2d.length (Vector2d.fromComponents ( 3, 4 ))
    --> 5

-}
length : Vector2d units coordinates -> Quantity Float units
length givenVector =
    Quantity.sqrt (squaredLength givenVector)


{-| Get the squared length of a vector. `squaredLength` is slightly faster than
`length`, so for example

    Vector2d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly more efficient than

    Vector2d.length vector > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `length` is much more
readable!

-}
squaredLength : Vector2d units coordinates -> Quantity Float (Squared units)
squaredLength givenVector =
    let
        ( x, y ) =
            components givenVector
    in
    Quantity.squared x |> Quantity.plus (Quantity.squared y)


{-| Attempt to find the direction of a vector. In the case of a zero vector,
return `Nothing`.

    Vector2d.direction (Vector2d.fromComponents ( 3, 3 ))
    --> Just (Direction2d.fromAngle (degrees 45))

    Vector2d.direction Vector2d.zero
    --> Nothing

-}
direction : Vector2d units coordinates -> Maybe (Direction2d coordinates)
direction givenVector =
    let
        vectorLength =
            length givenVector
    in
    if vectorLength == Quantity.zero then
        Nothing

    else
        let
            ( vx, vy ) =
                components givenVector
        in
        Just <|
            Direction2d.unsafeFromComponents
                ( Quantity.ratio vx vectorLength
                , Quantity.ratio vy vectorLength
                )


{-| Attempt to find the length and direction of a vector. In the case of a zero
vector, returns `Nothing`.

    vector =
        Vector2d.fromComponents ( 1, 1 )

    Vector2d.lengthAndDirection vector
    --> Just
    -->     ( 1.4142
    -->     , Direction2d.fromAngle (degrees 45)
    -->     )

    Vector2d.lengthAndDirection Vector2d.zero
    --> Nothing

-}
lengthAndDirection : Vector2d units coordinates -> Maybe ( Quantity Float units, Direction2d coordinates )
lengthAndDirection givenVector =
    let
        vectorLength =
            length givenVector
    in
    if vectorLength == Quantity.zero then
        Nothing

    else
        let
            ( vx, vy ) =
                components givenVector

            vectorDirection =
                Direction2d.unsafeFromComponents
                    ( Quantity.ratio vx vectorLength
                    , Quantity.ratio vy vectorLength
                    )
        in
        Just ( vectorLength, vectorDirection )


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
normalize givenVector =
    let
        vectorLength =
            length givenVector
    in
    if vectorLength == Quantity.zero then
        zero

    else
        let
            ( vx, vy ) =
                components givenVector
        in
        fromComponents
            ( Quantity.float (Quantity.ratio vx vectorLength)
            , Quantity.float (Quantity.ratio vy vectorLength)
            )


{-| Find the sum of two vectors.

    firstVector =
        Vector2d.fromComponents ( 1, 2 )

    secondVector =
        Vector2d.fromComponents ( 3, 4 )

    Vector2d.sum firstVector secondVector
    --> Vector2d.fromComponents ( 4, 6 )

-}
plus : Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates
plus secondVector firstVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    fromComponents
        ( x1 |> Quantity.plus x2
        , y1 |> Quantity.plus y2
        )


{-| Find the difference between two vectors (the first vector minus the second).

    firstVector =
        Vector2d.fromComponents ( 5, 6 )

    secondVector =
        Vector2d.fromComponents ( 1, 3 )

    Vector2d.difference firstVector secondVector
    --> Vector2d.fromComponents ( 4, 3 )

-}
minus : Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates
minus secondVector firstVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    fromComponents
        ( x1 |> Quantity.minus x2
        , y1 |> Quantity.minus y2
        )


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
dot secondVector firstVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    (x1 |> Quantity.times x2) |> Quantity.plus (y1 |> Quantity.times y2)


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
cross secondVector firstVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    (x1 |> Quantity.times y2) |> Quantity.minus (y1 |> Quantity.times x2)


{-| Reverse the direction of a vector, negating its components.

    Vector2d.reverse (Vector2d.fromComponents ( -1, 2 ))
    --> Vector2d.fromComponents ( 1, -2 )

(This could have been called `negate`, but `reverse` is more consistent with
the naming used in other modules.)

-}
reverse : Vector2d units coordinates -> Vector2d units coordinates
reverse givenVector =
    let
        ( x, y ) =
            components givenVector
    in
    fromComponents ( Quantity.negate x, Quantity.negate y )


{-| Scale the length of a vector by a given scale.

    Vector2d.scaleBy 3 (Vector2d.fromComponents ( 1, 2 ))
    --> Vector2d.fromComponents ( 3, 6 )

(This could have been called `multiply` or `times`, but `scaleBy` was chosen as
a more geometrically meaningful name and to be consistent with the `scaleAbout`
name used in other modules.)

-}
scaleBy : Float -> Vector2d units coordinates -> Vector2d units coordinates
scaleBy givenScale givenVector =
    let
        ( x, y ) =
            components givenVector
    in
    fromComponents
        ( Quantity.multiplyBy givenScale x
        , Quantity.multiplyBy givenScale y
        )


{-| Rotate a vector counterclockwise by a given angle (in radians).

    Vector2d.fromComponents ( 1, 1 )
        |> Vector2d.rotateBy (degrees 45)
    --> Vector2d.fromComponents ( 0, 1.4142 )

    Vector2d.fromComponents ( 1, 0 )
        |> Vector2d.rotateBy pi
    --> Vector2d.fromComponents ( -1, 0 )

-}
rotateBy : Angle -> Vector2d units coordinates -> Vector2d units coordinates
rotateBy givenAngle givenVector =
    let
        c =
            Angle.cos givenAngle

        s =
            Angle.sin givenAngle

        ( x, y ) =
            components givenVector
    in
    fromComponents
        ( Quantity.aXbY c x -s y
        , Quantity.aXbY s x c y
        )


{-| Rotate the given vector 90 degrees counterclockwise;

    Vector2d.rotateCounterclockwise vector

is equivalent to

    Vector2d.rotateBy (degrees 90) vector

but is more efficient.

-}
rotateCounterclockwise : Vector2d units coordinates -> Vector2d units coordinates
rotateCounterclockwise givenVector =
    let
        ( x, y ) =
            components givenVector
    in
    fromComponents ( Quantity.negate y, x )


{-| Rotate the given vector 90 degrees clockwise;

    Vector2d.rotateClockwise vector

is equivalent to

    Vector2d.rotateBy (degrees -90) vector

but is more efficient.

-}
rotateClockwise : Vector2d units coordinates -> Vector2d units coordinates
rotateClockwise givenVector =
    let
        ( x, y ) =
            components givenVector
    in
    fromComponents ( y, Quantity.negate x )


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
mirrorAcross givenAxis givenVector =
    let
        ( dx, dy ) =
            Direction2d.components (Axis2d.direction givenAxis)

        yy =
            1 - 2 * dy * dy

        xy =
            2 * dx * dy

        xx =
            1 - 2 * dx * dx

        ( vx, vy ) =
            components givenVector
    in
    fromComponents
        ( Quantity.aXbY yy vx xy vy
        , Quantity.aXbY xy vx xx vy
        )


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
projectionIn givenDirection givenVector =
    givenDirection |> withLength (givenVector |> componentIn givenDirection)


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
projectOnto givenAxis givenVector =
    projectionIn (Axis2d.direction givenAxis) givenVector


{-| Take a vector defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Vector2d.fromComponents ( 2, 0 )
        |> Vector2d.relativeTo rotatedFrame
    --> Vector2d.fromComponents ( 1.732, -1 )

-}
relativeTo : Frame2d frameUnits globalCoordinates localCoordinates -> Vector2d units globalCoordinates -> Vector2d units localCoordinates
relativeTo givenFrame givenVector =
    fromComponents
        ( componentIn (Frame2d.xDirection givenFrame) givenVector
        , componentIn (Frame2d.yDirection givenFrame) givenVector
        )


{-| Take a vector defined in local coordinates relative to a given reference
frame, and return that vector expressed in global coordinates.

    Vector2d.fromComponents ( 2, 0 )
        |> Vector2d.placeIn rotatedFrame
    --> Vector2d.fromComponents ( 1.732, 1 )

-}
placeIn : Frame2d frameUnits globalCoordinates localCoordinates -> Vector2d units localCoordinates -> Vector2d units globalCoordinates
placeIn givenFrame givenVector =
    let
        ( x1, y1 ) =
            Direction2d.components (Frame2d.xDirection givenFrame)

        ( x2, y2 ) =
            Direction2d.components (Frame2d.yDirection givenFrame)

        ( x, y ) =
            components givenVector
    in
    fromComponents
        ( Quantity.aXbY x1 x x2 y
        , Quantity.aXbY y1 x y2 y
        )
