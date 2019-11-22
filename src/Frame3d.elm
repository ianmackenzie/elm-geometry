--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Frame3d exposing
    ( Frame3d
    , atOrigin
    , withXDirection, withYDirection, withZDirection, atPoint, copy, unsafe
    , fromXAxis, fromYAxis, fromZAxis
    , originPoint, xDirection, yDirection, zDirection, isRightHanded
    , xAxis, yAxis, zAxis
    , xyPlane, yxPlane, yzPlane, zyPlane, zxPlane, xzPlane
    , xySketchPlane, yxSketchPlane, yzSketchPlane, zySketchPlane, zxSketchPlane, xzSketchPlane
    , reverseX, reverseY, reverseZ, moveTo, rotateAround, rotateAroundOwn, translateBy, translateIn, translateAlongOwn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| A `Frame3d` has an origin point and a set of X, Y and Z directions (which
are always perpendicular to each other). It can be thought of as:

  - A local coordinate system: Most geometric types have associated `relativeTo`
    and `placeIn` functions that convert values of that type from global
    coordinates to local coordinates in a particular frame, and vice versa.
  - A set of axes and planes: It is often convenient to (for example) rotate
    around the Z axis of a frame, or mirror across its XY plane. Frames can
    also themselves be translated, rotated and mirrored!
  - A combined 3D position and orientation: For example, a `Frame3d` could be
    used to define the position and orientation of a spaceship in a 3D game.
    Movement of the ship would then be done by translating and rotating the
    frame.

@docs Frame3d


# Constants

@docs atOrigin


# Constructors

The `withXDirection`, `withYDirection` and `withZDirection` functions all
construct a new `Frame3d` with the given axis direction, having the given origin
point. The other two directions will be chosen arbitrarily. This can be useful
when constructing 'scratch' frames where (for example) you want a particular Z
direction but the specific X/Y directions are unimportant:

    zDirection =
        Direction3d.xz (Angle.degrees 60)

    frame =
        Frame3d.withZDirection zDirection Point3d.origin

    Frame3d.zDirection frame
    --> Direction3d.xz (Angle.degrees 60)

    Frame3d.originPoint frame
    --> Point3d.origin

    Frame3d.xDirection frame
    --> Direction3d.xz (Angle.degrees -30)

    Frame3d.yDirection frame
    --> Direction3d.y

No guarantees are given about the other two directions other than that the three
directions will be mutually perpendicular, and will be oriented so that the
resulting frame is [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).

@docs withXDirection, withYDirection, withZDirection, atPoint, copy, unsafe


## From axes

@docs fromXAxis, fromYAxis, fromZAxis


# Properties

@docs originPoint, xDirection, yDirection, zDirection, isRightHanded


## Axes

@docs xAxis, yAxis, zAxis


## Planes

The following functions all return planes with the same origin point as the
given frame, but with varying normal directions. In each case the normal
direction of the resulting plane is given by the cross product of the two
indicated basis directions (assuming a right-handed frame); for example,

    Frame3d.xyPlane Frame3d.atOrigin
    --> Plane3d.through Point3d.origin
    -->     Direction3d.positiveZ

since the cross product of the X and Y basis directions of a frame is equal to
its Z basis direction. And since reversing the order of arguments in a cross
product reverses the sign of the result,

    Frame3d.yxPlane Frame3d.atOrigin
    --> Plane3d.through Point3d.origin
    -->     Direction3d.negativeZ

@docs xyPlane, yxPlane, yzPlane, zyPlane, zxPlane, xzPlane


## Sketch planes

These functions all form a `SketchPlane3d` from two axes of the given frame. The
X and Y axes of the sketch plane will correspond to the two indicated axes. For
example,

    yzSketchPlane =
        Frame3d.yzSketchPlane Frame3d.atOrigin

    SketchPlane3d.originPoint yzSketchPlane
    --> Point3d.origin

    SketchPlane3d.xDirection yzSketchPlane
    --> Direction3d.y

    SketchPlane3d.yDirection yzSketchPlane
    --> Direction3d.z

Note that this can be confusing - for example, a local X coordinate in the above
sketch plane corresponds to a global Y coordinate, and a local Y coordinate
corresponds to a global Z coordinate!

@docs xySketchPlane, yxSketchPlane, yzSketchPlane, zySketchPlane, zxSketchPlane, xzSketchPlane


# Transformations

@docs reverseX, reverseY, reverseZ, moveTo, rotateAround, rotateAroundOwn, translateBy, translateIn, translateAlongOwn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Rate)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| The type parameters of a `Frame3d` indicate what units and coordinate
systems it's defined in, and what coordinate system (if any) it itself defines.
A concrete `Frame3d` type might look like

    type alias Frame =
        Frame3d Meters World { defines : Local }

which can be read as "a `Frame3d` defined in meters in world coordinates, which
itself defines local coordinates". For frames that don't define a local
coordinate system, you could use

    type alias Frame =
        Frame3d Meters World {}

Many functions in this module don't care about the third type argument (whether
it's a record with a `defines` field like in the first example, an empty record
like in the second example, or even something else entirely) but functions like
`placeIn` and `relativeTo` expect the `{ defines : localCoordinates }` pattern.

-}
type alias Frame3d units coordinates defines =
    Types.Frame3d units coordinates defines


{-| The global XYZ frame, centered at the origin.

    Frame3d.originPoint Frame3d.atOrigin
    --> Point3d.origin

    Frame3d.xDirection Frame3d.atOrigin
    --> Direction3d.x

    Frame3d.yDirection Frame3d.atOrigin
    --> Direction3d.y

    Frame3d.zDirection Frame3d.atOrigin
    --> Direction3d.z

-}
atOrigin : Frame3d units coordinates defines
atOrigin =
    Types.Frame3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        , zDirection = Direction3d.z
        }


{-| Construct a frame with the given origin point and X direction.
-}
withXDirection : Direction3d coordinates -> Point3d units coordinates -> Frame3d units coordinates defines
withXDirection givenXDirection givenOrigin =
    let
        ( computedYDirection, computedZDirection ) =
            Direction3d.perpendicularBasis givenXDirection
    in
    unsafe
        { originPoint = givenOrigin
        , xDirection = givenXDirection
        , yDirection = computedYDirection
        , zDirection = computedZDirection
        }


{-| Construct a frame with the given origin point and Y direction.
-}
withYDirection : Direction3d coordinates -> Point3d units coordinates -> Frame3d units coordinates defines
withYDirection givenYDirection givenOrigin =
    let
        ( computedZDirection, computedXDirection ) =
            Direction3d.perpendicularBasis givenYDirection
    in
    unsafe
        { originPoint = givenOrigin
        , xDirection = computedXDirection
        , yDirection = givenYDirection
        , zDirection = computedZDirection
        }


{-| Construct a frame with the given origin point and Z direction.
-}
withZDirection : Direction3d coordinates -> Point3d units coordinates -> Frame3d units coordinates defines
withZDirection givenZDirection givenOrigin =
    let
        ( computedXDirection, computedYDirection ) =
            Direction3d.perpendicularBasis givenZDirection
    in
    unsafe
        { originPoint = givenOrigin
        , xDirection = computedXDirection
        , yDirection = computedYDirection
        , zDirection = givenZDirection
        }


{-| Create a 'fresh copy' of a frame: one with the same origin point and X/Y/Z
directions, but that can be used to define a different local coordinate system.
Sometimes useful in generic/library code. Despite the name, this is efficient:
it really just returns the value you passed in, but with a different type.
-}
copy : Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
copy (Types.Frame3d properties) =
    Types.Frame3d properties


{-| Construct a frame directly from its origin point and X, Y and Z directions:

    frame =
        Frame3d.unsafe
            { originPoint = Point3d.meters 2 1 3
            , xDirection =
                Direction3d.unsafe
                    { x = 0.8, y = 0.6, z = 0 }
            , yDirection =
                Direction3d.unsafe
                    { x = -0.6, y = 0.8, z = 0 }
            , zDirection =
                Direction3d.unsafe
                    { x = 0, y = 0, z = 1 }
            }

In this case **you must be careful to ensure that the X, Y and Z directions are
perpendicular**. (You will likely also want to make sure that they form a
[right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
coordinate system.) To construct sets of mutually perpendicular directions,
[`Direction3d.orthonormalize`](Direction3d#orthonormalize),
[`Direction3d.orthogonalize`](Direction3d#orthogonalize), or
[`Direction3d.perpendicularBasis`](Direction3d#perpendicularBasis) may be
useful.

-}
unsafe :
    { originPoint : Point3d units coordinates
    , xDirection : Direction3d coordinates
    , yDirection : Direction3d coordinates
    , zDirection : Direction3d coordinates
    }
    -> Frame3d units coordinates defines
unsafe properties =
    Types.Frame3d properties


{-| Construct a frame aligned with the global XYZ frame but with the given
origin point.

    frame =
        Frame3d.atPoint
            (Point3d.meters 2 1 3)

    Frame3d.originPoint frame
    --> Point3d.meters 2 1 3

    Frame3d.xDirection frame
    --> Direction3d.x

    Frame3d.yDirection frame
    --> Direction3d.y

    Frame3d.zDirection frame
    --> Direction3d.z

-}
atPoint : Point3d units coordinates -> Frame3d units coordinates defines
atPoint point =
    unsafe
        { originPoint = point
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        , zDirection = Direction3d.z
        }


{-| Construct a `Frame3d` with the given X axis. Perpendicular Y and Z
directions will be chosen arbitrarily.
-}
fromXAxis : Axis3d units coordinates -> Frame3d units coordinates defines
fromXAxis givenXAxis =
    let
        givenXDirection =
            Axis3d.direction givenXAxis

        ( computedYDirection, computedZDirection ) =
            Direction3d.perpendicularBasis givenXDirection
    in
    unsafe
        { originPoint = Axis3d.originPoint givenXAxis
        , xDirection = givenXDirection
        , yDirection = computedYDirection
        , zDirection = computedZDirection
        }


{-| Construct a `Frame3d` with the given Y axis. Perpendicular X and Z
directions will be chosen arbitrarily.
-}
fromYAxis : Axis3d units coordinates -> Frame3d units coordinates defines
fromYAxis givenYAxis =
    let
        givenYDirection =
            Axis3d.direction givenYAxis

        ( computedZDirection, computedXDirection ) =
            Direction3d.perpendicularBasis givenYDirection
    in
    unsafe
        { originPoint = Axis3d.originPoint givenYAxis
        , xDirection = computedXDirection
        , yDirection = givenYDirection
        , zDirection = computedZDirection
        }


{-| Construct a `Frame3d` with the given Z axis. Perpendicular X and Y
directions will be chosen arbitrarily.
-}
fromZAxis : Axis3d units coordinates -> Frame3d units coordinates defines
fromZAxis givenZAxis =
    let
        givenZDirection =
            Axis3d.direction givenZAxis

        ( computedXDirection, computedYDirection ) =
            Direction3d.perpendicularBasis givenZDirection
    in
    unsafe
        { originPoint = Axis3d.originPoint givenZAxis
        , xDirection = computedXDirection
        , yDirection = computedYDirection
        , zDirection = givenZDirection
        }


{-| Convert a frame from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Frame3d units1 coordinates defines -> Frame3d units2 coordinates defines
at rate (Types.Frame3d frame) =
    Types.Frame3d
        { originPoint = Point3d.at rate frame.originPoint
        , xDirection = frame.xDirection
        , yDirection = frame.yDirection
        , zDirection = frame.zDirection
        }


{-| Convert a frame from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Frame3d units1 coordinates defines -> Frame3d units2 coordinates defines
at_ rate frame =
    at (Quantity.inverse rate) frame


{-| Get the origin point of a given frame.
-}
originPoint : Frame3d units coordinates defines -> Point3d units coordinates
originPoint (Types.Frame3d properties) =
    properties.originPoint


{-| Get the X direction of a given frame.
-}
xDirection : Frame3d units coordinates defines -> Direction3d coordinates
xDirection (Types.Frame3d properties) =
    properties.xDirection


{-| Get the Y direction of a given frame.
-}
yDirection : Frame3d units coordinates defines -> Direction3d coordinates
yDirection (Types.Frame3d properties) =
    properties.yDirection


{-| Get the Z direction of a given frame.
-}
zDirection : Frame3d units coordinates defines -> Direction3d coordinates
zDirection (Types.Frame3d properties) =
    properties.zDirection


{-| Check if a frame is [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).

    Frame3d.isRightHanded Frame3d.atOrigin
    --> True

    Frame3d.isRightHanded
        (Frame3d.reverseZ Frame3d.atOrigin)
    --> False

All predefined frames are right-handed, and most operations on frames preserve
handedness, so about the only ways to end up with a left-handed frame are by
constructing one explicitly with `unsafe` or by mirroring a right-handed frame.

-}
isRightHanded : Frame3d units coordinates defines -> Bool
isRightHanded (Types.Frame3d frame) =
    let
        a =
            Direction3d.xComponent frame.xDirection

        b =
            Direction3d.yComponent frame.xDirection

        c =
            Direction3d.zComponent frame.xDirection

        d =
            Direction3d.xComponent frame.yDirection

        e =
            Direction3d.yComponent frame.yDirection

        f =
            Direction3d.zComponent frame.yDirection

        g =
            Direction3d.xComponent frame.zDirection

        h =
            Direction3d.yComponent frame.zDirection

        i =
            Direction3d.zComponent frame.zDirection
    in
    a * e * i + b * f * g + c * d * h - c * e * g - b * d * i - a * f * h > 0


{-| Get the X axis of a given frame (the axis formed from the frame's origin
point and X direction).
-}
xAxis : Frame3d units coordinates defines -> Axis3d units coordinates
xAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.xDirection


{-| Get the Y axis of a given frame (the axis formed from the frame's origin
point and Y direction).
-}
yAxis : Frame3d units coordinates defines -> Axis3d units coordinates
yAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.yDirection


{-| Get the Z axis of a given frame (the axis formed from the frame's origin
point and Z direction).
-}
zAxis : Frame3d units coordinates defines -> Axis3d units coordinates
zAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.zDirection


{-| Get a plane with normal direction equal to the frame's positive Z direction.
-}
xyPlane : Frame3d units coordinates defines -> Plane3d units coordinates
xyPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.zDirection


{-| Get a plane with normal direction equal to the frame's negative Z direction.
-}
yxPlane : Frame3d units coordinates defines -> Plane3d units coordinates
yxPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.zDirection)


{-| Get a plane with normal direction equal to the frame's positive X direction.
-}
yzPlane : Frame3d units coordinates defines -> Plane3d units coordinates
yzPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.xDirection


{-| Get a plane with normal direction equal to the frame's negative X direction.
-}
zyPlane : Frame3d units coordinates defines -> Plane3d units coordinates
zyPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.xDirection)


{-| Get a plane with normal direction equal to the frame's positive Y direction.
-}
zxPlane : Frame3d units coordinates defines -> Plane3d units coordinates
zxPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.yDirection


{-| Get a plane with normal direction equal to the frame's negative Y direction.
-}
xzPlane : Frame3d units coordinates defines -> Plane3d units coordinates
xzPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.yDirection)


{-| Form a sketch plane from the given frame's X and Y axes.
-}
xySketchPlane : Frame3d units coordinates defines3d -> SketchPlane3d units coordinates defines2d
xySketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| Form a sketch plane from the given frame's Y and X axes.
-}
yxSketchPlane : Frame3d units coordinates defines3d -> SketchPlane3d units coordinates defines2d
yxSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = xDirection frame
        }


{-| Form a sketch plane from the given frame's Y and Z axes.
-}
yzSketchPlane : Frame3d units coordinates defines3d -> SketchPlane3d units coordinates defines2d
yzSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = zDirection frame
        }


{-| Form a sketch plane from the given frame's Z and Y axes.
-}
zySketchPlane : Frame3d units coordinates defines3d -> SketchPlane3d units coordinates defines2d
zySketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = yDirection frame
        }


{-| Form a sketch plane from the given frame's Z and X axes.
-}
zxSketchPlane : Frame3d units coordinates defines3d -> SketchPlane3d units coordinates defines2d
zxSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = xDirection frame
        }


{-| Form a sketch plane from the given frame's X and Z axes.
-}
xzSketchPlane : Frame3d units coordinates defines3d -> SketchPlane3d units coordinates defines2d
xzSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = zDirection frame
        }


{-| Reverse the X direction of a frame. Note that this will switch the
[handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.
-}
reverseX : Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
reverseX frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = Direction3d.reverse (xDirection frame)
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Reverse the Y direction of a frame. Note that this will switch the
[handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.
-}
reverseY : Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
reverseY frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = Direction3d.reverse (yDirection frame)
        , zDirection = zDirection frame
        }


{-| Reverse the Z direction of a frame. Note that this will switch the
[handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.
-}
reverseZ : Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
reverseZ frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = Direction3d.reverse (zDirection frame)
        }


{-| Move a frame so that it has the given origin point but unchanged
orientation.

    point =
        Point3d.meters 2 1 3

    Frame3d.atOrigin |> Frame3d.moveTo point
    --> Frame3d.atPoint point

-}
moveTo : Point3d units coordinates -> Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
moveTo newOrigin frame =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Rotate a frame around an axis by a given angle. The frame's origin point and
basis directions will all be rotated around the given axis.

    frame =
        Frame3d.atPoint (Point3d.meters 2 1 3)

    rotatedFrame =
        frame
            |> Frame3d.rotateAround Axis3d.z
                (Angle.degrees 90)

    Frame3d.originPoint rotatedFrame
    --> Point3d.meters -1 2 3

    Frame3d.xDirection rotatedFrame
    --> Direction3d.y

    Frame3d.yDirection rotatedFrame
    --> Direction3d.negativeX

    Frame3d.zDirection rotatedFrame
    --> Direction3d.z

-}
rotateAround : Axis3d units coordinates -> Angle -> Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
rotateAround axis angle frame =
    unsafe
        { originPoint = Point3d.rotateAround axis angle (originPoint frame)
        , xDirection = Direction3d.rotateAround axis angle (xDirection frame)
        , yDirection = Direction3d.rotateAround axis angle (yDirection frame)
        , zDirection = Direction3d.rotateAround axis angle (zDirection frame)
        }


{-| Rotate a frame around one of its own axes by a given angle.

The first argument is a function that returns the axis to rotate around, given
the current frame. The majority of the time this will be either `Frame3d.xAxis`,
`Frame3d.yAxis` or `Frame3d.zAxis`. Compare the following to the above example
for `rotateAround`:

    frame =
        Frame3d.atPoint (Point3d.meters 2 1 3)

    rotatedFrame =
        frame
            |> Frame3d.rotateAroundOwn Frame3d.zAxis
                (Angle.degrees 90)

    Frame3d.originPoint rotatedFrame
    --> Point3d.meters 2 1 3

    Frame3d.xDirection rotatedFrame
    --> Direction3d.y

    Frame3d.yDirection rotatedFrame
    --> Direction3d.negativeX

    Frame3d.zDirection rotatedFrame
    --> Direction3d.z

Since the rotation is done around the frame's own Z axis (which passes through
the frame's origin point), the origin point remains the same after rotation.

In this example the frame's Z axis has the same orientation as the global Z axis
so the frame's basis directions are rotated the same way, but in more complex
examples involving rotated frames a rotation around (for example) the frame's
own Z axis may be completely different from a rotation around the global Z axis.

-}
rotateAroundOwn : (Frame3d units coordinates defines1 -> Axis3d units coordinates) -> Angle -> Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
rotateAroundOwn axis angle frame =
    rotateAround (axis frame) angle frame


{-| Translate a frame by a given displacement.

    frame =
        Frame3d.atPoint (Point3d.meters 2 1 3)

    displacement =
        Vector3d.meters 1 1 1

    Frame3d.translateBy displacement frame
    --> Frame3d.atPoint (Point3d.meters 3 2 4)

-}
translateBy : Vector3d units coordinates -> Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
translateBy vector frame =
    unsafe
        { originPoint = Point3d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Translate a frame in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
translateIn direction distance frame =
    translateBy (Vector3d.withLength distance direction) frame


{-| Translate a frame along one of its own axes by a given distance.

The first argument is a function that returns the axis to translate along, given
the current frame. The majority of the time this will be either `Frame3d.xAxis`,
`Frame3d.yAxis` or `Frame3d.zAxis`.

This function is convenient when constructing frames via a series of
transformations. For example,

    point =
        Point3d.meters 2 0 0

    frame =
        Frame3d.atPoint point
            |> Frame3d.rotateAroundOwn Frame3d.zAxis
                (Angle.degrees 45)
            |> Frame3d.translateAlongOwn Frame3d.xAxis
                (Length.meters 2)

means "construct a frame at the point (2, 0, 0), rotate it around its own Z axis
counterclockwise by 45 degrees, then translate it along its own (rotated) X axis
by 2 meters", resulting in

    Frame3d.originPoint frame
    --> Point3d.meters 3.4142 1.4142 0

    Frame3d.xDirection frame
    --> Direction3d.xy (Angle.degrees 45)

    Frame3d.yDirection frame
    --> Direction3d.xy (Angle.degrees 135)

    Frame3d.zDirection frame
    --> Direction3d.z

-}
translateAlongOwn : (Frame3d units coordinates defines1 -> Axis3d units coordinates) -> Quantity Float units -> Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
translateAlongOwn axis distance frame =
    frame |> translateIn (Axis3d.direction (axis frame)) distance


{-| Mirror a frame across a plane.

    frame =
        Frame3d.atPoint (Point3d.meters 2 1 3)

    mirroredFrame =
        Frame3d.mirrorAcross Plane3d.xy frame

    Frame3d.originPoint mirroredFrame
    --> Point3d.meters 2 1 -3

    Frame3d.xDirection mirroredFrame
    --> Direction3d.x

    Frame3d.yDirection mirroredFrame
    --> Direction3d.y

    Frame3d.zDirection mirroredFrame
    --> Direction3d.negativeZ

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
mirrorAcross : Plane3d units coordinates -> Frame3d units coordinates defines1 -> Frame3d units coordinates defines2
mirrorAcross plane frame =
    unsafe
        { originPoint = Point3d.mirrorAcross plane (originPoint frame)
        , xDirection = Direction3d.mirrorAcross plane (xDirection frame)
        , yDirection = Direction3d.mirrorAcross plane (yDirection frame)
        , zDirection = Direction3d.mirrorAcross plane (zDirection frame)
        }


{-| Take two frames defined in global coordinates, and return the second one
expressed in local coordinates relative to the first.
-}
relativeTo :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Frame3d units globalCoordinates defines
    -> Frame3d units localCoordinates defines
relativeTo otherFrame frame =
    Types.Frame3d
        { originPoint = Point3d.relativeTo otherFrame (originPoint frame)
        , xDirection = Direction3d.relativeTo otherFrame (xDirection frame)
        , yDirection = Direction3d.relativeTo otherFrame (yDirection frame)
        , zDirection = Direction3d.relativeTo otherFrame (zDirection frame)
        }


{-| Take one frame defined in global coordinates and a second frame defined
in local coordinates relative to the first frame, and return the second frame
expressed in global coordinates.
-}
placeIn :
    Frame3d units globalCoordinates { defines : localCoordinates }
    -> Frame3d units localCoordinates defines
    -> Frame3d units globalCoordinates defines
placeIn otherFrame frame =
    Types.Frame3d
        { originPoint = Point3d.placeIn otherFrame (originPoint frame)
        , xDirection = Direction3d.placeIn otherFrame (xDirection frame)
        , yDirection = Direction3d.placeIn otherFrame (yDirection frame)
        , zDirection = Direction3d.placeIn otherFrame (zDirection frame)
        }
