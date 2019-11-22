--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Frame2d exposing
    ( Frame2d
    , atOrigin
    , atPoint, withAngle, withXDirection, withYDirection, fromXAxis, fromYAxis, copy, unsafe
    , originPoint, xDirection, yDirection, isRightHanded, xAxis, yAxis
    , reverseX, reverseY, moveTo, rotateBy, rotateAround, translateBy, translateIn, translateAlongOwn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| A `Frame2d` has an origin point and a pair of X and Y directions (which are
always perpendicular to each other). It can be thought of as:

  - A local coordinate system: Most geometric types have associated `relativeTo`
    and `placeIn` functions that convert values of that type from global
    coordinates to local coordinates in a particular frame, and vice versa.
  - A pair of X and Y axes: It is often convenient to (for example) mirror
    across the X axis of a frame, or project onto its Y axis. Frames can
    also themselves be translated, rotated and mirrored!
  - A combined 2D position and orientation: For example, a `Frame2d` could be
    used to define the position and orientation of a spaceship in a 2D game.
    Movement of the ship would then be done by translating and rotating the
    frame.

@docs Frame2d


# Constants

@docs atOrigin


# Constructors

@docs atPoint, withAngle, withXDirection, withYDirection, fromXAxis, fromYAxis, copy, unsafe


# Properties

@docs originPoint, xDirection, yDirection, isRightHanded, xAxis, yAxis


# Transformations

@docs reverseX, reverseY, moveTo, rotateBy, rotateAround, translateBy, translateIn, translateAlongOwn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Vector2d exposing (Vector2d)


{-| The type parameters of a `Frame2d` indicate what units and coordinate
systems it's defined in, and what coordinate system (if any) it itself defines.
A concrete `Frame2d` type might look like

    type alias Frame =
        Frame2d Meters World { defines : Local }

which can be read as "a `Frame2d` defined in meters in world coordinates, which
itself defines local coordinates". For frames that don't define a local
coordinate system, you could use

    type alias Frame =
        Frame2d Meters World {}

Many functions in this module don't care about the third type argument (whether
it's a record with a `defines` field like in the first example, an empty record
like in the second example, or even something else entirely) but functions like
`placeIn` and `relativeTo` expect the `{ defines : localCoordinates }` pattern.

-}
type alias Frame2d units coordinates defines =
    Types.Frame2d units coordinates defines


{-| The global XY frame, centered at the origin.

    Frame2d.originPoint Frame2d.atOrigin
    --> Point2d.origin

    Frame2d.xDirection Frame2d.atOrigin
    --> Direction2d.x

    Frame2d.yDirection Frame2d.atOrigin
    --> Direction2d.y

-}
atOrigin : Frame2d units coordinates defines
atOrigin =
    Types.Frame2d
        { originPoint = Point2d.origin
        , xDirection = Direction2d.x
        , yDirection = Direction2d.y
        }


{-| Construct a frame with the given X axis direction, having the given origin
point. The Y axis direction will be constructed by rotating the given X
direction 90 degrees counterclockwise:

    frame =
        Frame2d.withXDirection (Direction2d.degrees 30)
            (Point2d.meters 2 3)

    Frame2d.yDirection frame
    --> Direction2d.degrees 120

-}
withXDirection : Direction2d coordinates -> Point2d units coordinates -> Frame2d units coordinates defines
withXDirection givenDirection givenOrigin =
    unsafe
        { originPoint = givenOrigin
        , xDirection = givenDirection
        , yDirection = givenDirection |> Direction2d.rotateCounterclockwise
        }


{-| Construct a frame with the given Y axis direction, having the given origin
point. The X axis direction will be constructed by rotating the given Y
direction 90 degrees clockwise:

    frame =
        Frame2d.withYDirection (Direction2d.degrees 30)
            (Point2d.meters 2 3)

    Frame2d.xDirection frame
    --> Direction2d.degrees -60

-}
withYDirection : Direction2d coordinates -> Point2d units coordinates -> Frame2d units coordinates defines
withYDirection givenDirection givenOrigin =
    unsafe
        { originPoint = givenOrigin
        , xDirection = givenDirection |> Direction2d.rotateClockwise
        , yDirection = givenDirection
        }


{-| Construct a `Frame2d` given its X axis;

    Frame2d.fromXAxis axis

is equivalent to

    Frame2d.withXDirection (Axis2d.direction axis)
        (Axis2d.originPoint axis)

-}
fromXAxis : Axis2d units coordinates -> Frame2d units coordinates defines
fromXAxis givenAxis =
    withXDirection (Axis2d.direction givenAxis) (Axis2d.originPoint givenAxis)


{-| Construct a `Frame2d` given its Y axis;

    Frame2d.fromYAxis axis

is equivalent to

    Frame2d.withYDirection (Axis2d.direction axis)
        (Axis2d.originPoint axis)

-}
fromYAxis : Axis2d units coordinates -> Frame2d units coordinates defines
fromYAxis givenAxis =
    withYDirection (Axis2d.direction givenAxis) (Axis2d.originPoint givenAxis)


{-| Create a 'fresh copy' of a frame: one with the same origin point and X/Y
directions, but that can be used to define a different local coordinate system.
Sometimes useful in generic/library code. Despite the name, this is efficient:
it really just returns the value you passed in, but with a different type.
-}
copy : Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
copy (Types.Frame2d properties) =
    Types.Frame2d properties


{-| Construct a frame directly from its origin point and X and Y directions:

    frame =
        Frame2d.unsafe
            { originPoint = Point2d.meters 2 3
            , xDirection = Direction2d.degrees 45
            , yDirection = Direction2d.degrees 135
            }

In this case **you must be careful to ensure that the X and Y directions are
perpendicular**. To construct pairs of perpendicular directions,
[`Direction2d.orthonormalize`](Direction2d#orthonormalize) or
[`Direction2d.orthogonalize`](Direction2d#orthogonalize) may be useful.

-}
unsafe :
    { originPoint : Point2d units coordinates
    , xDirection : Direction2d coordinates
    , yDirection : Direction2d coordinates
    }
    -> Frame2d units coordinates defines
unsafe properties =
    Types.Frame2d properties


{-| Construct a frame aligned with the global XY frame but with the given origin
point.

    frame =
        Frame2d.atPoint (Point2d.meters 2 3)

    Frame2d.originPoint frame
    --> Point2d.meters 2 3

    Frame2d.xDirection frame
    --> Direction2d.x

    Frame2d.yDirection frame
    --> Direction2d.y

-}
atPoint : Point2d units coordinates -> Frame2d units coordinates defines
atPoint point =
    unsafe
        { originPoint = point
        , xDirection = Direction2d.x
        , yDirection = Direction2d.y
        }


{-| Construct a frame with the given angle and origin point. The angle is
the amount the returned frame will be rotated relative to the global XY frame,
or equivalently the angle of the frame's X direction;

    Frame2d.withAngle angle point

is equivalent to

    Frame2d.withXDirection
        (Direction2d.fromAngle givenAngle)
        point

-}
withAngle : Angle -> Point2d units coordinates -> Frame2d units coordinates defines
withAngle givenAngle givenOrigin =
    withXDirection (Direction2d.fromAngle givenAngle) givenOrigin


{-| Convert a frame from one units type to another, by providing a conversion
factor given as a rate of change of destination units with respect to source
units.
-}
at : Quantity Float (Rate units2 units1) -> Frame2d units1 coordinates defines -> Frame2d units2 coordinates defines
at rate (Types.Frame2d frame) =
    Types.Frame2d
        { originPoint = Point2d.at rate frame.originPoint
        , xDirection = frame.xDirection
        , yDirection = frame.yDirection
        }


{-| Convert a frame from one units type to another, by providing an 'inverse'
conversion factor given as a rate of change of source units with respect to
destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> Frame2d units1 coordinates defines -> Frame2d units2 coordinates defines
at_ rate frame =
    at (Quantity.inverse rate) frame


{-| Get the origin point of a given frame.
-}
originPoint : Frame2d units coordinates defines -> Point2d units coordinates
originPoint (Types.Frame2d frame) =
    frame.originPoint


{-| Get the X direction of a given frame.
-}
xDirection : Frame2d units coordinates defines -> Direction2d coordinates
xDirection (Types.Frame2d frame) =
    frame.xDirection


{-| Get the Y direction of a given frame.
-}
yDirection : Frame2d units coordinates defines -> Direction2d coordinates
yDirection (Types.Frame2d frame) =
    frame.yDirection


{-| Check if a frame is [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).

    Frame2d.isRightHanded Frame2d.atOrigin
    --> True

    Frame2d.isRightHanded
        (Frame2d.reverseX Frame2d.atOrigin)
    --> False

All predefined frames are right-handed, and most operations on frames preserve
handedness, so about the only ways to end up with a left-handed frame are by
constructing one explicitly with `unsafe` or by mirroring a right-handed frame.

-}
isRightHanded : Frame2d units coordinates defines -> Bool
isRightHanded (Types.Frame2d frame) =
    let
        x1 =
            Direction2d.xComponent frame.xDirection

        y1 =
            Direction2d.yComponent frame.xDirection

        x2 =
            Direction2d.xComponent frame.yDirection

        y2 =
            Direction2d.yComponent frame.yDirection
    in
    x1 * y2 - y1 * x2 > 0


{-| Get the X axis of a given frame (the axis formed from the frame's origin
point and X direction).
-}
xAxis : Frame2d units coordinates defines -> Axis2d units coordinates
xAxis (Types.Frame2d frame) =
    Axis2d.through frame.originPoint frame.xDirection


{-| Get the Y axis of a given frame (the axis formed from the frame's origin
point and Y direction).
-}
yAxis : Frame2d units coordinates defines -> Axis2d units coordinates
yAxis (Types.Frame2d frame) =
    Axis2d.through frame.originPoint frame.yDirection


{-| Reverse the X direction of a frame, leaving its Y direction and origin point
the same. Note that this will switch the
[handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.
-}
reverseX : Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
reverseX frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = Direction2d.reverse (xDirection frame)
        , yDirection = yDirection frame
        }


{-| Reverse the Y direction of a frame, leaving its X direction and origin point
the same. Note that this will switch the
[handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.
-}
reverseY : Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
reverseY frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = Direction2d.reverse (yDirection frame)
        }


{-| Move a frame so that it has the given origin point.

    point =
        Point2d.meters 1 1

    Frame2d.atOrigin |> Frame2d.moveTo point
    --> Frame2d.atPoint point

-}
moveTo : Point2d units coordinates -> Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
moveTo newOrigin frame =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| Rotate a frame counterclockwise by a given angle around the frame's own
origin point. The resulting frame will have the same origin point, and its X and
Y directions will be rotated by the given angle.

    rotatedFrame =
        Frame2d.atOrigin
            |> Frame2d.rotateBy (Angle.degrees 30)

    Frame2d.xDirection rotatedFrame
    --> Direction2d.degrees 30

    Frame2d.yDirection rotatedFrame
    --> Direction2d.degrees 120

-}
rotateBy : Angle -> Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
rotateBy angle frame =
    let
        rotateDirection =
            Direction2d.rotateBy angle
    in
    unsafe
        { originPoint = originPoint frame
        , xDirection = rotateDirection (xDirection frame)
        , yDirection = rotateDirection (yDirection frame)
        }


{-| Rotate a frame counterclockwise around a given point by a given angle. The
frame's origin point will be rotated around the given point by the given angle,
and its X and Y basis directions will be rotated by the given angle.

    rotatedFrame =
        Frame2d.atPoint (Point2d.meters 1 1)
            |> Frame2d.rotateAround Point2d.origin
                (Angle.degrees 45)

    Frame2d.originPoint rotatedFrame
    --> Point2d.meters 0 1.4142

    Frame2d.xDirection rotatedFrame
    --> Direction2d.degrees 45

    Frame2d.yDirection rotatedFrame
    --> Direction2d.degrees 135

-}
rotateAround : Point2d units coordinates -> Angle -> Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \frame ->
        unsafe
            { originPoint = rotatePoint (originPoint frame)
            , xDirection = rotateDirection (xDirection frame)
            , yDirection = rotateDirection (yDirection frame)
            }


{-| Translate a frame by a given displacement.

    frame =
        Frame2d.atPoint (Point2d.meters 2 3)

    displacement =
        Vector2d.meters 1 1

    Frame2d.translateBy displacement frame
    --> Frame2d.atPoint (Point2d.meters 3 4)

-}
translateBy : Vector2d units coordinates -> Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
translateBy vector frame =
    unsafe
        { originPoint = Point2d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| Translate a frame in a given direction by a given distance.
-}
translateIn : Direction2d coordinates -> Quantity Float units -> Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
translateIn direction distance frame =
    translateBy (Vector2d.withLength distance direction) frame


{-| Translate a frame along one of its own axes by a given distance.

The first argument is a function that returns the axis to translate along, given
the current frame. The majority of the time this argument will be either
`Frame2d.xAxis` or `Frame2d.yAxis`. The second argument is the distance to
translate along the given axis.

This function is convenient when constructing frames via a series of
transformations. For example,

    frame =
        Frame2d.atPoint (Point2d.meters 2 0)
            |> Frame2d.rotateBy (Angle.degrees 45)
            |> Frame2d.translateAlongOwn Frame2d.xAxis
                (Length.meters 2)

means "construct a frame at the point (2, 0), rotate it around its own origin
point by 45 degrees, then translate it along its own X axis by 2 meters",
resulting in

    Frame2d.originPoint frame
    --> Point2d.meters 3.4142 1.4142

    Frame2d.xDirection frame
    --> Direction2d.degrees 45

    Frame2d.yDirection frame
    --> Direction2d.degrees 135

-}
translateAlongOwn : (Frame2d units coordinates defines1 -> Axis2d units coordinates) -> Quantity Float units -> Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
translateAlongOwn axis distance frame =
    frame |> translateIn (Axis2d.direction (axis frame)) distance


{-| Mirror a frame across an axis.

    frame =
        Frame2d.atPoint (Point2d.meters 2 3)

    mirroredFrame =
        Frame2d.mirrorAcross Axis2d.x frame

    Frame2d.originPoint mirroredFrame
    --> Point2d.meters 2 -3

    Frame2d.xDirection mirroredFrame
    --> Direction2d.x

    Frame2d.yDirection mirroredFrame
    --> Direction2d.negativeY

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
mirrorAcross : Axis2d units coordinates -> Frame2d units coordinates defines1 -> Frame2d units coordinates defines2
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAcross axis
    in
    \frame ->
        unsafe
            { originPoint = mirrorPoint (originPoint frame)
            , xDirection = mirrorDirection (xDirection frame)
            , yDirection = mirrorDirection (yDirection frame)
            }


{-| Take two frames defined in global coordinates, and return the second one
expressed in local coordinates relative to the first.
-}
relativeTo :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Frame2d units globalCoordinates defines
    -> Frame2d units localCoordinates defines
relativeTo otherFrame frame =
    Types.Frame2d
        { originPoint = Point2d.relativeTo otherFrame (originPoint frame)
        , xDirection = Direction2d.relativeTo otherFrame (xDirection frame)
        , yDirection = Direction2d.relativeTo otherFrame (yDirection frame)
        }


{-| Take one frame defined in global coordinates and a second frame defined
in local coordinates relative to the first frame, and return the second frame
expressed in global coordinates.
-}
placeIn :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Frame2d units localCoordinates defines
    -> Frame2d units globalCoordinates defines
placeIn otherFrame frame =
    Types.Frame2d
        { originPoint = Point2d.placeIn otherFrame (originPoint frame)
        , xDirection = Direction2d.placeIn otherFrame (xDirection frame)
        , yDirection = Direction2d.placeIn otherFrame (yDirection frame)
        }
