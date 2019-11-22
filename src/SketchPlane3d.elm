--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module SketchPlane3d exposing
    ( SketchPlane3d
    , xy, yx, yz, zy, zx, xz
    , through, withNormalDirection, on, throughPoints, fromPlane, copy, unsafe
    , toPlane, toFrame
    , originPoint, xDirection, yDirection, normalDirection, xAxis, yAxis, normalAxis
    , offsetBy, reverseX, reverseY, moveTo, rotateAround, rotateAroundOwn, translateBy, translateIn, translateAlongOwn, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| A `SketchPlane3d` represents a 2D planar coordinate system in 3D space, and
is defined by its origin point and X and Y directions (which are always
perpendicular to each other). Sketch planes are the primary tool for converting
back and forth between 2D and 3D coordinates:

  - 3D geometry such as points, directions and line segments can be projected
    _into_ a sketch plane, which effectively projects the geometry _onto_ the
    sketch plane and then expresses the projected geometry _in_ 2D coordinates.
  - 2D geometry can be place _onto_ a sketch plane to result in 3D geometry. For
    example, a 2D point placed onto a sketch plane will result in a 3D point
    _on_ that sketch plane that has the given 2D coordinate _in_ the sketch
    plane.

This allows you to create algorithms that project from 3D into 2D, perform some
calculations in 2D, then convert the result back to 3D.

Many 3D data types have `projectInto` functions that return the corresponding 2D
data type, and `on` functions for converting back to 3D. For example,
[`Triangle3d.projectInto`](Triangle3d#projectInto) returns a `Triangle2d` and
[`Triangle3d.on`](Triangle3d#on) returns a `Triangle3d`. These pairs of
functions are almost, but not quite, inverses of each other:

    triangle2d
        |> Triangle3d.on sketchPlane
        |> Triangle3d.projectInto sketchPlane

will just return the original `triangle2d` value (within roundoff error), while

    triangle3d
        |> Triangle3d.projectInto sketchPlane
        |> Triangle3d.on sketchPlane

is equivalent to

    triangle3d
        |> Triangle3d.projectOnto
            (SketchPlane3d.toPlane sketchPlane)

@docs SketchPlane3d


# Constants

These predefined sketch planes all have the global origin point as their origin
point, and use the two indicated global axes as their X and Y axes. For example,

    SketchPlane3d.originPoint SketchPlane3d.yz
    --> Point3d.origin

    SketchPlane3d.xDirection SketchPlane3d.yz
    --> Direction3d.y

    SketchPlane3d.yDirection SketchPlane3d.yz
    --> Direction3d.z

@docs xy, yx, yz, zy, zx, xz


# Constructors

Sketch planes can also be constructed from `Frame3d` values using
`Frame3d.xySketchPlane` etc.

@docs through, withNormalDirection, on, throughPoints, fromPlane, copy, unsafe


# Conversions

@docs toPlane, toFrame


# Properties

@docs originPoint, xDirection, yDirection, normalDirection, xAxis, yAxis, normalAxis


# Transformations

@docs offsetBy, reverseX, reverseY, moveTo, rotateAround, rotateAroundOwn, translateBy, translateIn, translateAlongOwn, mirrorAcross


# Unit conversions

@docs at, at_


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types exposing (Frame3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Rate)
import Quantity.Extra as Quantity
import Unsafe.Direction3d as Direction3d
import Vector3d exposing (Vector3d)


{-| The type parameters of a `SketchPlane3d` indicate what units and coordinate
systems it's defined in, and what local 2D coordinate system (if any) it itself
defines. A concrete `SketchPlane3d` type might look like

    type alias SketchPlane =
        SketchPlane3d Meters World { defines : Sketch }

which can be read as "a `SketchPlane3d` defined in meters in world coordinates,
which itself defines a local 2D sketch coordinate system". For sketch planes
that don't define a local 2D coordinate system, you could use

    type alias SketchPlane =
        SketchPlane3d Meters World {}

Many functions in this module don't care about the third type argument (whether
it's a record with a `defines` field like in the first example, an empty record
like in the second example, or even something else entirely) but functions like
`projectInto` and `on` expect the `{ defines : localCoordinates }` pattern.

-}
type alias SketchPlane3d units coordinates defines =
    Types.SketchPlane3d units coordinates defines


{-| A sketch plane formed from the global X and Y axes.
-}
xy : SketchPlane3d units coordinates defines
xy =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        }


{-| A sketch plane formed from the global Y and X axes.
-}
yx : SketchPlane3d units coordinates defines
yx =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.x
        }


{-| A sketch plane formed from the global Y and Z axes.
-}
yz : SketchPlane3d units coordinates defines
yz =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        }


{-| A sketch plane formed from the global Z and Y axes.
-}
zy : SketchPlane3d units coordinates defines
zy =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.y
        }


{-| A sketch plane formed from the global Z and X axes.
-}
zx : SketchPlane3d units coordinates defines
zx =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.x
        }


{-| A sketch plane formed from the global X and Z axes.
-}
xz : SketchPlane3d units coordinates defines
xz =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.z
        }


{-| Construct a sketch plane having the given origin point and normal direction.
Perpendicular X and Y directions will be chosen arbitrarily; this is useful when
constructing 'scratch' sketch planes where the specific X/Y directions are
unimportant.

    sketchPlane =
        SketchPlane3d.through Point3d.origin
            (Direction3d.xz (Angle.degrees 60))

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.origin

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.xz (Angle.degrees -30)

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.y

-}
through : Point3d units coordinates -> Direction3d coordinates -> SketchPlane3d units coordinates defines
through givenOrigin givenNormalDirection =
    let
        ( computedXDirection, computedYDirection ) =
            Direction3d.perpendicularBasis givenNormalDirection
    in
    unsafe
        { originPoint = givenOrigin
        , xDirection = computedXDirection
        , yDirection = computedYDirection
        }


{-| Construct a sketch plane with the given normal direction, through the given
point. Flipped version of `through`.
-}
withNormalDirection : Direction3d coordinates -> Point3d units coordinates -> SketchPlane3d units coordinates defines
withNormalDirection givenNormalDirection givenOrigin =
    through givenOrigin givenNormalDirection


{-| Construct one sketch plane lying on another sketch plane, but with a
different origin point and X/Y directions. To do this, a `Frame2d` must be
provided that specifies the origin point and X/Y directions of the new sketch
plane, in 2D coordinates within the existing sketch plane. Whew!

    frame2d =
        Frame2d.atPoint (Point2d.meters 2 3)
            |> Frame2d.rotateBy (Angle.degrees -30)

    sketchPlane =
        SketchPlane3d.on SketchPlane3d.yz frame2d

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.meters 0 2 3

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.yz (Angle.degrees -30)

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.yz (Angle.degrees 60)

-}
on :
    SketchPlane3d units coordinates3d { defines : coordinates2d }
    -> Frame2d units coordinates2d defines
    -> SketchPlane3d units coordinates3d defines
on sketchPlane frame =
    Types.SketchPlane3d
        { originPoint = Point3d.on sketchPlane (Frame2d.originPoint frame)
        , xDirection = Direction3d.on sketchPlane (Frame2d.xDirection frame)
        , yDirection = Direction3d.on sketchPlane (Frame2d.yDirection frame)
        }


{-| Construct a SketchPlane3d from the given plane;

    SketchPlane3d.fromPlane plane

is equivalent to

    SketchPlane3d.withNormalDirection
        (Plane3d.normalDirection plane)
        (Plane3d.originPoint plane)

Note that because the X and Y directions of the resulting sketch plane are
chosen arbitrarily, conversions may not work exactly as you expect. For example,
in the current implementation,

    sketchPlane =
        SketchPlane3d.fromPlane Plane3d.xy

is not equal to `SketchPlane3d.xy` (although the two sketch planes have the same
origin point and are coplanar):

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.origin

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.negativeY

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.positiveX

-}
fromPlane : Plane3d units coordinates -> SketchPlane3d units coordinates defines
fromPlane plane =
    withNormalDirection (Plane3d.normalDirection plane)
        (Plane3d.originPoint plane)


{-| Construct a sketch plane directly from its origin point and X and Y
directions:

    sketchPlane =
        SketchPlane3d.unsafe
            { originPoint = Point3d.meters 2 1 3
            , xDirection = Direction3d.positiveY
            , yDirection = Direction3d.negativeZ
            }

If you construct a `SketchPlane3d` this way, **you must ensure that the X and Y
basis directions are perpendicular to each other**.

-}
unsafe :
    { originPoint : Point3d units coordinates
    , xDirection : Direction3d coordinates
    , yDirection : Direction3d coordinates
    }
    -> SketchPlane3d units coordinates defines
unsafe =
    Types.SketchPlane3d


{-| Attempt to construct a sketch plane that passes through the three given
points. Returns a sketch plane where:

  - The origin point is the first given point
  - The X direction is equal to the direction from the first given point to the
    second
  - The Y direction is chosen such that the third given point lies on the sketch
    plane and has a positive Y coordinate within the sketch plane (that is, it
    is on the positive Y side of the sketch plane's X axis)

If the three given points are collinear, returns `Nothing`.

    SketchPlane3d.throughPoints
        (Point3d.meters 2 0 0)
        (Point3d.meters 3 0 0)
        (Point3d.meters 4 1 1)
    --> Just sketchPlane

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.meters 2 0 0

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.x

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.yz (Angle.degrees 45)

    SketchPlane3d.throughPoints
        (Point3d.meters 2 0 0)
        (Point3d.meters 3 0 0)
        (Point3d.meters 4 0 0)
    --> Nothing

-}
throughPoints : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Maybe (SketchPlane3d units coordinates defines)
throughPoints firstPoint secondPoint thirdPoint =
    Direction3d.from firstPoint secondPoint
        |> Maybe.andThen
            (\computedXDirection ->
                let
                    firstCandidate =
                        Vector3d.from firstPoint thirdPoint

                    secondCandidate =
                        Vector3d.from secondPoint thirdPoint

                    chosenVector =
                        if
                            Vector3d.length firstCandidate
                                |> Quantity.lessThanOrEqualTo
                                    (Vector3d.length secondCandidate)
                        then
                            firstCandidate

                        else
                            secondCandidate

                    yVector =
                        chosenVector
                            |> Vector3d.minus
                                (chosenVector
                                    |> Vector3d.projectionIn computedXDirection
                                )
                in
                Vector3d.direction yVector
                    |> Maybe.map
                        (\computedYDirection ->
                            unsafe
                                { originPoint = firstPoint
                                , xDirection = computedXDirection
                                , yDirection = computedYDirection
                                }
                        )
            )


{-| Convert a sketch plane from one units type to another, by providing a
conversion factor given as a rate of change of destination units with respect to
source units.
-}
at : Quantity Float (Rate units2 units1) -> SketchPlane3d units1 coordinates defines -> SketchPlane3d units2 coordinates defines
at rate (Types.SketchPlane3d sketchPlane) =
    Types.SketchPlane3d
        { originPoint = Point3d.at rate sketchPlane.originPoint
        , xDirection = sketchPlane.xDirection
        , yDirection = sketchPlane.yDirection
        }


{-| Convert a sketch plane from one units type to another, by providing an
'inverse' conversion factor given as a rate of change of source units with
respect to destination units.
-}
at_ : Quantity Float (Rate units1 units2) -> SketchPlane3d units1 coordinates defines -> SketchPlane3d units2 coordinates defines
at_ rate sketchPlane =
    at (Quantity.inverse rate) sketchPlane


{-| Get the origin point of a sketch plane.
-}
originPoint : SketchPlane3d units coordinates defines -> Point3d units coordinates
originPoint (Types.SketchPlane3d properties) =
    properties.originPoint


{-| Get the X direction of a sketch plane (the direction of the sketch plane's
X axis).
-}
xDirection : SketchPlane3d units coordinates defines -> Direction3d coordinates
xDirection (Types.SketchPlane3d properties) =
    properties.xDirection


{-| Get the Y direction of a sketch plane (the direction of the sketch plane's
Y axis).
-}
yDirection : SketchPlane3d units coordinates defines -> Direction3d coordinates
yDirection (Types.SketchPlane3d properties) =
    properties.yDirection


{-| Get the normal direction to a sketch plane. This is equal to the cross
product of the sketch plane's X and Y directions.

    SketchPlane3d.normalDirection SketchPlane3d.xy
    --> Direction3d.z

    SketchPlane3d.normalDirection SketchPlane3d.xz
    --> Direction3d.negativeY

-}
normalDirection : SketchPlane3d units coordinates defines -> Direction3d coordinates
normalDirection sketchPlane =
    Direction3d.unsafeCrossProduct
        (xDirection sketchPlane)
        (yDirection sketchPlane)


{-| Get the X axis of a sketch plane. A 2D X coordinate within the sketch plane
corresponds to a distance along this axis in 3D.

    SketchPlane3d.xAxis SketchPlane3d.zx
    --> Axis3d.z

-}
xAxis : SketchPlane3d units coordinates defines -> Axis3d units coordinates
xAxis (Types.SketchPlane3d sketchPlane) =
    Axis3d.through sketchPlane.originPoint sketchPlane.xDirection


{-| Get the Y axis of a sketch plane. A 2D Y coordinate within the sketch plane
corresponds to a distance along this axis in 3D.

    SketchPlane3d.yAxis SketchPlane3d.zx
    --> Axis3d.x

-}
yAxis : SketchPlane3d units coordinates defines -> Axis3d units coordinates
yAxis (Types.SketchPlane3d sketchPlane) =
    Axis3d.through sketchPlane.originPoint sketchPlane.yDirection


{-| Get the normal axis to a sketch plane (the axis formed from the sketch
plane's origin point and normal direction).

    SketchPlane3d.normalAxis SketchPlane3d.xy
    --> Axis3d.z

    SketchPlane3d.normalAxis SketchPlane3d.xz
    --> Axis3d.reverse Axis3d.y

-}
normalAxis : SketchPlane3d units coordinates defines -> Axis3d units coordinates
normalAxis sketchPlane =
    Axis3d.through (originPoint sketchPlane) (normalDirection sketchPlane)


{-| Convert a `SketchPlane3d` to a `Plane3d` with the same origin point and
normal direction.

    SketchPlane3d.toPlane SketchPlane3d.xy
    --> Plane3d.xy

    SketchPlane3d.toPlane SketchPlane3d.yx
    --> Plane3d.reverseNormal Plane3d.xy

-}
toPlane : SketchPlane3d units coordinates defines -> Plane3d units coordinates
toPlane sketchPlane =
    Plane3d.through (originPoint sketchPlane) (normalDirection sketchPlane)


{-| Convert a `SketchPlane3d` to a `Frame3d` with the same origin point and X
and Y directions. The Z direction of the returned frame will be equal to the
normal direction of the given sketch plane. This means that the given sketch
plane will be the XY sketch plane of the returned frame.
-}
toFrame : SketchPlane3d units coordinates defines2d -> Frame3d units coordinates defines3d
toFrame sketchPlane =
    Types.Frame3d
        { originPoint = originPoint sketchPlane
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        , zDirection = normalDirection sketchPlane
        }


{-| Create a 'fresh copy' of a sketch plane: one with the same origin point and
X/Y directions, but that can be used to define a different local coordinate
system. Sometimes useful in generic/library code. Despite the name, this is
efficient: it really just returns the value you passed in, but with a different
type.
-}
copy : SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
copy (Types.SketchPlane3d properties) =
    Types.SketchPlane3d properties


{-| Shift a sketch plane in its own normal direction by the given (signed)
distance.

    SketchPlane3d.xy
        |> SketchPlane3d.offsetBy (Length.meters -2.0)
        |> SketchPlane3d.originPoint
    --> Point3d.meters 0 0 -2

    SketchPlane3d.zx
        |> SketchPlane3d.offsetBy (Length.meters 1.0)
        |> SketchPlane3d.originPoint
    --> Point3d.meters 0 1 0

-}
offsetBy : Quantity Float units -> SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
offsetBy distance sketchPlane =
    sketchPlane |> translateIn (normalDirection sketchPlane) distance


{-| Reverse the X direction of a sketch plane, leaving its Y direction and
origin point unchanged.
-}
reverseX : SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
reverseX sketchPlane =
    unsafe
        { originPoint = originPoint sketchPlane
        , xDirection = Direction3d.reverse (xDirection sketchPlane)
        , yDirection = yDirection sketchPlane
        }


{-| Reverse the Y direction of a sketch plane, leaving its X direction and
origin point unchanged.
-}
reverseY : SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
reverseY sketchPlane =
    unsafe
        { originPoint = originPoint sketchPlane
        , xDirection = xDirection sketchPlane
        , yDirection = Direction3d.reverse (yDirection sketchPlane)
        }


{-| Set the origin point of the given sketch plane to the given point, leaving
its X and Y directions unchanged.
-}
moveTo : Point3d units coordinates -> SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
moveTo newOrigin sketchPlane =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| Rotate a sketch plane around an axis by a given angle. The
sketch plane's origin point and X and Y directions will all be rotated around
the given axis.

    SketchPlane3d.xy
        |> SketchPlane3d.rotateAround Axis3d.x
            (Angle.degrees 90)
    --> SketchPlane3d.xz

-}
rotateAround : Axis3d units coordinates -> Angle -> SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
rotateAround axis angle sketchPlane =
    unsafe
        { originPoint =
            Point3d.rotateAround axis angle (originPoint sketchPlane)
        , xDirection =
            Direction3d.rotateAround axis angle (xDirection sketchPlane)
        , yDirection =
            Direction3d.rotateAround axis angle (yDirection sketchPlane)
        }


{-| Rotate a sketch plane around one of its own axes by a given angle.

The first argument is a function that returns the axis to rotate around, given
the current sketch plane. The majority of the time this will be either
`SketchPlane3d.xAxis` or `SketchPlane3d.yAxis`.

This function is convenient when constructing sketch planes via a series of
transformations. For example,

    sketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.translateBy
                (Vector3d.meters 1 0 0)
            |> SketchPlane3d.rotateAroundOwn
                SketchPlane3d.yAxis
                (Angle.degrees -45)

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.meters 1 0 0

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.xz (Angle.degrees 45)

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.y

Note that since the rotation was around the sketch plane's own Y axis (which
passes through the sketch plane's origin point) instead of the global Y axis,
the origin point itself was not affected by the rotation.

-}
rotateAroundOwn : (SketchPlane3d units coordinates defines1 -> Axis3d units coordinates) -> Angle -> SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
rotateAroundOwn axis angle sketchPlane =
    rotateAround (axis sketchPlane) angle sketchPlane


{-| Translate a sketch plane by a given displacement.

    displacement =
        Vector3d.meters 2 1 3

    sketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.translateBy displacement

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.meters 2 1 3

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.x

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.y

-}
translateBy : Vector3d units coordinates -> SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
translateBy vector sketchPlane =
    unsafe
        { originPoint = Point3d.translateBy vector (originPoint sketchPlane)
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| Translate a sketch plane in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Quantity Float units -> SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
translateIn direction distance sketchPlane =
    translateBy (Vector3d.withLength distance direction) sketchPlane


{-| Translate a sketch plane along one of its own axes by a given distance.

The first argument is a function that returns the axis to translate along, given
the current sketch plane. The majority of the time this will be either
`SketchPlane3d.xAxis` or `SketchPlane3d.yAxis`.

This function is convenient when constructing frames via a series of
transformations. For example,

    sketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround
                Axis3d.x
                (Angle.degrees 45)
            |> SketchPlane3d.translateAlongOwn
                SketchPlane3d.yAxis
                (Length.meters 2)

means 'take the global XY sketch plane, rotate it around the global X axis by
45 degrees, then translate the result 2 meters along its own (rotated) Y axis',
resulting in

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.meters 0 1.4142 1.4142

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.x

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.yz (Angle.degrees 45)

-}
translateAlongOwn : (SketchPlane3d units coordinates defines1 -> Axis3d units coordinates) -> Quantity Float units -> SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
translateAlongOwn axis distance sketchPlane =
    sketchPlane |> translateIn (Axis3d.direction (axis sketchPlane)) distance


{-| Mirror a sketch plane across a plane.

    sketchPlane =
        SketchPlane3d.yz
            |> SketchPlane3d.moveTo (Point3d.meters 2 1 3)

    mirroredSketchPlane =
        SketchPlane3d.mirrorAcross Plane3d.xy sketchPlane

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.meters 2 1 -3

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.y

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.negativeZ

-}
mirrorAcross : Plane3d units coordinates -> SketchPlane3d units coordinates defines1 -> SketchPlane3d units coordinates defines2
mirrorAcross plane sketchPlane =
    unsafe
        { originPoint = Point3d.mirrorAcross plane (originPoint sketchPlane)
        , xDirection = Direction3d.mirrorAcross plane (xDirection sketchPlane)
        , yDirection = Direction3d.mirrorAcross plane (yDirection sketchPlane)
        }


{-| Take a sketch plane defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> SketchPlane3d units globalCoordinates defines -> SketchPlane3d units localCoordinates defines
relativeTo frame sketchPlane =
    Types.SketchPlane3d
        { originPoint = Point3d.relativeTo frame (originPoint sketchPlane)
        , xDirection = Direction3d.relativeTo frame (xDirection sketchPlane)
        , yDirection = Direction3d.relativeTo frame (yDirection sketchPlane)
        }


{-| Take a sketch plane defined in local coordinates relative to a given
reference frame, and return that sketch plane expressed in global coordinates.
-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> SketchPlane3d units localCoordinates defines -> SketchPlane3d units globalCoordinates defines
placeIn frame sketchPlane =
    Types.SketchPlane3d
        { originPoint = Point3d.placeIn frame (originPoint sketchPlane)
        , xDirection = Direction3d.placeIn frame (xDirection sketchPlane)
        , yDirection = Direction3d.placeIn frame (yDirection sketchPlane)
        }
