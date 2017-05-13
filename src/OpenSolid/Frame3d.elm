--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module OpenSolid.Frame3d
    exposing
        ( xyz
        , at
        , originPoint
        , xDirection
        , yDirection
        , zDirection
        , isRightHanded
        , xAxis
        , yAxis
        , zAxis
        , xyPlane
        , yxPlane
        , yzPlane
        , zyPlane
        , zxPlane
        , xzPlane
        , xySketchPlane
        , yxSketchPlane
        , yzSketchPlane
        , zySketchPlane
        , zxSketchPlane
        , xzSketchPlane
        , flipX
        , flipY
        , flipZ
        , moveTo
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateAlongOwn
        , mirrorAcross
        , relativeTo
        , placeIn
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/frame3d.svg" alt="Frame3d" width="160">

A `Frame3d` has an origin point and a set of X, Y and Z directions (which are
always perpendicular to each other). It can be thought of as:

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

Frames can by constructed by passing a record with `originPoint`, `xDirection`,
`yDirection` and `zDirection` fields to the `Frame3d` constructor, for example:

    frame =
        Frame3d
            { originPoint = Point3d ( 2, 1, 3 )
            , xDirection = Direction3d ( 0.8, 0.6, 0 )
            , yDirection = Direction3d ( -0.6, 0.8, 0 )
            , zDirection = Direction3d ( 0, 0, 1 )
            }

In this case **you must be careful to ensure that the X, Y and Z directions are
perpendicular**. (You will likely also want to make sure that they form a
[right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
coordinate system.) To construct sets of mutually perpendicular directions,
[`Vector3d.orthonormalize`](OpenSolid-Vector3d#orthonormalize) or
[`Direction3d.orthogonalize`](OpenSolid-Direction3d#orthogonalize) may be
useful.


# Predefined frames

@docs xyz


# Constructors

@docs at


# Accessors

@docs originPoint, xDirection, yDirection, zDirection


# Handedness

@docs isRightHanded


# Axes

@docs xAxis, yAxis, zAxis


# Planes

The following functions all return planes with the same origin point as the
given plane, but with varying normal directions. In each case the normal
direction of the resulting plane is given by the cross product of the two
indicated basis directions (assuming a right-handed frame); for example,

    Frame3d.xyPlane Frame3d.xyz
    --> Plane3d
    -->     { originPoint = Point3d.origin
    -->     , normalDirection = Direction3d.positiveZ
    -->     }

since the cross product of the X and Y basis directions of a frame is equal to
its Z basis direction. And since reversing the order of arguments in a cross
product reverses the sign of the result,

    Frame3d.yxPlane Frame3d.xyz
    --> Plane3d
    -->     { originPoint = Point3d.origin
    -->     , normalDirection = Direction3d.negativeZ
    -->     }

@docs xyPlane, yxPlane, yzPlane, zyPlane, zxPlane, xzPlane


# Sketch planes

These functions all form a `SketchPlane3d` from two axes of the given frame. The
X and Y axes of the sketch plane will correspond to the two indicated axes. For
example,

    Frame3d.yzSketchPlane Frame3d.xyz
    --> SketchPlane3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d.y
    -->     , yDirection = Direction3d.z
    -->     }

Note that this can be confusing - for example, a local X coordinate in the above
sketch plane corresponds to a global Y coordinate, and a local Y coordinate
corresponds to a global Z coordinate!

@docs xySketchPlane, yxSketchPlane, yzSketchPlane, zySketchPlane, zxSketchPlane, xzSketchPlane


# Transformations

@docs flipX, flipY, flipZ, moveTo, rotateAround, rotateAroundOwn, translateBy, translateAlongOwn, mirrorAcross


# Coordinate frames

@docs relativeTo, placeIn

-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Vector3d as Vector3d


{-| The global XYZ frame.

    Frame3d.xyz
    --> Frame3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d.x
    -->     , yDirection = Direction3d.y
    -->     , zDirection = Direction3d.z
    -->     }

-}
xyz : Frame3d
xyz =
    at Point3d.origin


{-| Construct a frame aligned with the global XYZ frame but with the given
origin point.

    Frame3d.at (Point3d ( 2, 1, 3 ))
    --> Frame3d
    -->     { originPoint = Point3d ( 2, 1, 3 )
    -->     , xDirection = Direction3d.x
    -->     , yDirection = Direction3d.y
    -->     , zDirection = Direction3d.z
    -->     }

-}
at : Point3d -> Frame3d
at point =
    Frame3d
        { originPoint = point
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        , zDirection = Direction3d.z
        }


{-| Get the origin point of a given frame.

    Frame3d.originPoint Frame3d.xyz
    --> Point3d.origin

-}
originPoint : Frame3d -> Point3d
originPoint (Frame3d properties) =
    properties.originPoint


{-| Get the X direction of a given frame.

    Frame3d.xDirection Frame3d.xyz
    --> Direction3d.x

-}
xDirection : Frame3d -> Direction3d
xDirection (Frame3d properties) =
    properties.xDirection


{-| Get the Y direction of a given frame.

    Frame3d.yDirection Frame3d.xyz
    --> Direction3d.y

-}
yDirection : Frame3d -> Direction3d
yDirection (Frame3d properties) =
    properties.yDirection


{-| Get the Z direction of a given frame.

    Frame3d.zDirection Frame3d.xyz
    --> Direction3d.z

-}
zDirection : Frame3d -> Direction3d
zDirection (Frame3d properties) =
    properties.zDirection


{-| Check if a frame is [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).

    Frame3d.isRightHanded Frame3d.xyz
    --> True

    Frame3d.isRightHanded (Frame3d.flipZ Frame3d.xyz)
    --> False

All predefined frames are right-handed, and most operations on frames preserve
handedness, so about the only ways to end up with a left-handed frame are by
constructing one explicitly or by mirroring a right-handed frame.

-}
isRightHanded : Frame3d -> Bool
isRightHanded frame =
    let
        xVector =
            Direction3d.toVector (xDirection frame)

        yVector =
            Direction3d.toVector (yDirection frame)

        zVector =
            Direction3d.toVector (zDirection frame)
    in
        Vector3d.dotProduct zVector (Vector3d.crossProduct xVector yVector) > 0


{-| Get the X axis of a given frame (the axis formed from the frame's origin
point and X direction).

    Frame3d.xAxis Frame3d.xyz
    --> Axis3d.x

-}
xAxis : Frame3d -> Axis3d
xAxis frame =
    Axis3d { originPoint = originPoint frame, direction = xDirection frame }


{-| Get the Y axis of a given frame (the axis formed from the frame's origin
point and Y direction).

    Frame3d.yAxis Frame3d.xyz
    --> Axis3d.y

-}
yAxis : Frame3d -> Axis3d
yAxis frame =
    Axis3d { originPoint = originPoint frame, direction = yDirection frame }


{-| Get the Z axis of a given frame (the axis formed from the frame's origin
point and Z direction).

    Frame3d.zAxis Frame3d.xyz
    --> Axis3d.z

-}
zAxis : Frame3d -> Axis3d
zAxis frame =
    Axis3d { originPoint = originPoint frame, direction = zDirection frame }


{-| Get a plane with normal direction equal to the frame's positive Z direction.
-}
xyPlane : Frame3d -> Plane3d
xyPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , normalDirection = zDirection frame
        }


{-| Get a plane with normal direction equal to the frame's negative Z direction.
-}
yxPlane : Frame3d -> Plane3d
yxPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , normalDirection = Direction3d.flip (zDirection frame)
        }


{-| Get a plane with normal direction equal to the frame's positive X direction.
-}
yzPlane : Frame3d -> Plane3d
yzPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , normalDirection = xDirection frame
        }


{-| Get a plane with normal direction equal to the frame's negative X direction.
-}
zyPlane : Frame3d -> Plane3d
zyPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , normalDirection = Direction3d.flip (xDirection frame)
        }


{-| Get a plane with normal direction equal to the frame's positive Y direction.
-}
zxPlane : Frame3d -> Plane3d
zxPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , normalDirection = yDirection frame
        }


{-| Get a plane with normal direction equal to the frame's negative Y direction.
-}
xzPlane : Frame3d -> Plane3d
xzPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , normalDirection = Direction3d.flip (yDirection frame)
        }


{-| Form a sketch plane from the given frame's X and Y axes.
-}
xySketchPlane : Frame3d -> SketchPlane3d
xySketchPlane frame =
    SketchPlane3d
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| Form a sketch plane from the given frame's Y and X axes.
-}
yxSketchPlane : Frame3d -> SketchPlane3d
yxSketchPlane frame =
    SketchPlane3d
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = xDirection frame
        }


{-| Form a sketch plane from the given frame's Y and Z axes.
-}
yzSketchPlane : Frame3d -> SketchPlane3d
yzSketchPlane frame =
    SketchPlane3d
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = zDirection frame
        }


{-| Form a sketch plane from the given frame's Z and Y axes.
-}
zySketchPlane : Frame3d -> SketchPlane3d
zySketchPlane frame =
    SketchPlane3d
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = yDirection frame
        }


{-| Form a sketch plane from the given frame's Z and X axes.
-}
zxSketchPlane : Frame3d -> SketchPlane3d
zxSketchPlane frame =
    SketchPlane3d
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = xDirection frame
        }


{-| Form a sketch plane from the given frame's X and Z axes.
-}
xzSketchPlane : Frame3d -> SketchPlane3d
xzSketchPlane frame =
    SketchPlane3d
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = zDirection frame
        }


{-| Reverse the X direction of a frame.

    Frame3d.flipX Frame3d.xyz
    --> Frame3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d.negativeX
    -->     , yDirection = Direction3d.positiveY
    -->     , zDirection = Direction3d.positiveZ
    -->     }

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
flipX : Frame3d -> Frame3d
flipX frame =
    Frame3d
        { originPoint = originPoint frame
        , xDirection = Direction3d.flip (xDirection frame)
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Reverse the Y direction of a frame.

    Frame3d.flipY Frame3d.xyz
    --> Frame3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d.positiveX
    -->     , yDirection = Direction3d.negativeY
    -->     , zDirection = Direction3d.positiveZ
    -->     }

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
flipY : Frame3d -> Frame3d
flipY frame =
    Frame3d
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = Direction3d.flip (yDirection frame)
        , zDirection = zDirection frame
        }


{-| Reverse the Z direction of a frame.

    Frame3d.flipZ Frame3d.xyz
    --> Frame3d
    -->     { originPoint = Point3d.origin
    -->     , xDirection = Direction3d.positiveX
    -->     , yDirection = Direction3d.positiveY
    -->     , zDirection = Direction3d.negativeZ
    -->     }

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
flipZ : Frame3d -> Frame3d
flipZ frame =
    Frame3d
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = Direction3d.flip (zDirection frame)
        }


{-| Move a frame so that it has the given origin point but same orientation.

    point =
        Point3d ( 2, 1, 3 )

    Frame3d.at point
    --> Frame3d
    -->     { originPoint = Point3d ( 2, 1, 3 )
    -->     , xDirection = Direction3d.x
    -->     , yDirection = Direction3d.y
    -->     , zDirection = Direction3d.z
    -->     }

-}
moveTo : Point3d -> Frame3d -> Frame3d
moveTo newOrigin frame =
    Frame3d
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Rotate a frame around an axis by a given angle (in radians). The frame's
origin point and basis directions will all be rotated around the given axis.

    frame =
        Frame3d.at (Point3d ( 2, 1, 3 ))

    Frame3d.rotateAround Frame3d.zAxis (degrees 90) frame
    --> Frame3d
    -->     { originPoint = Point3d ( -1, 2, 3 )
    -->     , xDirection = Direction3d.positiveY
    -->     , yDirection = Direction3d.negativeX
    -->     , zDirection = Direction3d.positiveZ
    -->     }

-}
rotateAround : Axis3d -> Float -> Frame3d -> Frame3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \frame ->
            Frame3d
                { originPoint = rotatePoint (originPoint frame)
                , xDirection = rotateDirection (xDirection frame)
                , yDirection = rotateDirection (yDirection frame)
                , zDirection = rotateDirection (zDirection frame)
                }


{-| Rotate a frame around one of its own axes by a given angle (in radians).

The first argument is a function that returns the axis to rotate around, given
the current frame. The majority of the time this will be either `Frame3d.xAxis`,
`Frame3d.yAxis` or `Frame3d.zAxis`. Compare the following to the above example
for `rotateAround`:

    frame =
        Frame3d.at (Point3d ( 2, 1, 3 ))

    Frame3d.rotateAroundOwn Frame3d.zAxis (degrees 90) frame
    --> Frame3d
    -->     { originPoint = Point3d ( 2, 1, 3 )
    -->     , xDirection = Direction3d.positiveY
    -->     , yDirection = Direction3d.negativeX
    -->     , zDirection = Direction3d.positiveZ
    -->     }

Since the rotation is done around the frame's own Z axis (which passes through
the frame's origin point), the origin point remains the same after rotation.

In this example the frame's Z axis has the same orientation as the global Z axis
so the frame's basis directions are rotated the same way, but in more complex
examples involving rotated frames a rotation around (for example) the frame's
own Z axis may be completely different from a rotation around the global Z axis.

-}
rotateAroundOwn : (Frame3d -> Axis3d) -> Float -> Frame3d -> Frame3d
rotateAroundOwn axis angle frame =
    rotateAround (axis frame) angle frame


{-| Translate a frame by a given displacement.

    frame =
        Frame3d.at (Point3d ( 2, 1, 3 ))

    displacement =
        Vector3d ( 1, 1, 1 )

    Frame3d.translateBy displacement frame
    --> Frame3d.at (Point3d ( 3, 2, 4 ))

-}
translateBy : Vector3d -> Frame3d -> Frame3d
translateBy vector frame =
    Frame3d
        { originPoint = Point3d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Translate a frame along one of its own axes by a given distance.

The first argument is a function that returns the axis to translate along, given
the current frame. The majority of the time this will be either `Frame3d.xAxis`,
`Frame3d.yAxis` or `Frame3d.zAxis`.

This function is convenient when constructing frames via a series of
transformations. For example,

    Frame3d.at (Point3d ( 2, 0, 0 ))
        |> Frame3d.rotateAroundOwn Frame3d.zAxis (degrees 45)
        |> Frame3d.translateAlongOwn Frame3d.xAxis 2

means "construct a frame at the point (2, 0, 0), rotate it around its own Z axis
by 45 degrees, then translate it along its own (rotated) X axis by 2 units",
resulting in

    Frame3d
        { originPoint = Point3d ( 3.4142, 1.4142, 0 )
        , xDirection = Direction3d ( 0.7071, 0.7071, 0 )
        , yDirection = Direction3d ( -0.7071, 0.7071, 0)
        , zDirection = Direction3d.z
        }

-}
translateAlongOwn : (Frame3d -> Axis3d) -> Float -> Frame3d -> Frame3d
translateAlongOwn axis distance frame =
    let
        direction =
            Axis3d.direction (axis frame)
    in
        translateBy (Vector3d.in_ direction distance) frame


{-| Mirror a frame across a plane.

    frame =
        Frame3d.at (Point3d ( 2, 1, 3 ))

    Frame3d.mirrorAcross Plane3d.xy frame
    --> Frame3d
    -->     { originPoint = Point3d ( 2, 1, -3 )
    -->     , xDirection = Direction3d.positiveX
    -->     , yDirection = Direction3d.positiveY
    -->     , zDirection = Direction3d.negativeZ
    -->     }

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
mirrorAcross : Plane3d -> Frame3d -> Frame3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
        \frame ->
            Frame3d
                { originPoint = mirrorPoint (originPoint frame)
                , xDirection = mirrorDirection (xDirection frame)
                , yDirection = mirrorDirection (yDirection frame)
                , zDirection = mirrorDirection (zDirection frame)
                }


{-| Take two frames defined in global coordinates, and return the second one
expressed in local coordinates relative to the first.
-}
relativeTo : Frame3d -> Frame3d -> Frame3d
relativeTo otherFrame =
    let
        relativePoint =
            Point3d.relativeTo otherFrame

        relativeDirection =
            Direction3d.relativeTo otherFrame
    in
        \frame ->
            Frame3d
                { originPoint = relativePoint (originPoint frame)
                , xDirection = relativeDirection (xDirection frame)
                , yDirection = relativeDirection (yDirection frame)
                , zDirection = relativeDirection (zDirection frame)
                }


{-| Take one frame defined in global coordinates and a second frame defined
in local coordinates relative to the first frame, and return the second frame
expressed in global coordinates.
-}
placeIn : Frame3d -> Frame3d -> Frame3d
placeIn otherFrame =
    let
        placePoint =
            Point3d.placeIn otherFrame

        placeDirection =
            Direction3d.placeIn otherFrame
    in
        \frame ->
            Frame3d
                { originPoint = placePoint (originPoint frame)
                , xDirection = placeDirection (xDirection frame)
                , yDirection = placeDirection (yDirection frame)
                , zDirection = placeDirection (zDirection frame)
                }
