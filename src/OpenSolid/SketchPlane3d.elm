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


module OpenSolid.SketchPlane3d
    exposing
        ( xy
        , yx
        , yz
        , zy
        , zx
        , xz
        , originPoint
        , xDirection
        , yDirection
        , xAxis
        , yAxis
        , plane
        , flipX
        , flipY
        , moveTo
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        )

{-| Various functions for creating and working with `SketchPlane3d` values.
Sketch planes are defined by an origin point and X and Y basis directions in 3D,
which define a 2D coordinate system (the X and Y basis directions are always
perpendicular). A sketch plane can therefore be used to convert between 2D and
3D coordinates, such as taking 2D lines and placing them on a 3D plane, or
projecting a 3D point into a 2D sketch.

Sketch planes can be constructed explicitly by passing a record with
`originPoint`, `xDirection` and `yDirection` fields to the `SketchPlane3d`
constructor, for example

    sketchPlane =
        SketchPlane3d
            { originPoint = Point3d ( 2, 1, 3 )
            , xDirection = Direction3d.y
            , yDirection = Direction3d.negate Direction3d.z
            }

If you construct a `SketchPlane3d` this way, you are responsible for ensuring
that the X and Y basis directions are perpendicular to each other.

## Reading this documentation

For the examples below, assume that all OpenSolid core types have been imported
using

    import OpenSolid.Geometry.Types exposing (..)

and all other necessary modules have been imported using the following pattern:

    import OpenSolid.SketchPlane3d as SketchPlane3d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Predefined sketch planes

These predefined sketch planes all have the global origin point as their origin
point, and use the two indicated global axes as their X and Y axes. For example,

    SketchPlane3d.yz ==
        SketchPlane3d
            { originPoint = Point3d.origin
            , xDirection = Direction3d.y
            , yDirection = Direction3d.z
            }

@docs xy, yx, yz, zy, zx, xz

# Accessors

@docs originPoint, xDirection, yDirection

# Axes

@docs xAxis, yAxis

# Conversions

@docs plane

# Transformations

@docs flipX, flipY, moveTo, rotateAround, translateBy, mirrorAcross

# Coordinate frames

@docs relativeTo, placeIn
-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d


{-| A sketch plane formed from the global X and Y axes.
-}
xy : SketchPlane3d
xy =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        }


{-| A sketch plane formed from the global Y and X axes.
-}
yx : SketchPlane3d
yx =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.x
        }


{-| A sketch plane formed from the global Y and Z axes.
-}
yz : SketchPlane3d
yz =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        }


{-| A sketch plane formed from the global Z and Y axes.
-}
zy : SketchPlane3d
zy =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.y
        }


{-| A sketch plane formed from the global Z and X axes.
-}
zx : SketchPlane3d
zx =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.x
        }


{-| A sketch plane formed from the global X and Z axes.
-}
xz : SketchPlane3d
xz =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.z
        }


{-| Get the origin point of a sketch plane.

    SketchPlane3d.originPoint SketchPlane3d.xy ==
        Point3d.origin
-}
originPoint : SketchPlane3d -> Point3d
originPoint (SketchPlane3d properties) =
    properties.originPoint


{-| Get the X direction of a sketch plane (the direction of the sketch plane's
X axis).

    SketchPlane3d.xDirection SketchPlane3d.zx ==
        Direction3d.z
-}
xDirection : SketchPlane3d -> Direction3d
xDirection (SketchPlane3d properties) =
    properties.xDirection


{-| Get the Y direction of a sketch plane (the direction of the sketch plane's
Y axis).

    SketchPlane3d.yDirection SketchPlane3d.zx ==
        Direction3d.x
-}
yDirection : SketchPlane3d -> Direction3d
yDirection (SketchPlane3d properties) =
    properties.yDirection


{-| Get the X axis of a sketch plane. A 2D X coordinate within the sketch plane
corresponds to a distance along this axis in 3D.

    SketchPlane3d.xAxis SketchPlane3d.zx ==
        Axis3d.z
-}
xAxis : SketchPlane3d -> Axis3d
xAxis sketchPlane =
    Axis3d
        { originPoint = originPoint sketchPlane
        , direction = xDirection sketchPlane
        }


{-| Get the Y axis of a sketch plane. A 2D Y coordinate within the sketch plane
corresponds to a distance along this axis in 3D.

    SketchPlane3d.yAxis SketchPlane3d.zx ==
        Axis3d.x
-}
yAxis : SketchPlane3d -> Axis3d
yAxis sketchPlane =
    Axis3d
        { originPoint = originPoint sketchPlane
        , direction = yDirection sketchPlane
        }


{-| Conver a `SketchPlane3d` to a `Plane3d`. The normal direction of the
returned plane will be equal to the cross product of the X and Y directions of
the given sketch plane.

    SketchPlane3d.plane SketchPlane3d.xy ==
        Plane3d.xy

    SketchPlane3d.plane SketchPlane3d.yx ==
        Plane3d.flip Plane3d.xy
-}
plane : SketchPlane3d -> Plane3d
plane sketchPlane =
    let
        normalVector =
            Direction3d.crossProduct (xDirection sketchPlane)
                (yDirection sketchPlane)

        normalDirection =
            Direction3d (Vector3d.components normalVector)
    in
        Plane3d
            { originPoint = originPoint sketchPlane
            , normalDirection = normalDirection
            }


{-| Flip the X direction of a sketch plane, leaving its Y direction and origin
point unchanged.

    SketchPlane3d.flipX SketchPlane3d.yz ==
        SketchPlane3d
            { originPoint = Point3d.origin
            , xDirection = Direction3d.negate Direction3d.y
            , yDirection = Direction3d.z
            }
-}
flipX : SketchPlane3d -> SketchPlane3d
flipX sketchPlane =
    SketchPlane3d
        { originPoint = originPoint sketchPlane
        , xDirection = Direction3d.negate (xDirection sketchPlane)
        , yDirection = yDirection sketchPlane
        }


{-| Flip the Y direction of a sketch plane, leaving its X direction and origin
point unchanged.

    SketchPlane3d.flipY SketchPlane3d.yz ==
        SketchPlane3d
            { originPoint = Point3d.origin
            , xDirection = Direction3d.y
            , yDirection = Direction3d.negate Direction3d.z
            }
-}
flipY : SketchPlane3d -> SketchPlane3d
flipY sketchPlane =
    SketchPlane3d
        { originPoint = originPoint sketchPlane
        , xDirection = xDirection sketchPlane
        , yDirection = Direction3d.negate (yDirection sketchPlane)
        }


{-| Set the origin point of the given sketch plane to the given point, leaving
its X and Y directions unchanged.

    SketchPlane3d.moveTo (Point3d ( 2, 1, 3 )) SketchPlane3d.yz ==
        SketchPlane3d
            { originPoint = Point3d ( 2, 1, 3 )
            , xDirection = Direction3d.y
            , yDirection = Direction3d.z
            }
-}
moveTo : Point3d -> SketchPlane3d -> SketchPlane3d
moveTo newOrigin sketchPlane =
    SketchPlane3d
        { originPoint = newOrigin
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| Rotate a sketch plane around an axis by a given angle (in radians). The
sketch plane's origin point and X and Y directions will all be rotated around
the given axis.

    SketchPlane3d.rotateAround Axis3d.x (degrees 90) SketchPlane3d.xy ==
        SketchPlane3d.xz
-}
rotateAround : Axis3d -> Float -> SketchPlane3d -> SketchPlane3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = rotatePoint (originPoint sketchPlane)
                , xDirection = rotateDirection (xDirection sketchPlane)
                , yDirection = rotateDirection (yDirection sketchPlane)
                }


{-| Translate a sketch plane by a given displacement.

    displacement =
        Vector3d ( 2, 1, 3 )

    SketchPlane3d.translateBy displacement SketchPlane3d.xy ==
        SketchPlane3d
            { originPoint = Point3d ( 2, 1, 3 )
            , xDirection = Direction3d.x
            , yDirection = Direction3d.y
            }
-}
translateBy : Vector3d -> SketchPlane3d -> SketchPlane3d
translateBy vector sketchPlane =
    SketchPlane3d
        { originPoint = Point3d.translateBy vector (originPoint sketchPlane)
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| Mirror a sketch plane across a plane.

    sketchPlane =
        SketchPlane3d
            { originPoint = Point2d ( 2, 1, 3 )
            , xDirection = Direction3d.y
            , yDirection = Direction3d.z
            }

    SketchPlane3d.mirrorAcross Plane3d.xy sketchPlane ==
        SketchPlane3d
            { originPoint = Point2d ( 2, 1, -3 )
            , xDirection = Direction3d.y
            , yDirection = Direction3d.negate Direction3d.z
            }
-}
mirrorAcross : Plane3d -> SketchPlane3d -> SketchPlane3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = mirrorPoint (originPoint sketchPlane)
                , xDirection = mirrorDirection (xDirection sketchPlane)
                , yDirection = mirrorDirection (yDirection sketchPlane)
                }


{-| Take a sketch plane defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d -> SketchPlane3d -> SketchPlane3d
relativeTo frame =
    let
        relativePoint =
            Point3d.relativeTo frame

        relativeDirection =
            Direction3d.relativeTo frame
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = relativePoint (originPoint sketchPlane)
                , xDirection = relativeDirection (xDirection sketchPlane)
                , yDirection = relativeDirection (yDirection sketchPlane)
                }


{-| Take a sketch plane defined in local coordinates relative to a given
reference frame, and return that sketch plane expressed in global coordinates.
-}
placeIn : Frame3d -> SketchPlane3d -> SketchPlane3d
placeIn frame =
    let
        placePoint =
            Point3d.placeIn frame

        placeDirection =
            Direction3d.placeIn frame
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = placePoint (originPoint sketchPlane)
                , xDirection = placeDirection (xDirection sketchPlane)
                , yDirection = placeDirection (yDirection sketchPlane)
                }
