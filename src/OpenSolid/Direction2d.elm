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


module OpenSolid.Direction2d
    exposing
        ( x
        , y
        , perpendicularTo
        , fromAngle
        , toAngle
        , angleFrom
        , components
        , xComponent
        , yComponent
        , componentIn
        , equalWithin
        , toVector
        , times
        , flip
        , rotateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        , placeOnto
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/direction2d.svg" alt="Direction2d" width="160">

Various functions for creating and working with `Direction2d` values. The
simplest way to construct a `Direction2d` value is by passing a tuple of X and Y
components to the `Direction2d` constructor, for example
<code>Direction2d&nbsp;(&nbsp;1,&nbsp;0&nbsp;)</code>. However, if you do this
you must ensure that the sum of the squares of the given components is exactly
one:

    Direction2d ( 1, 0 )
    Direction2d ( 0, -1 )
    Direction2d ( 0.6, 0.8 )

are all valid but

    Direction2d ( 2, 0 )
    Direction2d ( 1, 1 )

are not. Instead of manually constructing `Direction2d` values, it may be easier
to use constructors like `Direction2d.fromAngle` or start with existing
directions and transform them as necessary.

# Predefined directions

@docs x, y

# Constructors

@docs perpendicularTo

# Angles

@docs fromAngle, toAngle, angleFrom

# Components

@docs components, xComponent, yComponent, componentIn

# Comparison

@docs equalWithin

# Vector conversion

@docs toVector, times

# Transformations

@docs flip, rotateBy, mirrorAcross

# Coordinate frames

Functions for transforming directions between local and global coordinates in
different coordinate frames. Like other transformations, coordinate
transformations of directions depend only on the orientations of the relevant
frames, not the positions of their origin points.

For the examples, assume the following frames have been defined:

    upsideDownFrame =
        Frame2d
            { originPoint = Point2d.origin
            , xDirection = Direction2d.x
            , yDirection = Direction2d.flip Direction2d.y
            }

    rotatedFrame =
        Frame2d.rotateBy (degrees 30) Frame2d.xy

@docs relativeTo, placeIn

# Sketch frames

@docs placeOnto
-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Vector2d as Vector2d


toDirection : Vector2d -> Direction2d
toDirection (Vector2d components) =
    Direction2d components


{-| The positive X direction.

    Direction2d.x ==
        Direction2d ( 1, 0 )
-}
x : Direction2d
x =
    Direction2d ( 1, 0 )


{-| The positive Y direction.

    Direction2d.y ==
        Direction2d ( 0, 1 )
-}
y : Direction2d
y =
    Direction2d ( 0, 1 )


{-| Construct a direction perpendicular to the given direction, by rotating the
given direction 90 degrees counterclockwise.

    Direction2d.perpendicularTo Direction2d.x ==
        Direction2d.y

    Direction2d.perpendicularTo Direction2d.y ==
        Direction2d.flip Direction2d.x
-}
perpendicularTo : Direction2d -> Direction2d
perpendicularTo =
    toVector >> Vector2d.perpendicularTo >> toDirection


{-| Construct a direction from an angle in radians, given counterclockwise from
the positive X direction.

    Direction2d.fromAngle 0 ==
        Direction2d.x

    Direction2d.fromAngle (degrees 90) ==
        Direction2d.y

    Direction2d.fromAngle (degrees -135) ==
        Direction2d ( -0.7071, -0.7071 )
-}
fromAngle : Float -> Direction2d
fromAngle angle =
    Direction2d ( cos angle, sin angle )


{-| Convert a direction to a counterclockwise angle in radians from the positive
X direction. The result will be in the range -π to π.

    Direction2d.toAngle Direction2d.x ==
        0

    Direction2d.toAngle Direction2d.y ==
        pi / 2

    Direction2d.toAngle (Direction2d ( 0, -1 )) ==
        -pi / 2
-}
toAngle : Direction2d -> Float
toAngle direction =
    let
        ( x, y ) =
            components direction
    in
        atan2 y x


{-| Find the counterclockwise angle in radians from the first direction to the
second. The result will be in the range -π to π.

    referenceDirection =
        Direction2d.fromAngle (degrees 30)

    Direction2d.angleFrom referenceDirection Direction2d.y ==
        degrees 60

    Direction2d.angleFrom referenceDirection Direction2d.x ==
        degrees -30
-}
angleFrom : Direction2d -> Direction2d -> Float
angleFrom other direction =
    let
        otherVector =
            toVector other

        directionVector =
            toVector direction
    in
        atan2 (Vector2d.crossProduct otherVector directionVector)
            (Vector2d.dotProduct otherVector directionVector)


{-| Get the components of a direction as a tuple (the components it would have
as a unit vector, also know as its direction cosines).

    ( x, y ) =
        Direction2d.components direction
-}
components : Direction2d -> ( Float, Float )
components (Direction2d components_) =
    components_


{-| Get the X component of a direction.

    Direction2d.xComponent Direction2d.x ==
        1

    Direction2d.xComponent Direction2d.y ==
        0
-}
xComponent : Direction2d -> Float
xComponent (Direction2d ( x, _ )) =
    x


{-| Get the Y component of a direction.

    Direction2d.yComponent Direction2d.x ==
        0

    Direction2d.yComponent Direction2d.y ==
        1
-}
yComponent : Direction2d -> Float
yComponent (Direction2d ( _, y )) =
    y


{-| Find the component of one direction in another direction. This is equal to
the cosine of the angle between the directions, or equivalently the dot product
of the two directions converted to unit vectors.

    direction =
        Direction2d.fromAngle (degrees 60)

    Direction2d.componentIn Direction2d.x direction ==
        0.5

    Direction2d.componentIn Direction2d.x Direction2d.x ==
        1

    Direction2d.componentIn Direction2d.x Direction2d.y ==
        0

This is more general and flexible than using `xComponent` or `yComponent`, both
of which can be expressed in terms of `componentIn`:

    Direction2d.xComponent direction ==
        Direction2d.componentIn Direction2d.x direction
-}
componentIn : Direction2d -> Direction2d -> Float
componentIn firstDirection secondDirection =
    Vector2d.componentIn firstDirection (toVector secondDirection)


{-| Compare two directions within a tolerance. Returns true if the angle between
the two given directions is less than the given tolerance.

    firstDirection =
        Direction2d.fromAngle (degrees 45)

    secondDirection =
        Direction2d.fromAngle (degrees 47)

    Direction2d.equalWithin (degrees 5) firstDirection secondDirection ==
        True

    Direction2d.equalWithin (degrees 1) firstDirection secondDirection ==
        False
-}
equalWithin : Float -> Direction2d -> Direction2d -> Bool
equalWithin angle firstDirection secondDirection =
    abs (angleFrom firstDirection secondDirection) <= angle


{-| Convert a direction to a unit vector.

    Direction2d.toVector Direction2d.x ==
        Vector2d ( 1, 0 )
-}
toVector : Direction2d -> Vector2d
toVector (Direction2d components) =
    Vector2d components


{-| Construct a vector from a magnitude and a direction. If the magnitude is
negative the resulting vector will be in the opposite of the given direction.

    direction =
        Direction2d ( 0.6, 0.8 )

    Direction2d.times 2 direction ==
        Vector2d ( 1.2, 1.6 )

-}
times : Float -> Direction2d -> Vector2d
times scale =
    toVector >> Vector2d.times scale


{-| Reverse a direction.

    Direction2d.flip Direction2d.y ==
        Direction2d ( 0, -1 )
-}
flip : Direction2d -> Direction2d
flip =
    toVector >> Vector2d.negate >> toDirection


{-| Rotate a direction counterclockwise by a given angle (in radians).

    Direction2d.rotateBy (degrees 45) Direction2d.x ==
        Direction2d ( 0.7071, 0.7071 )

    Direction2d.rotateBy pi Direction2d.y ==
        Direction2d.flip Direction2d.y
-}
rotateBy : Float -> Direction2d -> Direction2d
rotateBy angle =
    toVector >> Vector2d.rotateBy angle >> toDirection


{-| Mirror a direction across a particular axis. Note that only the direction of
the axis affects the result, since directions are position-independent.

    slopedAxis =
        Axis2d
            { originPoint = Point2d ( 100, 200 )
            , direction = Direction2d.fromAngle (degrees 45)
            }

    Direction2d.mirrorAcross slopedAxis Direction2d.x ==
        Direction2d.y

    Direction2d.mirrorAcross slopedAxis Direction2d.y ==
        Direction2d.x
-}
mirrorAcross : Axis2d -> Direction2d -> Direction2d
mirrorAcross axis =
    toVector >> Vector2d.mirrorAcross axis >> toDirection


{-| Take a direction defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Direction2d.relativeTo upsideDownFrame Direction2d.y ==
        Direction2d ( 0, -1 )

    Direction2d.relativeTo rotatedFrame Direction2d.x ==
        Direction2d ( 0.866, -0.5 )

    Direction2d.relativeTo rotatedFrame Direction2d.y ==
        Direction2d ( 0.5, 0.866 )
-}
relativeTo : Frame2d -> Direction2d -> Direction2d
relativeTo frame =
    toVector >> Vector2d.relativeTo frame >> toDirection


{-| Take a direction defined in local coordinates relative to a given reference
frame, and return that direction expressed in global coordinates.

    Direction2d.placeIn upsideDownFrame Direction2d.y ==
        Direction2d ( 0, -1 )

    Direction2d.placeIn rotatedFrame Direction2d.x ==
        Direction2d ( 0.866, 0.5 )

    Direction2d.placeIn rotatedFrame Direction2d.y ==
        Direction2d ( -0.5, 0.866 )
-}
placeIn : Frame2d -> Direction2d -> Direction2d
placeIn frame =
    toVector >> Vector2d.placeIn frame >> toDirection


{-| Take a direction defined in 2D coordinates within a particular sketch plane
and return the corresponding direction in 3D.

    direction =
        Direction2d ( 0.6, 0.8 )

    Direction2d.placeOnto SketchPlane3d.xy direction ==
        Direction3d ( 0.6, 0.8, 0 )

    Direction2d.placeOnto SketchPlane3d.yz direction ==
        Direction3d ( 0, 0.6, 0.8 )

    Direction2d.placeOnto SketchPlane3d.zx direction ==
        Direction3d ( 0.8, 0, 0.6 )
-}
placeOnto : SketchPlane3d -> Direction2d -> Direction3d
placeOnto sketchPlane =
    let
        (SketchPlane3d { originPoint, xDirection, yDirection }) =
            sketchPlane

        (Direction3d ( ux, uy, uz )) =
            xDirection

        (Direction3d ( vx, vy, vz )) =
            yDirection
    in
        \(Direction2d ( x, y )) ->
            Direction3d
                ( x * ux + y * vx
                , x * uy + y * vy
                , x * uz + y * vz
                )
