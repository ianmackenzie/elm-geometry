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


module Axis3d
    exposing
        ( Axis3d
        , direction
        , flip
        , mirrorAcross
        , moveTo
        , on
        , originPoint
        , placeIn
        , projectInto
        , projectOnto
        , relativeTo
        , rotateAround
        , translateBy
        , withDirection
        , x
        , y
        , z
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/axis3d.svg" alt="Axis3d" width="160">

An `Axis3d` represents an infinitely long straight line in 3D and is defined by
an origin point and direction. Axes have several uses, such as:

  - Rotating around the axis
  - Projecting onto the axis
  - Measuring distance along the axis from the origin point

@docs Axis3d


# Constants

@docs x, y, z


# Constructors

@docs withDirection, on


# Properties

@docs originPoint, direction


# Transformations

@docs flip, moveTo, rotateAround, translateBy, mirrorAcross, projectOnto


# Coordinate conversions

Functions for transforming axes between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn, projectInto

-}

import Axis2d exposing (Axis2d)
import Direction3d exposing (Direction3d)
import Geometry.Internal as Internal exposing (Frame3d, Plane3d, SketchPlane3d)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Axis3d =
    Internal.Axis3d


{-| The global X axis.

    Axis3d.x
    --> Axis3d.withDirection Direction3d.x Point3d.origin

-}
x : Axis3d
x =
    withDirection Direction3d.x Point3d.origin


{-| The global Y axis.

    Axis3d.y
    --> Axis3d.withDiretion Direction3d.y Point3d.origin

-}
y : Axis3d
y =
    withDirection Direction3d.y Point3d.origin


{-| The global Z axis.

    Axis3d.z
    --> Axis3d.withDirection Direction3d.z Point3d.origin

-}
z : Axis3d
z =
    withDirection Direction3d.z Point3d.origin


{-| Construct an axis with the given direction having the given origin point:

    exampleAxis =
        Axis3d.withDirection Direction3d.y
            (Point3d.fromCoordinates ( 1, 2, 3 ))

-}
withDirection : Direction3d -> Point3d -> Axis3d
withDirection direction originPoint =
    Internal.Axis3d { direction = direction, originPoint = originPoint }


{-| Construct a 3D axis lying _on_ a sketch plane by providing a 2D axis
specified in XY coordinates _within_ the sketch plane.

    axis2d =
        Axis2d.through (Point2d.fromCoordinates ( 1, 3 ))
            (Direction2d.fromAngle (degrees 30))

    Axis3d.on SketchPlane3d.xy axis2d
    --> Axis3d.withDirection
    -->     (Direction3d.fromAzimuthAndElevation
    -->         ( degrees 30, 0 )
    -->     )
    -->     (Point3d.fromCoordinates ( 1, 3, 0 ))

    Axis3d.on SketchPlane3d.zx axis2d
    --> Axis3d.withDirection
    -->     (Direction3d.fromAzimuthAndElevation
    -->         ( 0, degrees 60 )
    -->     )
    -->     (Point3d.fromCoordinates ( 3, 0, 1 ))

-}
on : SketchPlane3d -> Axis2d -> Axis3d
on sketchPlane (Internal.Axis2d axis2d) =
    withDirection (Direction3d.on sketchPlane axis2d.direction)
        (Point3d.on sketchPlane axis2d.originPoint)


{-| Get the origin point of an axis.

    Axis3d.originPoint exampleAxis
    --> Point3d.fromCoordinates ( 1, 2, 3 )

-}
originPoint : Axis3d -> Point3d
originPoint (Internal.Axis3d axis) =
    axis.originPoint


{-| Get the direction of an axis.

    Axis3d.direction exampleAxis
    --> Direction3d.y

-}
direction : Axis3d -> Direction3d
direction (Internal.Axis3d axis) =
    axis.direction


{-| Reverse the direction of an axis while keeping the same origin point.

    Axis3d.flip exampleAxis
    --> Axis3d.withDirection Direction3d.negativeY
    -->     (Point3d.fromCoordinates ( 1, 2, 3 ))

-}
flip : Axis3d -> Axis3d
flip (Internal.Axis3d axis) =
    withDirection (Direction3d.flip axis.direction) axis.originPoint


{-| Move an axis so that it has the given origin point but unchanged direction.

    newOrigin =
        Point3d.fromCoordinates ( 3, 4, 5 )

    Axis3d.moveTo newOrigin exampleAxis
    --> Axis3d.withDirection Direction3d.y
    -->     (Point3d.fromCoordinates ( 3, 4, 5 ))

-}
moveTo : Point3d -> Axis3d -> Axis3d
moveTo newOrigin (Internal.Axis3d axis) =
    withDirection axis.direction newOrigin


{-| Rotate an axis around another axis by a given angle. The axis to rotate
around is given first and the axis to rotate is given last.

    Axis3d.rotateAround Axis3d.z (degrees 90) exampleAxis
    --> Axis3d.withDirection Direction3d.negativeX
    -->     (Point3d.fromCoordinates ( -2, 1, 3 ))

-}
rotateAround : Axis3d -> Float -> Axis3d -> Axis3d
rotateAround otherAxis angle =
    let
        rotatePoint =
            Point3d.rotateAround otherAxis angle

        rotateDirection =
            Direction3d.rotateAround otherAxis angle
    in
    \(Internal.Axis3d axis) ->
        withDirection (rotateDirection axis.direction)
            (rotatePoint axis.originPoint)


{-| Translate an axis by a given displacement. Applies the given displacement to
the axis' origin point and leaves the direction unchanged.

    displacement =
        Vector3d.fromComponents ( 3, 3, 3 )

    Axis3d.translateBy displacement exampleAxis
    --> Axis3d.withDirection Direction3d.y
    -->     (Point3d.fromCoordinates ( 4, 5, 6 ))

-}
translateBy : Vector3d -> Axis3d -> Axis3d
translateBy vector (Internal.Axis3d axis) =
    withDirection axis.direction (Point3d.translateBy vector axis.originPoint)


{-| Mirror an axis across a plane.

    Axis3d.mirrorAcross Plane3d.xy exampleAxis
    --> Axis3d.withDirection Direction3d.y
    -->     (Point3d.fromCoordinates ( 1, 2, -3 ))

-}
mirrorAcross : Plane3d -> Axis3d -> Axis3d
mirrorAcross plane (Internal.Axis3d axis) =
    withDirection (Direction3d.mirrorAcross plane axis.direction)
        (Point3d.mirrorAcross plane axis.originPoint)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of an axis onto a plane. If the given axis is exactly perpendicular to the given
plane, returns `Nothing`.

    Axis3d.projectOnto Plane3d.xy exampleAxis
    --> Just
    -->     (Axis3d.withDirection Direction3d.y
    -->         (Point3d.fromCoordinates ( 1, 2, 0 ))
    -->     )

    Axis3d.projectOnto Plane3d.xy Axis3d.z
    --> Nothing

-}
projectOnto : Plane3d -> Axis3d -> Maybe Axis3d
projectOnto plane (Internal.Axis3d axis) =
    case Direction3d.projectOnto plane axis.direction of
        Just projectedDirection ->
            Just <|
                withDirection projectedDirection
                    (Point3d.projectOnto plane axis.originPoint)

        Nothing ->
            Nothing


{-| Take an axis defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 3, 3, 3 ))

    Axis3d.relativeTo localFrame exampleAxis
    --> Axis3d.withDirection Direction3d.y
    -->     (Point3d.fromCoordinates ( -2, -1, 0 ))

-}
relativeTo : Frame3d -> Axis3d -> Axis3d
relativeTo frame (Internal.Axis3d axis) =
    withDirection (Direction3d.relativeTo frame axis.direction)
        (Point3d.relativeTo frame axis.originPoint)


{-| Take an axis defined in local coordinates relative to a given reference
frame, and return that axis expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 3, 3, 3 ))

    Axis3d.placeIn localFrame exampleAxis
    --> Axis3d.withDirection Direction3d.y
    -->     (Point3d.fromCoordinates ( 4, 5, 6 ))

-}
placeIn : Frame3d -> Axis3d -> Axis3d
placeIn frame (Internal.Axis3d axis) =
    withDirection (Direction3d.placeIn frame axis.direction)
        (Point3d.placeIn frame axis.originPoint)


{-| Project an axis into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the axis onto the plane and then expresses the projected axis in 2D sketch
coordinates.

This is only possible if the axis is not perpendicular to the sketch
plane; if it is perpendicular, `Nothing` is returned.

    Axis3d.projectInto SketchPlane3d.xy exampleAxis
    --> Just
    -->     (Axis2d.withDirection Direction2d.y
    -->         (Point2d.fromCoordinates ( 1, 2 ))
    -->     )

    -- The global Y direction is the X direction of the
    -- YZ plane:
    Axis3d.projectInto SketchPlane3d.yz exampleAxis
    --> Just
    -->     (Axis2d.withDirection Direction2d.x
    -->         (Point2d.fromCoordinates ( 2, 3 ))
    -->     )

    Axis3d.projectInto SketchPlane3d.xz exampleAxis
    --> Nothing

-}
projectInto : SketchPlane3d -> Axis3d -> Maybe Axis2d
projectInto sketchPlane (Internal.Axis3d axis) =
    let
        projectedOrigin =
            Point3d.projectInto sketchPlane axis.originPoint
    in
    Direction3d.projectInto sketchPlane axis.direction
        |> Maybe.map (Axis2d.through projectedOrigin)
