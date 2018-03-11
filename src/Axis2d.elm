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


module Axis2d
    exposing
        ( Axis2d
        , direction
        , flip
        , mirrorAcross
        , moveTo
        , originPoint
        , placeIn
        , relativeTo
        , rotateAround
        , translateBy
        , withDirection
        , x
        , y
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/axis2d.svg" alt="Axis2d" width="160">

An `Axis2d` represents an infinitely long straight line in 2D and is defined by
an origin point and direction. Axes have several uses, such as:

  - Mirroring across the axis
  - Projecting onto the axis
  - Measuring distance along the axis

@docs Axis2d


# Constants

@docs x, y


# Constructors

@docs withDirection


# Properties

@docs originPoint, direction


# Transformations

@docs flip, moveTo, rotateAround, translateBy, mirrorAcross


# Coordinate conversions

Functions for transforming axes between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn

-}

import Direction2d exposing (Direction2d)
import Geometry.Internal as Internal exposing (Frame2d)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Axis2d =
    Internal.Axis2d


{-| The global X axis.

    Axis2d.x
    --> Axis2d.withDirection Direction2d.x Point2d.origin

-}
x : Axis2d
x =
    withDirection Direction2d.x Point2d.origin


{-| The global Y axis.

    Axis2d.y
    --> Axis2d.withDirection Direction2d.y Point2d.origin

-}
y : Axis2d
y =
    withDirection Direction2d.y Point2d.origin


{-| Construct an axis with the given direction having the given origin point:

    exampleAxis =
        Axis2d.withDirection
            (Direction2d.fromAngle (degrees 30))
            (Point2d.fromCoordinates ( 1, 3 ))

-}
withDirection : Direction2d -> Point2d -> Axis2d
withDirection direction originPoint =
    Internal.Axis2d { originPoint = originPoint, direction = direction }


{-| Get the origin point of an axis.

    Axis2d.originPoint exampleAxis
    --> Point2d.fromCoordinates ( 1, 3 )

-}
originPoint : Axis2d -> Point2d
originPoint (Internal.Axis2d axis) =
    axis.originPoint


{-| Get the direction of an axis.

    Axis2d.direction exampleAxis
    --> Direction2d.fromAngle (degrees 30)

-}
direction : Axis2d -> Direction2d
direction (Internal.Axis2d axis) =
    axis.direction


{-| Reverse the direction of an axis while keeping the same origin point.

    Axis2d.flip exampleAxis
    --> Axis2d.withDirection
    -->     (Direction2d.fromAngle (degrees -150))
    -->     (Point2d.fromCoordinates ( 1, 3 ))

-}
flip : Axis2d -> Axis2d
flip (Internal.Axis2d axis) =
    withDirection (Direction2d.flip axis.direction) axis.originPoint


{-| Move an axis so that it has the given origin point but unchanged direction.

    newOrigin =
        Point2d.fromCoordinates ( 4, 5 )

    Axis2d.moveTo newOrigin exampleAxis
    --> Axis2d.withDirection
    -->     (Direction2d.fromAngle (degrees 30))
    -->     (Point2d.fromCoordinates ( 4, 5 ))

-}
moveTo : Point2d -> Axis2d -> Axis2d
moveTo newOrigin (Internal.Axis2d axis) =
    withDirection axis.direction newOrigin


{-| Rotate an axis around a given center point by a given angle. Rotates the
axis' origin point around the given point by the given angle and the axis'
direction by the given angle.

    exampleAxis
        |> Axis2d.rotateAround Point2d.origin (degrees 90)
    --> Axis2d.withDirection
    -->     (Direction2d.fromAngle (degrees 120))
    -->     (Point2d.fromCoordinates ( -3, 1 ))

-}
rotateAround : Point2d -> Float -> Axis2d -> Axis2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \(Internal.Axis2d axis) ->
        withDirection (rotateDirection axis.direction)
            (rotatePoint axis.originPoint)


{-| Translate an axis by a given displacement. Applies the given displacement to
the axis' origin point and leaves the direction unchanged.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    Axis2d.translateBy displacement exampleAxis
    --> Axis2d.withDirection
    -->     (Direction2d.fromAngle (degrees 30))
    -->     (Point2d.fromCoordinates ( 3, 6 ))

-}
translateBy : Vector2d -> Axis2d -> Axis2d
translateBy vector (Internal.Axis2d axis) =
    withDirection axis.direction (Point2d.translateBy vector axis.originPoint)


{-| Mirror one axis across another. The axis to mirror across is given first and
the axis to mirror is given second.

    Axis2d.mirrorAcross Axis2d.x exampleAxis
    --> Axis2d.withDirection
    -->     (Direction2d.fromAngle (degrees -30))
    -->     (Point2d.fromCoordinates ( 1, -3 ))

-}
mirrorAcross : Axis2d -> Axis2d -> Axis2d
mirrorAcross otherAxis (Internal.Axis2d axis) =
    withDirection (Direction2d.mirrorAcross otherAxis axis.direction)
        (Point2d.mirrorAcross otherAxis axis.originPoint)


{-| Take an axis defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Axis2d.relativeTo frame exampleAxis
    --> Axis2d.withDirection
    -->     (Direction2d.fromAngle (degrees 30))
    -->     (Point2d.fromCoordinates ( -1, 0 ))

-}
relativeTo : Frame2d -> Axis2d -> Axis2d
relativeTo frame (Internal.Axis2d axis) =
    withDirection (Direction2d.relativeTo frame axis.direction)
        (Point2d.relativeTo frame axis.originPoint)


{-| Take an axis defined in local coordinates relative to a given reference
frame, and return that axis expressed in global coordinates.

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Axis2d.placeIn frame exampleAxis
    --> Axis2d.withDirection
    -->     (Direction2d.fromAngle (degrees 30))
    -->     (Point2d.fromCoordinates ( 3, 6 ))

-}
placeIn : Frame2d -> Axis2d -> Axis2d
placeIn frame (Internal.Axis2d axis) =
    withDirection (Direction2d.placeIn frame axis.direction)
        (Point2d.placeIn frame axis.originPoint)
