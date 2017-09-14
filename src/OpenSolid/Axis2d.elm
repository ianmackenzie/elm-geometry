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


module OpenSolid.Axis2d
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
        , with
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

@docs with


# Properties

@docs originPoint, direction


# Transformations

@docs flip, moveTo, rotateAround, translateBy, mirrorAcross


# Coordinate conversions

Functions for transforming axes between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn

-}

import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Geometry.Internal as Internal exposing (Frame2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


{-| -}
type alias Axis2d =
    Internal.Axis2d


{-| The global X axis.

    Axis2d.x
    --> Axis2d.with
    -->     { originPoint = Point2d.origin
    -->     , direction = Direction2d.x
    -->     }

-}
x : Axis2d
x =
    with { originPoint = Point2d.origin, direction = Direction2d.x }


{-| The global Y axis.

    Axis2d.y
    --> Axis2d.with
    -->     { originPoint = Point2d.origin
    -->     , direction = Direction2d.y
    -->     }

-}
y : Axis2d
y =
    with { originPoint = Point2d.origin, direction = Direction2d.y }


{-| Construct an axis from its origin point and direction:

    exampleAxis =
        Axis2d.with
            { originPoint =
                Point2d.fromCoordinates ( 1, 3 )
            , direction =
                Direction2d.fromAngle (degrees 30)
            }

-}
with : { originPoint : Point2d, direction : Direction2d } -> Axis2d
with =
    Internal.Axis2d


{-| Get the origin point of an axis.

    Axis2d.originPoint exampleAxis
    --> Point2d.fromCoordinates ( 1, 3 )

-}
originPoint : Axis2d -> Point2d
originPoint (Internal.Axis2d properties) =
    properties.originPoint


{-| Get the direction of an axis.

    Axis2d.direction exampleAxis
    --> Direction2d.fromAngle (degrees 30)

-}
direction : Axis2d -> Direction2d
direction (Internal.Axis2d properties) =
    properties.direction


{-| Reverse the direction of an axis while keeping the same origin point.

    Axis2d.flip exampleAxis
    --> Axis2d.with
    -->     { originPoint =
    -->         Point2d.fromCoordinates ( 1, 3 )
    -->     , direction =
    -->         Direction2d.fromAngle (degrees -150)
    -->     }

-}
flip : Axis2d -> Axis2d
flip axis =
    with
        { originPoint = originPoint axis
        , direction = Direction2d.flip (direction axis)
        }


{-| Move an axis so that it has the given origin point but unchanged direction.

    newOrigin =
        Point2d.fromCoordinates ( 4, 5 )

    Axis2d.moveTo newOrigin exampleAxis
    --> Axis2d.with
    -->     { originPoint =
    -->         Point2d.fromCoordinates ( 4, 5 )
    -->     , direction =
    -->         Direction2d.fromAngle (degrees 30)
    -->     }

-}
moveTo : Point2d -> Axis2d -> Axis2d
moveTo newOrigin axis =
    with { originPoint = newOrigin, direction = direction axis }


{-| Rotate an axis around a given center point by a given angle. Rotates the
axis' origin point around the given point by the given angle and the axis'
direction by the given angle.

    Axis2d.rotateAround Point2d.origin
        (degrees 90)
        exampleAxis
    --> Axis2d.with
    -->     { originPoint =
    -->         Point2d.fromCoordinates ( -3, 1 )
    -->     , direction =
    -->         Direction2d.fromAngle (degrees 120)
    -->     }

-}
rotateAround : Point2d -> Float -> Axis2d -> Axis2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \axis ->
        with
            { originPoint = rotatePoint (originPoint axis)
            , direction = rotateDirection (direction axis)
            }


{-| Translate an axis by a given displacement. Applies the given displacement to
the axis' origin point and leaves the direction unchanged.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    Axis2d.translateBy displacement exampleAxis
    --> Axis2d.with
    -->     { originPoint =
    -->         Point2d.fromCoordinates ( 3, 6 )
    -->     , direction =
    -->         Direction2d.fromAngle (degrees 30)
    -->     }

-}
translateBy : Vector2d -> Axis2d -> Axis2d
translateBy vector axis =
    with
        { originPoint = Point2d.translateBy vector (originPoint axis)
        , direction = direction axis
        }


{-| Mirror one axis across another. The axis to mirror across is given first and
the axis to mirror is given second.

    Axis2d.mirrorAcross Axis2d.x exampleAxis
    --> Axis2d.with
    -->     { originPoint =
    -->         Point2d.fromCoordinates ( 1, -3 )
    -->     , direction =
    -->         Direction2d.fromAngle (degrees -30)
    -->     }

-}
mirrorAcross : Axis2d -> Axis2d -> Axis2d
mirrorAcross otherAxis =
    let
        mirrorPoint =
            Point2d.mirrorAcross otherAxis

        mirrorDirection =
            Direction2d.mirrorAcross otherAxis
    in
    \axis ->
        with
            { originPoint = mirrorPoint (originPoint axis)
            , direction = mirrorDirection (direction axis)
            }


{-| Take an axis defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Axis2d.relativeTo frame exampleAxis
    --> Axis2d.with
    -->     { originPoint =
    -->         Point2d.fromCoordinates ( -1, 0 )
    -->     , direction =
    -->         Direction2d.fromAngle (degrees 30)
    -->     }

-}
relativeTo : Frame2d -> Axis2d -> Axis2d
relativeTo frame =
    let
        relativePoint =
            Point2d.relativeTo frame

        relativeDirection =
            Direction2d.relativeTo frame
    in
    \axis ->
        with
            { originPoint = relativePoint (originPoint axis)
            , direction = relativeDirection (direction axis)
            }


{-| Take an axis defined in local coordinates relative to a given reference
frame, and return that axis expressed in global coordinates.

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Axis2d.placeIn frame exampleAxis
    --> Axis2d.with
    -->     { originPoint =
    -->         Point2d.fromCoordinates ( 3, 6 )
    -->     , direction =
    -->         Direction2d.fromAngle (degrees 30)
    -->     }

-}
placeIn : Frame2d -> Axis2d -> Axis2d
placeIn frame =
    let
        placePoint =
            Point2d.placeIn frame

        placeDirection =
            Direction2d.placeIn frame
    in
    \axis ->
        with
            { originPoint = placePoint (originPoint axis)
            , direction = placeDirection (direction axis)
            }
