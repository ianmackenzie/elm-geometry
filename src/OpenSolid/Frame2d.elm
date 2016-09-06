{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Frame2d
    exposing
        ( xy
        , at
        , originPoint
        , xDirection
        , yDirection
        , xAxis
        , yAxis
        , flipX
        , flipY
        , rotateBy
        , rotateAround
        , translateBy
        , translateAlongOwn
        , moveTo
        , mirrorAcross
        , relativeTo
        , placeIn
        )

{-| Various functions for creating and working with `Frame2d` values. For the
examples below, assume that all OpenSolid core types have been imported using

    import OpenSolid.Core.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.Frame2d as Frame2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Predefined frames

@docs xy

# Constructors

Frames can by constructed by passing a record with `originPoint`, `xDirection`
and 'yDirection' fields to the `Frame2d` constructor, for example:

    frame =
        Frame2d
            { originPoint = Point2d ( 2, 3 )
            , xDirection = Direction2d.fromAngle (degrees 45)
            , yDirection = Direction2d.fromAngle (degrees 135)
            }

In this case you must be careful to ensure that the X and Y directions are
perpendicular to each other.

@docs at

# Accessors

@docs originPoint, xDirection, yDirection, xAxis, yAxis

# Transformations

@docs flipX, flipY, rotateBy, rotateAround, translateBy, translateAlongOwn, moveTo, mirrorAcross

# Coordinate frames

Functions for expressing frames relative to other frames. These can be
confusing, and are likely to be necessary only in fairly specialized situations,
but are provided for consistency with the `relativeTo` and `placeIn` functions
in other modules.

@docs relativeTo, placeIn
-}

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Axis2d as Axis2d


xy : Frame2d
xy =
    at Point2d.origin


at : Point2d -> Frame2d
at point =
    Frame2d
        { originPoint = point
        , xDirection = Direction2d.x
        , yDirection = Direction2d.y
        }


originPoint : Frame2d -> Point2d
originPoint (Frame2d properties) =
    properties.originPoint


xDirection : Frame2d -> Direction2d
xDirection (Frame2d properties) =
    properties.xDirection


yDirection : Frame2d -> Direction2d
yDirection (Frame2d properties) =
    properties.yDirection


xAxis : Frame2d -> Axis2d
xAxis frame =
    Axis2d { originPoint = originPoint frame, direction = xDirection frame }


yAxis : Frame2d -> Axis2d
yAxis frame =
    Axis2d { originPoint = originPoint frame, direction = yDirection frame }


flipX : Frame2d -> Frame2d
flipX frame =
    Frame2d
        { originPoint = originPoint frame
        , xDirection = Direction2d.negate (xDirection frame)
        , yDirection = yDirection frame
        }


flipY : Frame2d -> Frame2d
flipY frame =
    Frame2d
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = Direction2d.negate (yDirection frame)
        }


rotateBy : Float -> Frame2d -> Frame2d
rotateBy angle frame =
    let
        rotateDirection =
            Direction2d.rotateBy angle
    in
        Frame2d
            { originPoint = originPoint frame
            , xDirection = rotateDirection (xDirection frame)
            , yDirection = rotateDirection (yDirection frame)
            }


rotateAround : Point2d -> Float -> Frame2d -> Frame2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
        \frame ->
            Frame2d
                { originPoint = rotatePoint (originPoint frame)
                , xDirection = rotateDirection (xDirection frame)
                , yDirection = rotateDirection (yDirection frame)
                }


translateBy : Vector2d -> Frame2d -> Frame2d
translateBy vector frame =
    Frame2d
        { originPoint = Point2d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


translateAlongOwn : (Frame2d -> Axis2d) -> Float -> Frame2d -> Frame2d
translateAlongOwn axis distance frame =
    let
        displacement =
            Direction2d.times distance (Axis2d.direction (axis frame))
    in
        translateBy displacement frame


moveTo : Point2d -> Frame2d -> Frame2d
moveTo newOrigin frame =
    Frame2d
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


mirrorAcross : Axis2d -> Frame2d -> Frame2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAcross axis
    in
        \frame ->
            Frame2d
                { originPoint = mirrorPoint (originPoint frame)
                , xDirection = mirrorDirection (xDirection frame)
                , yDirection = mirrorDirection (yDirection frame)
                }


relativeTo : Frame2d -> Frame2d -> Frame2d
relativeTo otherFrame =
    let
        relativePoint =
            Point2d.relativeTo otherFrame

        relativeDirection =
            Direction2d.relativeTo otherFrame
    in
        \frame ->
            Frame2d
                { originPoint = relativePoint (originPoint frame)
                , xDirection = relativeDirection (xDirection frame)
                , yDirection = relativeDirection (yDirection frame)
                }


placeIn : Frame2d -> Frame2d -> Frame2d
placeIn otherFrame =
    let
        placePoint =
            Point2d.placeIn otherFrame

        placeDirection =
            Direction2d.placeIn otherFrame
    in
        \frame ->
            Frame2d
                { originPoint = placePoint (originPoint frame)
                , xDirection = placeDirection (xDirection frame)
                , yDirection = placeDirection (yDirection frame)
                }
