module OpenSolid.Axis2d
    exposing
        ( x
        , y
        , point
        , normalDirection
        , normalAxis
        , reverse
        , scaleAbout
        , rotateAbout
        , translateBy
        , mirrorAbout
        , relativeTo
        , placeIn
        , placeOnto
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d


x : Axis2d
x =
    Axis2d Point2d.origin Direction2d.x


y : Axis2d
y =
    Axis2d Point2d.origin Direction2d.y


point : Float -> Axis2d -> Point2d
point distance axis =
    Point2d.plus (Direction2d.times distance axis.direction) axis.originPoint


normalDirection : Axis2d -> Direction2d
normalDirection axis =
    Direction2d.normalDirection axis.direction


normalAxis : Axis2d -> Axis2d
normalAxis axis =
    Axis2d axis.originPoint (normalDirection axis)


reverse : Axis2d -> Axis2d
reverse axis =
    Axis2d axis.originPoint (Direction2d.negate axis.direction)


scaleAbout : Point2d -> Float -> Axis2d -> Axis2d
scaleAbout centerPoint scale axis =
    let
        scalePoint =
            Point2d.scaleAbout centerPoint scale
    in
        Axis2d (scalePoint axis.originPoint) axis.direction


rotateAbout : Point2d -> Float -> Axis2d -> Axis2d
rotateAbout centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAbout centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
        \axis ->
            Axis2d (rotatePoint axis.originPoint)
                (rotateDirection axis.direction)


translateBy : Vector2d -> Axis2d -> Axis2d
translateBy vector axis =
    Axis2d (Point2d.plus vector axis.originPoint) axis.direction


mirrorAbout : Axis2d -> Axis2d -> Axis2d
mirrorAbout otherAxis =
    let
        mirrorPoint =
            Point2d.mirrorAbout otherAxis

        mirrorDirection =
            Direction2d.mirrorAbout otherAxis.direction
    in
        \axis ->
            Axis2d (mirrorPoint axis.originPoint)
                (mirrorDirection axis.direction)


relativeTo : Frame2d -> Axis2d -> Axis2d
relativeTo frame =
    let
        localizePoint =
            Point2d.relativeTo frame

        localizeDirection =
            Direction2d.relativeTo frame
    in
        \axis ->
            Axis2d (localizePoint axis.originPoint)
                (localizeDirection axis.direction)


placeIn : Frame2d -> Axis2d -> Axis2d
placeIn frame =
    let
        globalizePoint =
            Point2d.placeIn frame

        globalizeDirection =
            Direction2d.placeIn frame
    in
        \axis ->
            Axis2d (globalizePoint axis.originPoint)
                (globalizeDirection axis.direction)


placeOnto : Plane3d -> Axis2d -> Axis3d
placeOnto plane =
    let
        placePoint =
            Point2d.placeOnto plane

        placeDirection =
            Direction2d.placeOnto plane
    in
        \axis ->
            Axis3d (placePoint axis.originPoint)
                (placeDirection axis.direction)
