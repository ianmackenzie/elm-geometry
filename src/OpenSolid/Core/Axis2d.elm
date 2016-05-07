module OpenSolid.Core.Axis2d
  ( x
  , y
  , point
  , segment
  , normalDirection
  , normalAxis
  , reversed
  , scaledAbout
  , rotatedAbout
  , translatedBy
  , mirroredAbout
  , relativeTo
  , placedIn
  , placedOnto
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Direction2d as Direction2d


x: Axis2d
x =
  Axis2d Point2d.origin Direction2d.x


y: Axis2d
y =
  Axis2d Point2d.origin Direction2d.y


point: Float -> Axis2d -> Point2d
point distance axis =
  Point2d.plus (Direction2d.times distance axis.direction) axis.originPoint


segment: Float -> Float -> Axis2d -> LineSegment2d
segment start end axis =
  LineSegment2d (point start axis) (point end axis)


normalDirection: Axis2d -> Direction2d
normalDirection axis =
  Direction2d.normalDirection axis.direction


normalAxis: Axis2d -> Axis2d
normalAxis axis =
  Axis2d axis.originPoint (normalDirection axis)


reversed: Axis2d -> Axis2d
reversed axis =
  Axis2d axis.originPoint (Direction2d.negated axis.direction)


scaledAbout: Point2d -> Float -> Axis2d -> Axis2d
scaledAbout centerPoint scale axis =
  Axis2d (Point2d.scaledAbout centerPoint scale axis.originPoint) axis.direction


rotatedAbout: Point2d -> Float -> Axis2d -> Axis2d
rotatedAbout centerPoint angle =
  let
    rotatePoint = Point2d.rotatedAbout centerPoint angle
    rotateDirection = Direction2d.rotatedBy angle
  in
    \axis -> Axis2d (rotatePoint axis.originPoint) (rotateDirection axis.direction)


translatedBy: Vector2d -> Axis2d -> Axis2d
translatedBy vector axis =
  Axis2d (Point2d.plus vector axis.originPoint) axis.direction


mirroredAbout: Axis2d -> Axis2d -> Axis2d
mirroredAbout otherAxis =
  let
    mirrorPoint = Point2d.mirroredAbout otherAxis
    mirrorDirection = Direction2d.mirroredAbout otherAxis.direction
  in
    \axis -> Axis2d (mirrorPoint axis.originPoint) (mirrorDirection axis.direction)


relativeTo: Frame2d -> Axis2d -> Axis2d
relativeTo frame =
  let
    localizePoint = Point2d.relativeTo frame
    localizeDirection = Direction2d.relativeTo frame
  in
    \axis -> Axis2d (localizePoint axis.originPoint) (localizeDirection axis.direction)


placedIn: Frame2d -> Axis2d -> Axis2d
placedIn frame =
  let
    globalizePoint = Point2d.placedIn frame
    globalizeDirection = Direction2d.placedIn frame
  in
    \axis -> Axis2d (globalizePoint axis.originPoint) (globalizeDirection axis.direction)


placedOnto: Plane3d -> Axis2d -> Axis3d
placedOnto plane =
  let
    placePoint = Point2d.placedOnto plane
    placeDirection = Direction2d.placedOnto plane
  in
    \axis -> Axis3d (placePoint axis.originPoint) (placeDirection axis.direction)
