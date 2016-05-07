module  OpenSolid.Core.Axis3d
  ( x
  , y
  , z
  , point
  , normalDirection
  , normalPlane
  , reversed
  , scaledAbout
  , rotatedAbout
  , translatedBy
  , mirroredAbout
  , relativeTo
  , placedIn
  , projectedOnto
  , projectedInto
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Plane3d as Plane3d


x: Axis3d
x =
  Axis3d Point3d.origin Direction3d.x


y: Axis3d
y =
  Axis3d Point3d.origin Direction3d.y


z: Axis3d
z =
  Axis3d Point3d.origin Direction3d.z


point: Float -> Axis3d -> Point3d
point distance axis =
  Point3d.plus (Direction3d.times distance axis.direction) axis.originPoint


normalDirection: Axis3d -> Direction3d
normalDirection axis =
  Direction3d.normalDirection axis.direction


normalPlane: Axis3d -> Plane3d
normalPlane axis =
  Plane3d.fromPointAndNormal axis.originPoint axis.direction


reversed: Axis3d -> Axis3d
reversed axis =
  Axis3d axis.originPoint (Direction3d.negated axis.direction)


scaledAbout: Point3d -> Float -> Axis3d -> Axis3d
scaledAbout centerPoint scale axis =
  Axis3d (Point3d.scaledAbout centerPoint scale axis.originPoint) axis.direction


rotatedAbout: Axis3d -> Float -> Axis3d -> Axis3d
rotatedAbout otherAxis angle =
  let
    rotatePoint = Point3d.rotatedAbout otherAxis angle
    rotateDirection = Direction3d.rotatedAbout otherAxis.direction angle
  in
    \axis -> Axis3d (rotatePoint axis.originPoint) (rotateDirection axis.direction)


translatedBy: Vector3d -> Axis3d -> Axis3d
translatedBy vector axis =
  Axis3d (Point3d.plus vector axis.originPoint) axis.direction


mirroredAbout: Plane3d -> Axis3d -> Axis3d
mirroredAbout plane =
  let
    mirrorPoint = Point3d.mirroredAbout plane
    mirrorDirection = Direction3d.mirroredAlong plane.normalDirection
  in
    \axis -> Axis3d (mirrorPoint axis.originPoint) (mirrorDirection axis.direction)


relativeTo: Frame3d -> Axis3d -> Axis3d
relativeTo frame =
  let
    localizePoint = Point3d.relativeTo frame
    localizeDirection = Direction3d.relativeTo frame
  in
    \axis -> Axis3d (localizePoint axis.originPoint) (localizeDirection axis.direction)


placedIn: Frame3d -> Axis3d -> Axis3d
placedIn frame =
  let
    globalizePoint = Point3d.placedIn frame
    globalizeDirection = Direction3d.placedIn frame
  in
    \axis -> Axis3d (globalizePoint axis.originPoint) (globalizeDirection axis.direction)


projectedOnto: Plane3d -> Axis3d -> Maybe Axis3d
projectedOnto plane axis =
  let
    projectedOrigin = Point3d.projectedOnto plane axis.originPoint
  in
    Maybe.map (Axis3d projectedOrigin) (Direction3d.projectedOnto plane axis.direction)


projectedInto: Plane3d -> Axis3d -> Maybe Axis2d
projectedInto plane axis =
  let
    projectedOrigin = Point3d.projectedInto plane axis.originPoint
  in
    Maybe.map (Axis2d projectedOrigin) (Direction3d.projectedInto plane axis.direction)
