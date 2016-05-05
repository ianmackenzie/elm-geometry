module  OpenSolid.Core.Plane3d
  ( xy
  , xz
  , yx
  , yz
  , zx
  , zy
  , fromPointAndNormal
  , offsetBy
  , flipped
  , normalAxis
  , scaledAbout
  , rotatedAbout
  , translatedBy
  , mirroredAbout
  , relativeTo
  , placedIn
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


xy: Plane3d
xy =
  Plane3d Point3d.origin Direction3d.x Direction3d.y Direction3d.z


xz: Plane3d
xz =
  Plane3d Point3d.origin Direction3d.x Direction3d.z (Direction3d.negated Direction3d.y)


yx: Plane3d
yx =
  Plane3d Point3d.origin Direction3d.y Direction3d.x (Direction3d.negated Direction3d.z)


yz: Plane3d
yz =
  Plane3d Point3d.origin Direction3d.y Direction3d.z Direction3d.x


zx: Plane3d
zx =
  Plane3d Point3d.origin Direction3d.z Direction3d.x Direction3d.y


zy: Plane3d
zy =
  Plane3d Point3d.origin Direction3d.z Direction3d.y (Direction3d.negated Direction3d.x)


fromPointAndNormal: Point3d -> Direction3d -> Plane3d
fromPointAndNormal originPoint normalDirection =
  let
    (xDirection, yDirection) = Direction3d.normalBasis normalDirection
  in
    Plane3d originPoint xDirection yDirection normalDirection


offsetBy: Float -> Plane3d -> Plane3d
offsetBy distance plane =
  translatedBy (Direction3d.times distance plane.normalDirection) plane


flipped: Plane3d -> Plane3d
flipped plane =
  { plane | normalDirection = Direction3d.negated plane.normalDirection }


normalAxis: Plane3d -> Axis3d
normalAxis plane =
  Axis3d plane.originPoint plane.normalDirection


scaledAbout: Point3d -> Float -> Plane3d -> Plane3d
scaledAbout centerPoint scale plane =
  { plane | originPoint = Point3d.scaledAbout centerPoint scale plane.originPoint }


rotatedAbout: Axis3d -> Float -> Plane3d -> Plane3d
rotatedAbout axis angle =
  let
    rotatePoint = Point3d.rotatedAbout axis angle
    rotateDirection = Direction3d.rotatedAbout axis.direction angle
  in
    \plane ->
      let
        originPoint = rotatePoint plane.originPoint
        xDirection = rotateDirection plane.xDirection
        yDirection = rotateDirection plane.yDirection
        normalDirection = rotateDirection plane.normalDirection
      in
        Plane3d originPoint xDirection yDirection normalDirection


translatedBy: Vector3d -> Plane3d -> Plane3d
translatedBy vector plane =
  { plane | originPoint = Point3d.plus vector plane.originPoint }


mirroredAbout: Plane3d -> Plane3d -> Plane3d
mirroredAbout plane =
  let
    mirrorPoint = Point3d.mirroredAbout plane
    mirrorDirection = Direction3d.mirroredAlong plane.normalDirection
  in
    \plane ->
      let
        originPoint = mirrorPoint plane.originPoint
        xDirection = mirrorDirection plane.xDirection
        yDirection = mirrorDirection plane.yDirection
        normalDirection = mirrorDirection plane.normalDirection
      in
        Plane3d originPoint xDirection yDirection normalDirection


relativeTo: Frame3d -> Plane3d -> Plane3d
relativeTo frame =
  let
    localizePoint = Point3d.relativeTo frame
    localizeDirection = Direction3d.relativeTo frame
  in
    \plane ->
      let
        originPoint = localizePoint plane.originPoint
        xDirection = localizeDirection plane.xDirection
        yDirection = localizeDirection plane.yDirection
        normalDirection = localizeDirection plane.normalDirection
      in
        Plane3d originPoint xDirection yDirection normalDirection


placedIn: Frame3d -> Plane3d -> Plane3d
placedIn frame =
  let
    globalizePoint = Point3d.placedIn frame
    globalizeDirection = Direction3d.placedIn frame
  in
    \plane ->
      let
        originPoint = globalizePoint plane.originPoint
        xDirection = globalizeDirection plane.xDirection
        yDirection = globalizeDirection plane.yDirection
        normalDirection = globalizeDirection plane.normalDirection
      in
        Plane3d originPoint xDirection yDirection normalDirection
