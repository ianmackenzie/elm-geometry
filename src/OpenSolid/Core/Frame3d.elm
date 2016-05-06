module OpenSolid.Core.Frame3d
  ( global
  , xAxis
  , yAxis
  , zAxis
  , xyPlane
  , xzPlane
  , yxPlane
  , yzPlane
  , zxPlane
  , zyPlane
  , scaledAbout
  , rotatedAbout
  , translatedBy
  , mirroredAbout
  , relativeTo
  , placedIn
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Direction3d as Direction3d


global: Frame3d
global =
  Frame3d Point3d.origin Direction3d.x Direction3d.y Direction3d.z


xAxis: Frame3d -> Axis3d
xAxis frame =
  Axis3d frame.originPoint frame.xDirection


yAxis: Frame3d -> Axis3d
yAxis frame =
  Axis3d frame.originPoint frame.yDirection


zAxis: Frame3d -> Axis3d
zAxis frame =
  Axis3d frame.originPoint frame.zDirection


xyPlane: Frame3d -> Plane3d
xyPlane frame =
  Plane3d frame.originPoint frame.xDirection frame.yDirection frame.zDirection


xzPlane: Frame3d -> Plane3d
xzPlane frame =
  Plane3d frame.originPoint frame.xDirection frame.zDirection (Direction3d.negated frame.yDirection)


yxPlane: Frame3d -> Plane3d
yxPlane frame =
  Plane3d frame.originPoint frame.yDirection frame.xDirection (Direction3d.negated frame.zDirection)


yzPlane: Frame3d -> Plane3d
yzPlane frame =
  Plane3d frame.originPoint frame.yDirection frame.zDirection frame.xDirection


zxPlane: Frame3d -> Plane3d
zxPlane frame =
  Plane3d frame.originPoint frame.zDirection frame.xDirection frame.yDirection


zyPlane: Frame3d -> Plane3d
zyPlane frame =
  Plane3d frame.originPoint frame.zDirection frame.yDirection (Direction3d.negated frame.xDirection)


scaledAbout: Point3d -> Float -> Frame3d -> Frame3d
scaledAbout centerPoint scale frame =
  { frame | originPoint = Point3d.scaledAbout centerPoint scale frame.originPoint }


rotatedAbout: Axis3d -> Float -> Frame3d -> Frame3d
rotatedAbout axis angle =
  let
    rotatePoint = Point3d.rotatedAbout axis angle
    rotateDirection = Direction3d.rotatedAbout axis.direction angle
  in
    \frame ->
      let
        originPoint = rotatePoint frame.originPoint
        xDirection = rotateDirection frame.xDirection
        yDirection = rotateDirection frame.yDirection
        zDirection = rotateDirection frame.zDirection
      in
        Frame3d originPoint xDirection yDirection zDirection


translatedBy: Vector3d -> Frame3d -> Frame3d
translatedBy vector frame =
  { frame | originPoint = Point3d.plus vector frame.originPoint }


mirroredAbout: Plane3d -> Frame3d -> Frame3d
mirroredAbout plane =
  let
    mirrorPoint = Point3d.mirroredAbout plane
    mirrorDirection = Direction3d.mirroredAlong plane.normalDirection
  in
    \frame ->
      let
        originPoint = mirrorPoint frame.originPoint
        xDirection = mirrorDirection frame.xDirection
        yDirection = mirrorDirection frame.yDirection
        zDirection = mirrorDirection frame.zDirection
      in
        Frame3d originPoint xDirection yDirection zDirection


relativeTo: Frame3d -> Frame3d -> Frame3d
relativeTo otherFrame =
  let
    localizePoint = Point3d.relativeTo otherFrame
    localizeDirection = Direction3d.relativeTo otherFrame
  in
    \frame ->
      let
        originPoint = localizePoint frame.originPoint
        xDirection = localizeDirection frame.xDirection
        yDirection = localizeDirection frame.yDirection
        zDirection = localizeDirection frame.zDirection
      in
        Frame3d originPoint xDirection yDirection zDirection


placedIn: Frame3d -> Frame3d -> Frame3d
placedIn frame =
  let
    globalizePoint = Point3d.placedIn frame
    globalizeDirection = Direction3d.placedIn frame
  in
    \frame ->
      let
        originPoint = globalizePoint frame.originPoint
        xDirection = globalizeDirection frame.xDirection
        yDirection = globalizeDirection frame.yDirection
        zDirection = globalizeDirection frame.zDirection
      in
        Frame3d originPoint xDirection yDirection zDirection
