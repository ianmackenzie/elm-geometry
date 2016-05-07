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
  , scaleAbout
  , rotateAbout
  , translateBy
  , mirrorAbout
  , relativeTo
  , placeIn
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
  Plane3d frame.originPoint frame.xDirection frame.zDirection (Direction3d.negate frame.yDirection)


yxPlane: Frame3d -> Plane3d
yxPlane frame =
  Plane3d frame.originPoint frame.yDirection frame.xDirection (Direction3d.negate frame.zDirection)


yzPlane: Frame3d -> Plane3d
yzPlane frame =
  Plane3d frame.originPoint frame.yDirection frame.zDirection frame.xDirection


zxPlane: Frame3d -> Plane3d
zxPlane frame =
  Plane3d frame.originPoint frame.zDirection frame.xDirection frame.yDirection


zyPlane: Frame3d -> Plane3d
zyPlane frame =
  Plane3d frame.originPoint frame.zDirection frame.yDirection (Direction3d.negate frame.xDirection)


scaleAbout: Point3d -> Float -> Frame3d -> Frame3d
scaleAbout centerPoint scale frame =
  { frame | originPoint = Point3d.scaleAbout centerPoint scale frame.originPoint }


rotateAbout: Axis3d -> Float -> Frame3d -> Frame3d
rotateAbout axis angle =
  let
    rotatePoint = Point3d.rotateAbout axis angle
    rotateDirection = Direction3d.rotateAbout axis.direction angle
  in
    \frame ->
      let
        originPoint = rotatePoint frame.originPoint
        xDirection = rotateDirection frame.xDirection
        yDirection = rotateDirection frame.yDirection
        zDirection = rotateDirection frame.zDirection
      in
        Frame3d originPoint xDirection yDirection zDirection


translateBy: Vector3d -> Frame3d -> Frame3d
translateBy vector frame =
  { frame | originPoint = Point3d.plus vector frame.originPoint }


mirrorAbout: Plane3d -> Frame3d -> Frame3d
mirrorAbout plane =
  let
    mirrorPoint = Point3d.mirrorAbout plane
    mirrorDirection = Direction3d.mirrorAlong plane.normalDirection
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


placeIn: Frame3d -> Frame3d -> Frame3d
placeIn frame =
  let
    globalizePoint = Point3d.placeIn frame
    globalizeDirection = Direction3d.placeIn frame
  in
    \frame ->
      let
        originPoint = globalizePoint frame.originPoint
        xDirection = globalizeDirection frame.xDirection
        yDirection = globalizeDirection frame.yDirection
        zDirection = globalizeDirection frame.zDirection
      in
        Frame3d originPoint xDirection yDirection zDirection
