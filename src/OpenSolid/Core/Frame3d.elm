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
