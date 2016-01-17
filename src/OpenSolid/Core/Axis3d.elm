module  OpenSolid.Core.Axis3d
  ( x
  , y
  , z
  , pointAt
  , normalDirection
  , normalPlane
  , reversed
  , transformedBy
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point3d as Point3d
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


pointAt: Float -> Axis3d -> Point3d
pointAt distance axis =
  Point3d.plus (Direction3d.times distance axis.direction) axis.originPoint


normalDirection: Axis3d -> Direction3d
normalDirection =
  .direction >> Direction3d.normalDirection


normalPlane: Axis3d -> Plane3d
normalPlane axis =
  Plane3d.fromPointAndNormal axis.originPoint axis.direction


reversed: Axis3d -> Axis3d
reversed axis =
  { axis | direction = Direction3d.reversed axis.direction }


transformedBy: Transformation3d -> Axis3d -> Axis3d
transformedBy transformation axis =
  let
    transformedOriginPoint = Point3d.transformedBy transformation axis.originPoint
    transformedDirection = Direction3d.transformedBy transformation axis.direction
  in
    Axis3d transformedOriginPoint transformedDirection
