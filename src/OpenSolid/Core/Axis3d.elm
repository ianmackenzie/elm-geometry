module  OpenSolid.Core.Axis3d
  ( x
  , y
  , z
  , pointAt
  , normalDirection
  , normalPlane
  , reversed
  , transformedBy
  , projectedOntoPlane
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point3d as Point3d
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
  { axis | direction = Direction3d.negated axis.direction }


transformedBy: Transformation3d -> Axis3d -> Axis3d
transformedBy transformation axis =
  let
    originPoint = Point3d.transformedBy transformation axis.originPoint
    direction = Direction3d.transformedBy transformation axis.direction
  in
    Axis3d originPoint direction


projectedOntoPlane: Plane3d -> Axis3d -> Axis3d
projectedOntoPlane plane axis =
  let
    originPoint = Point3d.projectedOntoPlane plane axis.originPoint
    direction = Vector3d.direction (Vector3d.projectedOntoPlane plane axis.direction)
  in
    Axis3d originPoint direction
