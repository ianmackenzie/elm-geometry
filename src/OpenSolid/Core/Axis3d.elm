module  OpenSolid.Core.Axis3d
  ( x
  , y
  , z
  , pointAt
  , normalDirection
  , normalPlane
  , reversed
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


pointAt: Float -> Axis3d -> Point3d
pointAt distance axis =
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
