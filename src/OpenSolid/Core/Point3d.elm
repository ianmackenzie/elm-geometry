module OpenSolid.Core.Point3d
  ( origin
  , components
  , squaredDistanceTo
  , distanceTo
  , transformedBy
  , plus
  , minus
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Math3d as Math3d


origin: Point3d
origin =
  Point3d 0 0 0


components: Point3d -> (Float, Float, Float)
components point =
  (point.x, point.y, point.z)


squaredDistanceTo: Point3d -> Point3d -> Float
squaredDistanceTo other =
  minus other >> Vector3d.squaredLength


distanceTo: Point3d -> Point3d -> Float
distanceTo other =
  squaredDistanceTo other >> sqrt


transformedBy: Transformation3d -> Point3d -> Point3d
transformedBy =
  snd


plus: Vector3d -> Point3d -> Point3d
plus =
  Math3d.plus


minus: Point3d -> Point3d -> Vector3d
minus =
  Math3d.minus
