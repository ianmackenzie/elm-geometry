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
  minus other >> Vector3d.length


transformedBy: Transformation3d -> Point3d -> Point3d
transformedBy =
  snd


plus: Vector3d -> Point3d -> Point3d
plus vector point =
  Point3d (point.x + vector.x) (point.y + vector.y)


minus: Point3d -> Point3d -> Vector3d
minus other point =
  Vector3d (point.x - other.x) (point.y - other.y)
