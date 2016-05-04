module OpenSolid.Core.BoundingBox3d
  ( fromComponents
  , components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


fromComponents: (Interval, Interval, Interval) -> BoundingBox3d
fromComponents (x, y, z) =
  BoundingBox3d x y z


components: BoundingBox3d -> (Interval, Interval, Interval)
components (BoundingBox3d x y z) =
  (x, y, z)


contains: Point3d -> BoundingBox3d -> Bool
contains (Point3d px py pz) (BoundingBox3d x y z) =
  Interval.contains px x && Interval.contains py y && Interval.contains pz z


overlaps: BoundingBox3d -> BoundingBox3d -> Bool
overlaps (BoundingBox3d x2 y2 z2) (BoundingBox3d x1 y1 z1) =
  Interval.overlaps x2 x1 && Interval.overlaps y2 y1 && Interval.overlaps z2 z1
