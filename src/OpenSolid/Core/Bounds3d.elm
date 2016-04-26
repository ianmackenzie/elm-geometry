module OpenSolid.Core.Bounds3d
  ( fromComponents
  , components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


fromComponents: (Interval, Interval, Interval) -> Bounds3d
fromComponents (x, y, z) =
  Bounds3d x y z


components: Bounds3d -> (Interval, Interval, Interval)
components (Bounds3d x y z) =
  (x, y, z)


contains: Point3d -> Bounds3d -> Bool
contains (Point3d px py pz) (Bounds3d x y z) =
  Interval.contains px x && Interval.contains py y && Interval.contains pz z


overlaps: Bounds3d -> Bounds3d -> Bool
overlaps (Bounds3d x2 y2 z2) (Bounds3d x1 y1 z1) =
  Interval.overlaps x2 x1 && Interval.overlaps y2 y1 && Interval.overlaps z2 z1
