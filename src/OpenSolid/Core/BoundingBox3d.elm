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


contains: BoundingBox3d -> BoundingBox3d -> Bool
contains (BoundingBox3d otherX otherY otherZ) (BoundingBox3d x y z) =
  Interval.contains otherX x && Interval.contains otherY y && Interval.contains otherZ z


overlaps: BoundingBox3d -> BoundingBox3d -> Bool
overlaps (BoundingBox3d otherX otherY otherZ) (BoundingBox3d x y z) =
  Interval.overlaps otherX x && Interval.overlaps otherY y && Interval.overlaps otherZ z
