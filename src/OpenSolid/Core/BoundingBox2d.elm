module OpenSolid.Core.BoundingBox2d
  ( fromComponents
  , components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


fromComponents: (Interval, Interval) -> BoundingBox2d
fromComponents (x, y) =
  BoundingBox2d x y


components: BoundingBox2d -> (Interval, Interval)
components (BoundingBox2d x y) =
  (x, y)


contains: BoundingBox2d -> BoundingBox2d -> Bool
contains (BoundingBox2d otherX otherY) (BoundingBox2d x y) =
  Interval.contains otherX x && Interval.contains otherY y


overlaps: BoundingBox2d -> BoundingBox2d -> Bool
overlaps (BoundingBox2d otherX otherY) (BoundingBox2d x y) =
  Interval.overlaps otherX x && Interval.overlaps otherY y
