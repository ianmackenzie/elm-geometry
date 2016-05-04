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


contains: Point2d -> BoundingBox2d -> Bool
contains (Point2d px py) (BoundingBox2d x y) =
  Interval.contains px x && Interval.contains py y


overlaps: BoundingBox2d -> BoundingBox2d -> Bool
overlaps (BoundingBox2d x2 y2) (BoundingBox2d x1 y1) =
  Interval.overlaps x2 x1 && Interval.overlaps y2 y1
