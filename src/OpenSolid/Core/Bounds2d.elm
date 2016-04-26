module OpenSolid.Core.Bounds2d
  ( fromComponents
  , components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


fromComponents: (Interval, Interval) -> Bounds2d
fromComponents (x, y) =
  Bounds2d x y


components: Bounds2d -> (Interval, Interval)
components (Bounds2d x y) =
  (x, y)


contains: Point2d -> Bounds2d -> Bool
contains (Point2d px py) (Bounds2d x y) =
  Interval.contains px x && Interval.contains py y


overlaps: Bounds2d -> Bounds2d -> Bool
overlaps (Bounds2d x2 y2) (Bounds2d x1 y1) =
  Interval.overlaps x2 x1 && Interval.overlaps y2 y1
