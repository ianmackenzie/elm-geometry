module OpenSolid.Core.Bounds2d
  ( components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


components: Bounds2d -> (Interval, Interval)
components bounds =
  (bounds.x, bounds.y)


contains: Point2d -> Bounds2d -> Bool
contains point bounds =
  Interval.contains point.x bounds.x && Interval.contains point.y bounds.y


overlaps: Bounds2d -> Bounds2d -> Bool
overlaps other bounds =
  Interval.overlaps other.x bounds.x && Interval.overlaps other.y bounds.y
