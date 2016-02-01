module OpenSolid.Core.Bounds2d
  ( components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


components: Bounds2d -> (Interval, Interval)
components (Bounds2d x y) =
  (x, y)


contains: Point2d -> Bounds2d -> Bool
contains (Point2d pointX pointY) (Bounds2d x y) =
  Interval.contains pointX x && Interval.contains pointY y


overlaps: Bounds2d -> Bounds2d -> Bool
overlaps (Bounds2d otherX otherY) (Bounds2d x y) =
  Interval.overlaps otherX x && Interval.overlaps otherY y
