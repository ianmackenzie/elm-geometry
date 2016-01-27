module OpenSolid.Core.Box2d
  ( components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


components: Box2d -> (Interval, Interval)
components (Box2d x y) =
  (x, y)


contains: Point2d -> Box2d -> Bool
contains (Point2d pointX pointY) (Box2d x y) =
  Interval.contains pointX x && Interval.contains pointY y


overlaps: Box2d -> Box2d -> Bool
overlaps (Box2d otherX otherY) (Box2d x y) =
  Interval.overlaps otherX x && Interval.overlaps otherY y
