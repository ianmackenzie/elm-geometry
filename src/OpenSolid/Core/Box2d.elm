module OpenSolid.Core.Box2d
  ( components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (Interval, Point2d, Box2d)
import OpenSolid.Core.Interval as Interval


components: Box2d -> (Interval, Interval)
components box =
  (box.x, box.y)


contains: Point2d -> Box2d -> Bool
contains point box =
  Interval.contains point.x box.x && Interval.contains point.y box.y


overlaps: Box2d -> Box2d -> Bool
overlaps other box =
  Interval.overlaps other.x box.x && Interval.overlaps other.y box.y
