module OpenSolid.Box2d
  ( Box2d
  , components
  , contains
  , overlaps
  ) where


import OpenSolid.Interval as Interval exposing (Interval)
import OpenSolid.Point2d as Point2d exposing (Point2d)


type alias Box2d =
  { x: Interval
  , y: Interval
  }


components: Box2d -> (Interval, Interval)
components box =
  (box.x, box.y)


contains: Point2d -> Box2d -> Bool
contains point box =
  Interval.contains point.x box.x && Interval.contains point.y box.y


overlaps: Box2d -> Box2d -> Bool
overlaps other box =
  Interval.overlaps other.x box.x && Interval.overlaps other.y box.y
