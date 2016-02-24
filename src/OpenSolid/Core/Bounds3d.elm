module OpenSolid.Core.Bounds3d
  ( toTuple
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


toTuple: Bounds3d -> (Interval, Interval, Interval)
toTuple bounds =
  (bounds.x, bounds.y, bounds.z)


contains: Point3d -> Bounds3d -> Bool
contains point bounds =
  Interval.contains point.x bounds.x &&
    Interval.contains point.y bounds.y &&
      Interval.contains point.z bounds.z


overlaps: Bounds3d -> Bounds3d -> Bool
overlaps other bounds =
  Interval.overlaps other.x bounds.x &&
    Interval.overlaps other.y bounds.y &&
      Interval.overlaps other.z bounds.z
