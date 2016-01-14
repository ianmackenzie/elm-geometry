module OpenSolid.Core.Box3d
  ( components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (Interval, Point3d, Box3d)
import OpenSolid.Core.Interval as Interval


components: Box3d -> (Interval, Interval, Interval)
components box =
  (box.x, box.y, box.z)


contains: Point3d -> Box3d -> Bool
contains point box =
  Interval.contains point.x box.x &&
  Interval.contains point.y box.y &&
  Interval.contains point.z box.z


overlaps: Box3d -> Box3d -> Bool
overlaps other box =
  Interval.overlaps other.x box.x &&
  Interval.overlaps other.y box.y &&
  Interval.overlaps other.z box.z
