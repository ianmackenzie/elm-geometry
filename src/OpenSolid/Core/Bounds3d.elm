module OpenSolid.Core.Bounds3d
  ( components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


components: Bounds3d -> (Interval, Interval, Interval)
components (Bounds3d x y z) =
  (x, y, z)


contains: Point3d -> Bounds3d -> Bool
contains (Point3d pointX pointY pointZ) (Bounds3d x y z) =
  Interval.contains pointX x && Interval.contains pointY y && Interval.contains pointZ z


overlaps: Bounds3d -> Bounds3d -> Bool
overlaps (Bounds3d otherX otherY otherZ) (Bounds3d x y z) =
  Interval.overlaps otherX x && Interval.overlaps otherY y && Interval.overlaps otherZ z
