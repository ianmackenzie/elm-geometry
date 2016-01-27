module OpenSolid.Core.Box3d
  ( components
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


components: Box3d -> (Interval, Interval, Interval)
components (Box3d x y z) =
  (x, y, z)


contains: Point3d -> Box3d -> Bool
contains (Point3d pointX pointY pointZ) (Box3d x y z) =
  Interval.contains pointX x && Interval.contains pointY y && Interval.contains pointZ z


overlaps: Box3d -> Box3d -> Bool
overlaps (Box3d otherX otherY otherZ) (Box3d x y z) =
  Interval.overlaps otherX x && Interval.overlaps otherY y && Interval.overlaps otherZ z
