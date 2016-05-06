module OpenSolid.Core.BoundingBox3d
  ( empty
  , whole
  , singleton
  , hull
  , hullOf
  , intersection
  , fromComponents
  , components
  , isEmpty
  , midpoint
  , contains
  , overlaps
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


empty: BoundingBox3d
empty =
  BoundingBox3d Interval.empty Interval.empty Interval.empty


whole: BoundingBox3d
whole =
  BoundingBox3d Interval.whole Interval.whole Interval.whole


singleton: Point3d -> BoundingBox3d
singleton (Point3d x y z) =
  BoundingBox3d (Interval.singleton x) (Interval.singleton y) (Interval.singleton z)


hull: BoundingBox3d -> BoundingBox3d -> BoundingBox3d
hull (BoundingBox3d otherX otherY otherZ) (BoundingBox3d x y z) =
  BoundingBox3d (Interval.hull otherX x) (Interval.hull otherY y) (Interval.hull otherZ z)


hullOf: List BoundingBox3d -> BoundingBox3d
hullOf =
  List.foldl hull empty


intersection: BoundingBox3d -> BoundingBox3d -> BoundingBox3d
intersection (BoundingBox3d otherX otherY otherZ) (BoundingBox3d x y z) =
  let
    x' = Interval.intersection otherX x
    y' = Interval.intersection otherY y
    z' = Interval.intersection otherZ z
  in
    if Interval.isEmpty x' || Interval.isEmpty y' || Interval.isEmpty z' then
      empty
    else
      BoundingBox3d x' y' z'


fromComponents: (Interval, Interval, Interval) -> BoundingBox3d
fromComponents (x, y, z) =
  BoundingBox3d x y z


components: BoundingBox3d -> (Interval, Interval, Interval)
components (BoundingBox3d x y z) =
  (x, y, z)


isEmpty: BoundingBox3d -> Bool
isEmpty (BoundingBox3d x y z) =
  Interval.isEmpty x && Interval.isEmpty y && Interval.isEmpty z


midpoint: BoundingBox3d -> Point3d
midpoint (BoundingBox3d x y z) =
  Point3d (Interval.midpoint x) (Interval.midpoint y) (Interval.midpoint z)


contains: BoundingBox3d -> BoundingBox3d -> Bool
contains (BoundingBox3d otherX otherY otherZ) (BoundingBox3d x y z) =
  Interval.contains otherX x && Interval.contains otherY y && Interval.contains otherZ z


overlaps: BoundingBox3d -> BoundingBox3d -> Bool
overlaps (BoundingBox3d otherX otherY otherZ) (BoundingBox3d x y z) =
  Interval.overlaps otherX x && Interval.overlaps otherY y && Interval.overlaps otherZ z
