module OpenSolid.Core.BoundingBox2d (empty, whole, singleton, hull, hullOf, intersection, fromComponents, components, isEmpty, midpoint, contains, overlaps) where

import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval


empty : BoundingBox2d
empty =
  BoundingBox2d Interval.empty Interval.empty


whole : BoundingBox2d
whole =
  BoundingBox2d Interval.whole Interval.whole


singleton : Point2d -> BoundingBox2d
singleton (Point2d x y) =
  BoundingBox2d (Interval.singleton x) (Interval.singleton y)


hull : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
hull (BoundingBox2d otherX otherY) (BoundingBox2d x y) =
  BoundingBox2d (Interval.hull otherX x) (Interval.hull otherY y)


hullOf : List BoundingBox2d -> BoundingBox2d
hullOf =
  List.foldl hull empty


intersection : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
intersection (BoundingBox2d otherX otherY) (BoundingBox2d x y) =
  let
    x' =
      Interval.intersection otherX x

    y' =
      Interval.intersection otherY y
  in
    if Interval.isEmpty x' || Interval.isEmpty y' then
      empty
    else
      BoundingBox2d x' y'


fromComponents : ( Interval, Interval ) -> BoundingBox2d
fromComponents ( x, y ) =
  BoundingBox2d x y


components : BoundingBox2d -> ( Interval, Interval )
components (BoundingBox2d x y) =
  ( x, y )


isEmpty : BoundingBox2d -> Bool
isEmpty (BoundingBox2d x y) =
  Interval.isEmpty x && Interval.isEmpty y


midpoint : BoundingBox2d -> Point2d
midpoint (BoundingBox2d x y) =
  Point2d (Interval.midpoint x) (Interval.midpoint y)


contains : BoundingBox2d -> BoundingBox2d -> Bool
contains (BoundingBox2d otherX otherY) (BoundingBox2d x y) =
  Interval.contains otherX x && Interval.contains otherY y


overlaps : BoundingBox2d -> BoundingBox2d -> Bool
overlaps (BoundingBox2d otherX otherY) (BoundingBox2d x y) =
  Interval.overlaps otherX x && Interval.overlaps otherY y
