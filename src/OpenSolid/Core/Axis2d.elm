module OpenSolid.Core.Axis2d
  ( x
  , y
  , pointAt
  , normalDirection
  , normalAxis
  , reversed
  , placedOntoPlane
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Direction2d as Direction2d


x: Axis2d
x =
  Axis2d Point2d.origin Direction2d.x


y: Axis2d
y =
  Axis2d Point2d.origin Direction2d.y


pointAt: Float -> Axis2d -> Point2d
pointAt distance axis =
  Point2d.plus (Direction2d.times distance axis.direction) axis.originPoint


normalDirection: Axis2d -> Direction2d
normalDirection axis =
  Direction2d.normalDirection axis.direction


normalAxis: Axis2d -> Axis2d
normalAxis axis =
  Axis2d axis.originPoint (normalDirection axis)


reversed: Axis2d -> Axis2d
reversed axis =
  Axis2d axis.originPoint (Direction2d.negated axis.direction)


placedOntoPlane: Plane3d -> Axis2d -> Axis3d
placedOntoPlane plane axis =
  let
    originPoint = Point2d.placedOntoPlane plane axis.originPoint
    direction = Direction2d.placedOntoPlane plane axis.direction
  in
    Axis3d originPoint direction
