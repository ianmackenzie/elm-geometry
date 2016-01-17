module OpenSolid.Core.Axis2d
  ( x
  , y
  , pointAt
  , normalDirection
  , normalAxis
  , reversed
  , transformedBy
  ) where


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
normalDirection =
  .direction >> Direction2d.normalDirection


normalAxis: Axis2d -> Axis2d
normalAxis axis =
  Axis2d axis.originPoint (normalDirection axis)


reversed: Axis2d -> Axis2d
reversed axis =
  Axis2d axis.originPoint (Direction2d.reversed axis.direction)


transformedBy: Transformation2d -> Axis2d -> Axis2d
transformedBy transformation axis =
  let
    transformedOriginPoint = Point2d.transformedBy transformation axis.originPoint
    transformedDirection = Direction2d.transformedBy transformation axis.direction
  in
    Axis2d transformedOriginPoint transformedDirection
