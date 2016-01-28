module OpenSolid.Core.Frame2d
  ( global
  , xAxis
  , yAxis
  , transformedBy
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Direction2d as Direction2d


global: Frame2d
global =
  Frame2d Point2d.origin Direction2d.x Direction2d.y


xAxis: Frame2d -> Axis2d
xAxis frame =
  Axis2d frame.originPoint frame.xDirection


yAxis: Frame2d -> Axis2d
yAxis frame =
  Axis2d frame.originPoint frame.yDirection


transformedBy: Transformation2d -> Frame2d -> Frame2d
transformedBy transformation frame =
  let
    originPoint = Point2d.transformedBy transformation frame.originPoint
    transformDirection = Direction2d.transformedBy transformation
    xDirection = transformDirection frame.xDirection
    yDirection = transformDirection frame.yDirection
  in
    Frame2d originPoint xDirection yDirection
