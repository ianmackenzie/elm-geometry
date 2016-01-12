module OpenSolid.Svg
  ( svg
  , lineSegment
  ) where


import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes
import OpenSolid.Interval as Interval exposing (Interval)
import OpenSolid.Box2d as Box2d exposing (Box2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)


svg: Float -> Float -> Box2d -> List Svg -> Svg
svg width height box elements =
  let
    widthAttribute = Attributes.width (toString width)
    heightAttribute = Attributes.height (toString height)
    viewX = toString box.x.lowerBound
    viewY = toString (-box.y.upperBound)
    viewWidth = toString (Interval.width box.x)
    viewHeight = toString (Interval.width box.y)
    viewBoxAttribute =
      Attributes.viewBox (viewX ++ " " ++ viewY ++ " " ++ viewWidth ++ " " ++ viewHeight)
  in
    Svg.svg [widthAttribute, heightAttribute, viewBoxAttribute] elements


lineSegment: List Attribute -> LineSegment2d -> Svg
lineSegment attributes lineSegment =
  let
    {firstEndpoint, secondEndpoint} = lineSegment
    x1 = toString firstEndpoint.x
    y1 = toString (-firstEndpoint.y)
    x2 = toString secondEndpoint.x
    y2 = toString (-secondEndpoint.y)
    d = "M " ++ x1 ++ " " ++ y1 ++ " L " ++ x2 ++ " " ++ y2
  in
    Svg.path ([Attributes.d d] ++ attributes) []
