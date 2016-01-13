module OpenSolid.Svg
  ( svg
  , lineSegment
  ) where


import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes
import String
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
    x1 = firstEndpoint.x
    y1 = -firstEndpoint.y
    x2 = secondEndpoint.x
    y2 = -secondEndpoint.y
    d = String.join " " ["M", toString x1, toString y1, "L", toString x2, toString y2]
  in
    Svg.path ([Attributes.d d] ++ attributes) []
