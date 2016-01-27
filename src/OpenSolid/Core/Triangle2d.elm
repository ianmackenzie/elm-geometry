module OpenSolid.Core.Triangle2d
  ( vertices
  , edges
  , mapReduce
  , scaledAbout
  , transformedBy
  , placedOntoPlane
  , area
  , centroid
  , contains
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d


vertices: Triangle2d -> (Point2d, Point2d, Point2d)
vertices (Triangle2d firstVertex secondVertex thirdVertex) =
  (firstVertex, secondVertex, thirdVertex)


edges: Triangle2d -> (LineSegment2d, LineSegment2d, LineSegment2d)
edges (Triangle2d firstVertex secondVertex thirdVertex) =
  ( LineSegment2d thirdVertex secondVertex
  , LineSegment2d firstVertex thirdVertex
  , LineSegment2d secondVertex firstVertex
  )


mapReduce: (Point2d -> a) -> (a -> a -> a -> b) -> Triangle2d -> b
mapReduce map reduce (Triangle2d firstVertex secondVertex thirdVertex) =
  reduce (map firstVertex) (map secondVertex) (map thirdVertex)


scaledAbout: Point2d -> Float -> Triangle2d -> Triangle2d
scaledAbout point scale =
  mapReduce (Point2d.scaledAbout point scale) Triangle2d


transformedBy: Transformation2d -> Triangle2d -> Triangle2d
transformedBy transformation =
  mapReduce (Point2d.transformedBy transformation) Triangle2d


placedOntoPlane: Plane3d -> Triangle2d -> Triangle3d
placedOntoPlane plane =
  mapReduce (Point2d.placedOntoPlane plane) Triangle3d


area: Triangle2d -> Float
area (Triangle2d firstVertex secondVertex thirdVertex) =
  let
    firstVector = Point2d.minus firstVertex secondVertex
    secondVector = Point2d.minus firstVertex thirdVertex
  in
    0.5 * Vector2d.cross secondVector firstVector


centroid: Triangle2d -> Point2d
centroid (Triangle2d firstVertex secondVertex thirdVertex) =
  let
    firstVector = Point2d.minus firstVertex secondVertex
    secondVector = Point2d.minus firstVertex thirdVertex
    displacement = Vector2d.times (1.0 / 3.0) (Vector2d.plus secondVector firstVector)
  in
    Point2d.plus displacement firstVertex


contains: Point2d -> Triangle2d -> Bool
contains point (Triangle2d firstVertex secondVertex thirdVertex) =
  let
    crossProduct startVertex endVertex =
      Vector2d.cross (Point2d.minus startVertex point) (Point2d.minus startVertex endVertex)

    firstProduct = crossProduct firstVertex secondVertex
    secondProduct = crossProduct secondVertex thirdVertex
    thirdProduct = crossProduct thirdVertex firstVertex
  in
    (firstProduct >= 0 && secondProduct >= 0 && thirdProduct >= 0) ||
    (firstProduct <= 0 && secondProduct <= 0 && thirdProduct <= 0)
