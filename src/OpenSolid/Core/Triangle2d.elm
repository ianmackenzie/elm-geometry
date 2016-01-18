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
vertices triangle =
  (triangle.firstVertex, triangle.secondVertex, triangle.thirdVertex)


edges: Triangle2d -> (LineSegment2d, LineSegment2d, LineSegment2d)
edges triangle =
  ( LineSegment2d triangle.thirdVertex triangle.secondVertex
  , LineSegment2d triangle.firstVertex triangle.thirdVertex
  , LineSegment2d triangle.secondVertex triangle.firstVertex
  )


mapReduce: (Point2d -> a) -> (a -> a -> a -> b) -> Triangle2d -> b
mapReduce map reduce triangle =
  reduce (map triangle.firstVertex) (map triangle.secondVertex) (map triangle.thirdVertex)


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
area triangle =
  let
    firstVector = Point2d.minus triangle.firstVertex triangle.secondVertex
    secondVector = Point2d.minus triangle.firstVertex triangle.thirdVertex
  in
    0.5 * Vector2d.cross secondVector firstVector


centroid: Triangle2d -> Point2d
centroid triangle =
  let
    firstVector = Point2d.minus triangle.firstVertex triangle.secondVertex
    secondVector = Point2d.minus triangle.firstVertex triangle.thirdVertex
    displacement = Vector2d.times (1.0 / 3.0) (Vector2d.plus secondVector firstVector)
  in
    Point2d.plus displacement triangle.firstVertex


contains: Point2d -> Triangle2d -> Bool
contains point triangle =
  let
    crossProduct startVertex endVertex =
      Vector2d.cross (Point2d.minus startVertex point) (Point2d.minus startVertex endVertex)

    firstProduct = crossProduct triangle.firstVertex triangle.secondVertex
    secondProduct = crossProduct triangle.secondVertex triangle.thirdVertex
    thirdProduct = crossProduct triangle.thirdVertex triangle.firstVertex
  in
    (firstProduct >= 0 && secondProduct >= 0 && thirdProduct >= 0) ||
    (firstProduct <= 0 && secondProduct <= 0 && thirdProduct <= 0)
