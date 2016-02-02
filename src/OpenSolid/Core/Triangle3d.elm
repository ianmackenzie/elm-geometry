module OpenSolid.Core.Triangle3d
  ( vertices
  , edges
  , mapReduce
  , scaledAbout
  , transformedBy
  , projectedOntoPlane
  , projectedIntoPlane
  , area
  , centroid
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Point3d as Point3d


vertices: Triangle3d -> (Point3d, Point3d, Point3d)
vertices triangle =
  (triangle.firstVertex, triangle.secondVertex, triangle.thirdVertex)


edges: Triangle3d -> (LineSegment3d, LineSegment3d, LineSegment3d)
edges triangle =
  ( LineSegment3d triangle.thirdVertex triangle.secondVertex
  , LineSegment3d triangle.firstVertex triangle.thirdVertex
  , LineSegment3d triangle.secondVertex triangle.firstVertex
  )


mapReduce: (Point3d -> a) -> (a -> a -> a -> b) -> Triangle3d -> b
mapReduce map reduce triangle =
  reduce (map triangle.firstVertex) (map triangle.secondVertex) (map triangle.thirdVertex)


scaledAbout: Point3d -> Float -> Triangle3d -> Triangle3d
scaledAbout point scale =
  mapReduce (Point3d.scaledAbout point scale) Triangle3d


transformedBy: Transformation3d -> Triangle3d -> Triangle3d
transformedBy transformation =
  mapReduce (Point3d.transformedBy transformation) Triangle3d


projectedOntoPlane: Plane3d -> Triangle3d -> Triangle3d
projectedOntoPlane plane =
  mapReduce (Point3d.projectedOntoPlane plane) Triangle3d


projectedIntoPlane: Plane3d -> Triangle3d -> Triangle2d
projectedIntoPlane plane =
  mapReduce (Point3d.projectedIntoPlane plane) Triangle2d


area: Triangle3d -> Float
area triangle =
  let
    firstVector = Point3d.minus triangle.firstVertex triangle.secondVertex
    secondVector = Point3d.minus triangle.firstVertex triangle.thirdVertex
  in
    0.5 * Vector3d.length (Vector3d.cross secondVector firstVector)


centroid: Triangle3d -> Point3d
centroid triangle =
  let
    firstVector = Point3d.minus triangle.firstVertex triangle.secondVertex
    secondVector = Point3d.minus triangle.firstVertex triangle.thirdVertex
    displacement = Vector3d.times (1.0 / 3.0) (Vector3d.plus secondVector firstVector)
  in
    Point3d.plus displacement triangle.firstVertex
