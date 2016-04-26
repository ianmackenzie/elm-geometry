module OpenSolid.Core.Triangle3d
  ( fromVertices
  , vertices
  , edges
  , mapReduce
  , scaledAbout
  , projectedOnto
  , projectedInto
  , area
  , centroid
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Point3d as Point3d


fromVertices: (Point3d, Point3d, Point3d) -> Triangle3d
fromVertices (first, second, third) =
  Triangle3d first second third


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


projectedOnto: Plane3d -> Triangle3d -> Triangle3d
projectedOnto plane =
  mapReduce (Point3d.projectedOnto plane) Triangle3d


projectedInto: Plane3d -> Triangle3d -> Triangle2d
projectedInto plane =
  mapReduce (Point3d.projectedInto plane) Triangle2d


area: Triangle3d -> Float
area triangle =
  let
    firstVector = Point3d.vectorTo triangle.secondVertex triangle.firstVertex
    secondVector = Point3d.vectorTo triangle.thirdVertex triangle.firstVertex
  in
    0.5 * Vector3d.length (Vector3d.cross secondVector firstVector)


centroid: Triangle3d -> Point3d
centroid triangle =
  let
    firstVector = Point3d.vectorTo triangle.secondVertex triangle.firstVertex
    secondVector = Point3d.vectorTo triangle.thirdVertex triangle.firstVertex
    displacement = Vector3d.times (1.0 / 3.0) (Vector3d.plus secondVector firstVector)
  in
    Point3d.plus displacement triangle.firstVertex
