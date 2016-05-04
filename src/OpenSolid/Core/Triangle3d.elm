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
fromVertices (p1, p2, p3) =
  Triangle3d p1 p2 p3


vertices: Triangle3d -> (Point3d, Point3d, Point3d)
vertices (Triangle3d p1 p2 p3) =
  (p1, p2, p3)


edges: Triangle3d -> (LineSegment3d, LineSegment3d, LineSegment3d)
edges (Triangle3d p1 p2 p3) =
  (LineSegment3d p3 p2, LineSegment3d p1 p3, LineSegment3d p2 p1)


mapReduce: (Point3d -> a) -> (a -> a -> a -> b) -> Triangle3d -> b
mapReduce map reduce (Triangle3d p1 p2 p3) =
  reduce (map p1) (map p2) (map p3)


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
area (Triangle3d p1 p2 p3) =
  0.5 * Vector3d.length (Vector3d.cross (Point3d.vectorTo p3 p1) (Point3d.vectorTo p2 p1))


centroid: Triangle3d -> Point3d
centroid (Triangle3d p1 p2 p3) =
  let
    firstVector = Point3d.vectorTo p2 p1
    secondVector = Point3d.vectorTo p3 p1
    displacement = Vector3d.times (1.0 / 3.0) (Vector3d.plus secondVector firstVector)
  in
    Point3d.plus displacement p1
