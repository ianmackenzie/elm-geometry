module OpenSolid.Core.Triangle3d (fromVertices, vertices, edges, map, mapTo, scaleAbout, rotateAbout, translateBy, mirrorAbout, projectOnto, projectInto, area, centroid, boundingBox) where

import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Point3d as Point3d


fromVertices : ( Point3d, Point3d, Point3d ) -> Triangle3d
fromVertices ( p1, p2, p3 ) =
  Triangle3d p1 p2 p3


vertices : Triangle3d -> ( Point3d, Point3d, Point3d )
vertices (Triangle3d p1 p2 p3) =
  ( p1, p2, p3 )


edges : Triangle3d -> ( LineSegment3d, LineSegment3d, LineSegment3d )
edges (Triangle3d p1 p2 p3) =
  ( LineSegment3d p3 p2, LineSegment3d p1 p3, LineSegment3d p2 p1 )


map : (Point3d -> Point3d) -> Triangle3d -> Triangle3d
map =
  mapTo Triangle3d


mapTo : (a -> a -> a -> b) -> (Point3d -> a) -> Triangle3d -> b
mapTo constructor map (Triangle3d p1 p2 p3) =
  constructor (map p1) (map p2) (map p3)


scaleAbout : Point3d -> Float -> Triangle3d -> Triangle3d
scaleAbout centerPoint scale =
  let
    scalePoint =
      Point3d.scaleAbout centerPoint scale
  in
    map scalePoint


rotateAbout : Axis3d -> Float -> Triangle3d -> Triangle3d
rotateAbout axis angle =
  let
    rotatePoint =
      Point3d.rotateAbout axis angle
  in
    map rotatePoint


translateBy : Vector3d -> Triangle3d -> Triangle3d
translateBy vector =
  map (Point3d.plus vector)


mirrorAbout : Plane3d -> Triangle3d -> Triangle3d
mirrorAbout plane =
  let
    mirrorPoint =
      Point3d.mirrorAbout plane
  in
    map mirrorPoint


projectOnto : Plane3d -> Triangle3d -> Triangle3d
projectOnto plane =
  let
    projectPoint =
      Point3d.projectOnto plane
  in
    map projectPoint


projectInto : Plane3d -> Triangle3d -> Triangle2d
projectInto plane =
  let
    projectPoint =
      Point3d.projectInto plane
  in
    mapTo Triangle2d projectPoint


area : Triangle3d -> Float
area (Triangle3d p1 p2 p3) =
  0.5 * Vector3d.length (Vector3d.cross (Point3d.vectorTo p3 p1) (Point3d.vectorTo p2 p1))


centroid : Triangle3d -> Point3d
centroid (Triangle3d p1 p2 p3) =
  let
    firstVector =
      Point3d.vectorTo p2 p1

    secondVector =
      Point3d.vectorTo p3 p1

    displacement =
      Vector3d.times (1.0 / 3.0) (Vector3d.plus secondVector firstVector)
  in
    Point3d.plus displacement p1


boundingBox : Triangle3d -> BoundingBox3d
boundingBox (Triangle3d (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3)) =
  let
    xMin =
      min x1 (min x2 x3)

    xMax =
      max x1 (max x2 x3)

    yMin =
      min y1 (min y2 y3)

    yMax =
      max y1 (max y2 y3)

    zMin =
      min z1 (min z2 z3)

    zMax =
      max z1 (max z2 z3)
  in
    BoundingBox3d (Interval xMin xMax) (Interval yMin yMax) (Interval zMin zMax)
