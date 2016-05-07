module OpenSolid.Core.LineSegment3d
  ( fromEndpoints
  , endpoints
  , map
  , mapTo
  , vector
  , direction
  , normalDirection
  , squaredLength
  , length
  , scaleAbout
  , rotateAbout
  , translateBy
  , mirrorAbout
  , projectOntoAxis
  , projectOnto
  , projectInto
  , boundingBox
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Point3d as Point3d


fromEndpoints: (Point3d, Point3d) -> LineSegment3d
fromEndpoints (p1, p2) =
  LineSegment3d p1 p2


endpoints: LineSegment3d -> (Point3d, Point3d)
endpoints (LineSegment3d p1 p2) =
  (p1, p2)


map: (Point3d -> Point3d) -> LineSegment3d -> LineSegment3d
map =
  mapTo LineSegment3d


mapTo: (a -> a -> b) -> (Point3d -> a) -> LineSegment3d -> b
mapTo constructor map (LineSegment3d p1 p2) =
  constructor (map p1) (map p2)


vector: LineSegment3d -> Vector3d
vector (LineSegment3d p1 p2) =
  Point3d.vectorTo p2 p1


direction: LineSegment3d -> Maybe Direction3d
direction =
  vector >> Vector3d.direction


normalDirection: LineSegment3d -> Maybe Direction3d
normalDirection =
  vector >> Vector3d.normalDirection


squaredLength: LineSegment3d -> Float
squaredLength =
  vector >> Vector3d.squaredLength


length: LineSegment3d -> Float
length =
  vector >> Vector3d.length


scaleAbout: Point3d -> Float -> LineSegment3d -> LineSegment3d
scaleAbout point scale =
  let
    scalePoint = Point3d.scaleAbout point scale
  in
    map scalePoint


rotateAbout: Axis3d -> Float -> LineSegment3d -> LineSegment3d
rotateAbout axis angle =
  let
    rotatePoint = Point3d.rotateAbout axis angle
  in
    map rotatePoint


translateBy: Vector3d -> LineSegment3d -> LineSegment3d
translateBy vector =
  map (Point3d.plus vector)


mirrorAbout: Plane3d -> LineSegment3d -> LineSegment3d
mirrorAbout plane =
  let
    mirrorPoint = Point3d.mirrorAbout plane
  in
    map mirrorPoint


projectOntoAxis: Axis3d -> LineSegment3d -> LineSegment3d
projectOntoAxis axis =
  let
    projectPoint = Point3d.projectOntoAxis axis
  in
    map projectPoint


projectOnto: Plane3d -> LineSegment3d -> LineSegment3d
projectOnto plane =
  let
    projectPoint = Point3d.projectOnto plane
  in
    map projectPoint


projectInto: Plane3d -> LineSegment3d -> LineSegment2d
projectInto plane =
  let
    projectPoint = Point3d.projectInto plane
  in
    mapTo LineSegment2d projectPoint


boundingBox: LineSegment3d -> BoundingBox3d
boundingBox (LineSegment3d p1 p2) =
  Point3d.hull p1 p2
