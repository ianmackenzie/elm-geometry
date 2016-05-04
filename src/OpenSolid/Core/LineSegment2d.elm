module OpenSolid.Core.LineSegment2d
  ( fromEndpoints
  , endpoints
  , map
  , mapTo
  , vector
  , direction
  , normalDirection
  , squaredLength
  , length
  , scaledAbout
  , rotatedAbout
  , translatedBy
  , mirroredAbout
  , projectedOnto
  , placedOnto
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d


fromEndpoints: (Point2d, Point2d) -> LineSegment2d
fromEndpoints (p1, p2) =
  LineSegment2d p1 p2


endpoints: LineSegment2d -> (Point2d, Point2d)
endpoints (LineSegment2d p1 p2) =
  (p1, p2)


map: (Point2d -> Point2d) -> LineSegment2d -> LineSegment2d
map =
  mapTo LineSegment2d


mapTo: (a -> a -> b) -> (Point2d -> a) -> LineSegment2d -> b
mapTo constructor map (LineSegment2d p1 p2) =
  constructor (map p1) (map p2)


vector: LineSegment2d -> Vector2d
vector (LineSegment2d p1 p2) =
  Point2d.vectorTo p2 p1


direction: LineSegment2d -> Maybe Direction2d
direction =
  vector >> Vector2d.direction


normalDirection: LineSegment2d -> Maybe Direction2d
normalDirection =
  vector >> Vector2d.normalDirection


squaredLength: LineSegment2d -> Float
squaredLength =
  vector >> Vector2d.squaredLength


length: LineSegment2d -> Float
length =
  vector >> Vector2d.length


scaledAbout: Point2d -> Float -> LineSegment2d -> LineSegment2d
scaledAbout point scale =
  let
    scalePoint = Point2d.scaledAbout point scale
  in
    map scalePoint


rotatedAbout: Point2d -> Float -> LineSegment2d -> LineSegment2d
rotatedAbout centerPoint angle =
  let
    rotatePoint = Point2d.rotatedAbout centerPoint angle
  in
    map rotatePoint


translatedBy: Vector2d -> LineSegment2d -> LineSegment2d
translatedBy vector =
  map (Point2d.plus vector)


mirroredAbout: Axis2d -> LineSegment2d -> LineSegment2d
mirroredAbout axis =
  let
    mirrorPoint = Point2d.mirroredAbout axis
  in
    map mirrorPoint


projectedOnto: Axis2d -> LineSegment2d -> LineSegment2d
projectedOnto axis =
  let
    projectPoint = Point2d.projectedOnto axis
  in
    map projectPoint


placedOnto: Plane3d -> LineSegment2d -> LineSegment3d
placedOnto plane =
  let
    placePoint = Point2d.placedOnto plane
  in
    mapTo LineSegment3d placePoint
