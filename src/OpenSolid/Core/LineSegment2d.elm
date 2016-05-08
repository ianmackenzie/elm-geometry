module OpenSolid.Core.LineSegment2d (fromEndpoints, endpoints, map, mapTo, vector, direction, normalDirection, squaredLength, length, scaleAbout, rotateAbout, translateBy, mirrorAbout, projectOnto, placeOnto, boundingBox) where

import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d


fromEndpoints : ( Point2d, Point2d ) -> LineSegment2d
fromEndpoints ( p1, p2 ) =
  LineSegment2d p1 p2


endpoints : LineSegment2d -> ( Point2d, Point2d )
endpoints (LineSegment2d p1 p2) =
  ( p1, p2 )


map : (Point2d -> Point2d) -> LineSegment2d -> LineSegment2d
map =
  mapTo LineSegment2d


mapTo : (a -> a -> b) -> (Point2d -> a) -> LineSegment2d -> b
mapTo constructor map (LineSegment2d p1 p2) =
  constructor (map p1) (map p2)


vector : LineSegment2d -> Vector2d
vector (LineSegment2d p1 p2) =
  Point2d.vectorTo p2 p1


direction : LineSegment2d -> Maybe Direction2d
direction =
  vector >> Vector2d.direction


normalDirection : LineSegment2d -> Maybe Direction2d
normalDirection =
  vector >> Vector2d.normalDirection


squaredLength : LineSegment2d -> Float
squaredLength =
  vector >> Vector2d.squaredLength


length : LineSegment2d -> Float
length =
  vector >> Vector2d.length


scaleAbout : Point2d -> Float -> LineSegment2d -> LineSegment2d
scaleAbout point scale =
  let
    scalePoint =
      Point2d.scaleAbout point scale
  in
    map scalePoint


rotateAbout : Point2d -> Float -> LineSegment2d -> LineSegment2d
rotateAbout centerPoint angle =
  let
    rotatePoint =
      Point2d.rotateAbout centerPoint angle
  in
    map rotatePoint


translateBy : Vector2d -> LineSegment2d -> LineSegment2d
translateBy vector =
  map (Point2d.plus vector)


mirrorAbout : Axis2d -> LineSegment2d -> LineSegment2d
mirrorAbout axis =
  let
    mirrorPoint =
      Point2d.mirrorAbout axis
  in
    map mirrorPoint


projectOnto : Axis2d -> LineSegment2d -> LineSegment2d
projectOnto axis =
  let
    projectPoint =
      Point2d.projectOnto axis
  in
    map projectPoint


placeOnto : Plane3d -> LineSegment2d -> LineSegment3d
placeOnto plane =
  let
    placePoint =
      Point2d.placeOnto plane
  in
    mapTo LineSegment3d placePoint


boundingBox : LineSegment2d -> BoundingBox2d
boundingBox (LineSegment2d p1 p2) =
  Point2d.hull p1 p2
