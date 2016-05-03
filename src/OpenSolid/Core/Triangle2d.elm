module OpenSolid.Core.Triangle2d
  ( fromVertices
  , vertices
  , edges
  , map
  , mapTo
  , scaledAbout
  , rotatedAbout
  , translatedBy
  , mirroredAbout
  , placedOnto
  , area
  , centroid
  , contains
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d


fromVertices: (Point2d, Point2d, Point2d) -> Triangle2d
fromVertices (firstVertex, secondVertex, thirdVertex) =
  Triangle2d firstVertex secondVertex thirdVertex


vertices: Triangle2d -> (Point2d, Point2d, Point2d)
vertices triangle =
  (triangle.firstVertex, triangle.secondVertex, triangle.thirdVertex)


edges: Triangle2d -> (LineSegment2d, LineSegment2d, LineSegment2d)
edges triangle =
  ( LineSegment2d triangle.thirdVertex triangle.secondVertex
  , LineSegment2d triangle.firstVertex triangle.thirdVertex
  , LineSegment2d triangle.secondVertex triangle.firstVertex
  )


map: (Point2d -> Point2d) -> Triangle2d -> Triangle2d
map =
  mapTo Triangle2d


mapTo: (a -> a -> a -> b) -> (Point2d -> a) -> Triangle2d -> b
mapTo constructor map triangle =
  constructor (map triangle.firstVertex) (map triangle.secondVertex) (map triangle.thirdVertex)


scaledAbout: Point2d -> Float -> Triangle2d -> Triangle2d
scaledAbout point scale =
  map (Point2d.scaledAbout point scale)


rotatedAbout: Point2d -> Float -> Triangle2d -> Triangle2d
rotatedAbout centerPoint angle =
  let
    rotatePoint = Point2d.rotatedAbout centerPoint angle
  in
    map rotatePoint


translatedBy: Vector2d -> Triangle2d -> Triangle2d
translatedBy vector =
  map (Point2d.plus vector)


mirroredAbout: Axis2d -> Triangle2d -> Triangle2d
mirroredAbout axis =
  let
    mirrorPoint = Point2d.mirroredAbout axis
  in
    map mirrorPoint


placedOnto: Plane3d -> Triangle2d -> Triangle3d
placedOnto plane =
  mapTo Triangle3d (Point2d.placedOnto plane)


area: Triangle2d -> Float
area triangle =
  let
    firstVector = Point2d.vectorTo triangle.secondVertex triangle.firstVertex
    secondVector = Point2d.vectorTo triangle.thirdVertex triangle.firstVertex
  in
    0.5 * Vector2d.cross secondVector firstVector


centroid: Triangle2d -> Point2d
centroid triangle =
  let
    firstVector = Point2d.vectorTo triangle.secondVertex triangle.firstVertex
    secondVector = Point2d.vectorTo triangle.thirdVertex triangle.firstVertex
    displacement = Vector2d.times (1.0 / 3.0) (Vector2d.plus secondVector firstVector)
  in
    Point2d.plus displacement triangle.firstVertex


contains: Point2d -> Triangle2d -> Bool
contains point triangle =
  let
    crossProduct startVertex endVertex =
      Vector2d.cross (Point2d.vectorTo point startVertex) (Point2d.vectorTo endVertex startVertex)

    firstProduct = crossProduct triangle.firstVertex triangle.secondVertex
    secondProduct = crossProduct triangle.secondVertex triangle.thirdVertex
    thirdProduct = crossProduct triangle.thirdVertex triangle.firstVertex
  in
    (firstProduct >= 0 && secondProduct >= 0 && thirdProduct >= 0) ||
    (firstProduct <= 0 && secondProduct <= 0 && thirdProduct <= 0)
