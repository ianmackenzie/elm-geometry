module OpenSolid.Core.LineSegment3d
  ( endpoints
  , vector
  , direction
  , normalDirection
  , squaredLength
  , length
  , scaledAbout
  , transformedBy
  , projectedOntoPlane
  , projectedIntoPlane
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Point3d as Point3d


endpoints: LineSegment3d -> (Point3d, Point3d)
endpoints lineSegment =
  (lineSegment.firstEndpoint, lineSegment.secondEndpoint)


vector: LineSegment3d -> Vector3d
vector lineSegment =
  Point3d.minus lineSegment.firstEndpoint lineSegment.secondEndpoint


direction: LineSegment3d -> Direction3d
direction =
  vector >> Vector3d.direction


normalDirection: LineSegment3d -> Direction3d
normalDirection =
  vector >> Vector3d.normalDirection


squaredLength: LineSegment3d -> Float
squaredLength =
  vector >> Vector3d.squaredLength


length: LineSegment3d -> Float
length =
  vector >> Vector3d.length


scaledAbout: Point3d -> Float -> LineSegment3d -> LineSegment3d
scaledAbout point scale lineSegment =
  let
    scalePoint = Point3d.scaledAbout point scale
    firstEndpoint = scalePoint lineSegment.firstEndpoint
    secondEndpoint = scalePoint lineSegment.secondEndpoint
  in
    LineSegment3d firstEndpoint secondEndpoint


transformedBy: Transformation3d -> LineSegment3d -> LineSegment3d
transformedBy transformation lineSegment =
  let
    transformPoint = Point3d.transformedBy transformation
    firstEndpoint = transformPoint lineSegment.firstEndpoint
    secondEndpoint = transformPoint lineSegment.secondEndpoint
  in
    LineSegment3d firstEndpoint secondEndpoint


projectedOntoPlane: Plane3d -> LineSegment3d -> LineSegment3d
projectedOntoPlane plane lineSegment =
  let
    projectPoint = Point3d.projectedOntoPlane plane
    firstEndpoint = projectPoint lineSegment.firstEndpoint
    secondEndpoint = projectPoint lineSegment.secondEndpoint
  in
    LineSegment3d firstEndpoint secondEndpoint


projectedIntoPlane: Plane3d -> LineSegment3d -> LineSegment2d
projectedIntoPlane plane lineSegment =
  let
    projectPoint = Point3d.projectedIntoPlane plane
    firstEndpoint = projectPoint lineSegment.firstEndpoint
    secondEndpoint = projectPoint lineSegment.secondEndpoint
  in
    LineSegment2d firstEndpoint secondEndpoint
