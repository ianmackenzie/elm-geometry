module OpenSolid.Core.Vector2d
  ( zero
  , fromTuple
  , toTuple
  , equals
  , componentIn
  , squaredLength
  , length
  , normalized
  , direction
  , perpendicularVector
  , normalDirection
  , rotatedBy
  , relativeTo
  , placedIn
  , mirroredAbout
  , projectedOnto
  , projectedOntoAxis
  , placedOntoPlane
  , negated
  , plus
  , minus
  , times
  , addedTo
  , subtractedFrom
  , dot
  , cross
  ) where


import Debug
import Maybe exposing (..)
import OpenSolid.Core exposing (..)
import OpenSolid.Core.Matrix2x2 as Matrix2x2
import OpenSolid.Core.Matrix3x2 as Matrix3x2


toVector: Direction2d -> Vector2d
toVector (Direction2d vector) =
  vector


zero: Vector2d
zero =
  Vector2d 0 0


fromTuple: (Float, Float) -> Vector2d
fromTuple (x, y) =
  Vector2d x y


toTuple: Vector2d -> (Float, Float)
toTuple (Vector2d x y) =
  (x, y)


equals: Vector2d -> Vector2d -> Bool
equals (Vector2d x2 y2) (Vector2d x1 y1) =
  x1 == x2 && y1 == y2


componentIn: Direction2d -> Vector2d -> Float
componentIn (Direction2d vector) =
  dot vector


squaredLength: Vector2d -> Float
squaredLength (Vector2d x y) =
  x * x + y * y


length: Vector2d -> Float
length =
  squaredLength >> sqrt


normalized: Vector2d -> Maybe Vector2d
normalized vector =
  if equals zero vector then Nothing else Just (times (1 / length vector) vector)


direction: Vector2d -> Maybe Direction2d
direction =
  normalized >> Maybe.map Direction2d


perpendicularVector: Vector2d -> Vector2d
perpendicularVector (Vector2d x y) =
  Vector2d (-y) x


normalDirection: Vector2d -> Maybe Direction2d
normalDirection =
  perpendicularVector >> direction


rotatedBy: Float -> Vector2d -> Vector2d
rotatedBy angle =
  let
    cosine = Debug.log "cosine" (cos angle)
    sine = Debug.log "sine" (sin angle)
  in
    \(Vector2d x y) -> Vector2d (x * cosine - y * sine) (y * cosine + x * sine)


relativeTo: Frame2d -> Vector2d -> Vector2d
relativeTo frame =
  Matrix2x2.dotProduct frame.xDirection frame.yDirection


placedIn: Frame2d -> Vector2d -> Vector2d
placedIn frame =
  Matrix2x2.product frame.xDirection frame.yDirection


mirroredAbout: Axis2d -> Vector2d -> Vector2d
mirroredAbout axis =
  let
    (Direction2d (Vector2d dx dy)) = axis.direction
    a = 1 - 2 * dy * dy
    b = 2 * dx * dy
    c = 1 - 2 * dx * dx
  in
    \(Vector2d vx vy) -> Vector2d (a * vx + b * vy) (c * vy + b * vx)


projectedOnto: Direction2d -> Vector2d -> Vector2d
projectedOnto direction vector =
  times (componentIn direction vector) (toVector2d direction)


projectedOntoAxis: Axis2d -> Vector2d -> Vector2d
projectedOntoAxis axis =
  projectedOnto axis.direction


placedOntoPlane: Plane3d -> Vector2d -> Vector3d
placedOntoPlane plane =
  Matrix3x2.product plane.xDirection plane.yDirection


negated: Vector2d -> Vector2d
negated (Vector2d x y) =
  Vector2d (-x) (-y)


plus: Vector2d -> Vector2d -> Vector2d
plus (Vector2d x2 y2) (Vector2d x1 y1) =
  Vector2d (x1 + x2) (y1 + y2)


minus: Vector2d -> Vector2d -> Vector2d
minus (Vector2d x2 y2) (Vector2d x1 y1) =
  Vector2d (x1 - x2) (y1 - y2)


times: Float -> Vector2d -> Vector2d
times scale (Vector2d x y) =
  Vector2d (x * scale) (y * scale)


addedTo: Point2d -> Vector2d -> Point2d
addedTo (Point2d px py) (Vector2d vx vy) =
  Point2d (px + vx) (py + vy)


subtractedFrom: Point2d -> Vector2d -> Point2d
subtractedFrom (Point2d px py) (Vector2d vx vy) =
  Point2d (px - vx) (py - vy)


dot: Vector2d -> Vector2d -> Float
dot (Vector2d x2 y2) (Vector2d x1 y1) =
  x1 * x2 + y1 * y2


cross: Vector2d -> Vector2d -> Float
cross (Vector2d x2 y2) (Vector2d x1 y1) =
  x1 * y2 - y1 * x2
