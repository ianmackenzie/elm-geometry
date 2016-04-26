module OpenSolid.Core.Direction3d
  ( x
  , y
  , z
  , fromTuple
  , toTuple
  , vector
  , normalDirection
  , normalBasis
  , projectedOnto
  , projectedInto
  , negated
  , times
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d


x: Direction3d
x =
  Direction3d (Vector3d 1 0 0)


y: Direction3d
y =
  Direction3d (Vector3d 0 1 0)


z: Direction3d
z =
  Direction3d (Vector3d 0 0 1)


fromTuple: (Float, Float, Float) -> Direction3d
fromTuple =
  Vector3d.fromTuple >> Direction3d


toTuple: Direction3d -> (Float, Float, Float)
toTuple =
  vector >> Vector3d.toTuple


vector: Direction3d -> Vector3d
vector (Direction3d vector') =
  vector'


normalDirection: Direction3d -> Direction3d
normalDirection direction =
  let
    perpendicularVector = Vector3d.perpendicularVector (vector direction)
  in
    Direction3d (Vector3d.times (1 / Vector3d.length perpendicularVector) perpendicularVector)


normalBasis: Direction3d -> (Direction3d, Direction3d)
normalBasis direction =
  let
    xDirection = normalDirection direction
    yDirection = Direction3d (Vector3d.cross (vector xDirection) (vector direction))
  in
    (xDirection, yDirection)


projectedOnto: Plane3d -> Direction3d -> Maybe Direction3d
projectedOnto plane =
  vector >> Vector3d.projectedOnto plane >> Vector3d.direction


projectedInto: Plane3d -> Direction3d -> Maybe Direction2d
projectedInto plane =
  vector >> Vector3d.projectedInto plane >> Vector2d.direction


negated: Direction3d -> Direction3d
negated =
  vector >> Vector3d.negated >> Direction3d


times: Float -> Direction3d -> Vector3d
times scale =
  vector >> Vector3d.times scale
