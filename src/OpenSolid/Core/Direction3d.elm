module OpenSolid.Core.Direction3d
  ( x
  , y
  , z
  , fromComponents
  , components
  , vector
  , normalDirection
  , normalBasis
  , rotatedAbout
  , mirroredAlong
  , relativeTo
  , placedIn
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


fromComponents: (Float, Float, Float) -> Direction3d
fromComponents =
  Vector3d.fromComponents >> Direction3d


components: Direction3d -> (Float, Float, Float)
components =
  vector >> Vector3d.components


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


rotatedAbout: Direction3d -> Float -> Direction3d -> Direction3d
rotatedAbout direction angle =
  let
    rotateVector = Vector3d.rotatedAbout direction angle
  in
    vector >> rotateVector >> Direction3d


mirroredAlong: Direction3d -> Direction3d -> Direction3d
mirroredAlong direction =
  let
    mirrorVector = Vector3d.mirroredAlong direction
  in
    vector >> mirrorVector >> Direction3d


relativeTo: Frame3d -> Direction3d -> Direction3d
relativeTo frame =
  let
    localizeVector = Vector3d.relativeTo frame
  in
    vector >> localizeVector >> Direction3d


placedIn: Frame3d -> Direction3d -> Direction3d
placedIn frame =
  let
    globalizeVector = Vector3d.placedIn frame
  in
    vector >> globalizeVector >> Direction3d


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
