module OpenSolid.Core.Direction2d
  ( x
  , y
  , polar
  , fromComponents
  , components
  , normalDirection
  , rotatedBy
  , mirroredAbout
  , relativeTo
  , placedIn
  , placedOnto
  , negated
  , times
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d


x: Direction2d
x =
  Direction2d (Vector2d 1 0)


y: Direction2d
y =
  Direction2d (Vector2d 0 1)


polar: Float -> Direction2d
polar angle =
  Direction2d (Vector2d (cos angle) (sin angle))


fromComponents: (Float, Float) -> Direction2d
fromComponents =
  Vector2d.fromComponents >> Direction2d


components: Direction2d -> (Float, Float)
components =
  vector >> Vector2d.components


vector: Direction2d -> Vector2d
vector (Direction2d vector') =
  vector'


normalDirection: Direction2d -> Direction2d
normalDirection =
  vector >> Vector2d.perpendicularVector >> Direction2d


rotatedBy: Float -> Direction2d -> Direction2d
rotatedBy angle =
  vector >> Vector2d.rotatedBy angle >> Direction2d


mirroredAbout: Direction2d -> Direction2d -> Direction2d
mirroredAbout direction =
  vector >> Vector2d.mirroredAbout direction >> Direction2d


relativeTo: Frame2d -> Direction2d -> Direction2d
relativeTo frame =
  vector >> Vector2d.relativeTo frame >> Direction2d


placedIn: Frame2d -> Direction2d -> Direction2d
placedIn frame =
  vector >> Vector2d.placedIn frame >> Direction2d


placedOnto: Plane3d -> Direction2d -> Direction3d
placedOnto plane =
  vector >> Vector2d.placedOnto plane >> Direction3d


negated: Direction2d -> Direction2d
negated =
  vector >> Vector2d.negated >> Direction2d


times: Float -> Direction2d -> Vector2d
times scale =
  vector >> Vector2d.times scale
