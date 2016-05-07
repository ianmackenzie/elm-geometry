module OpenSolid.Core.Direction2d
  ( x
  , y
  , fromAngle
  , fromComponents
  , components
  , normalDirection
  , rotateBy
  , mirrorAbout
  , relativeTo
  , placeIn
  , placeOnto
  , negate
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


fromAngle: Float -> Direction2d
fromAngle angle =
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


rotateBy: Float -> Direction2d -> Direction2d
rotateBy angle =
  vector >> Vector2d.rotateBy angle >> Direction2d


mirrorAbout: Direction2d -> Direction2d -> Direction2d
mirrorAbout direction =
  vector >> Vector2d.mirrorAbout direction >> Direction2d


relativeTo: Frame2d -> Direction2d -> Direction2d
relativeTo frame =
  vector >> Vector2d.relativeTo frame >> Direction2d


placeIn: Frame2d -> Direction2d -> Direction2d
placeIn frame =
  vector >> Vector2d.placeIn frame >> Direction2d


placeOnto: Plane3d -> Direction2d -> Direction3d
placeOnto plane =
  vector >> Vector2d.placeOnto plane >> Direction3d


negate: Direction2d -> Direction2d
negate =
  vector >> Vector2d.negate >> Direction2d


times: Float -> Direction2d -> Vector2d
times scale =
  vector >> Vector2d.times scale
