module OpenSolid.Core.Components2d
  ( toTuple
  , negated
  , plus
  , minus
  , times
  ) where


import OpenSolid.Core exposing (..)


type alias Components2d =
  { x: Float
  , y: Float
  }


toTuple: Components2d -> (Float, Float)
toTuple components =
  (components.x, components.y)


negated: Components2d -> Components2d
negated components =
  Components2d (-components.x) (-components.y)


plus: Components2d -> Components2d -> Components2d
plus second first =
  Components2d (first.x + second.x) (first.y + second.y)


minus: Components2d -> Components2d -> Components2d
minus second first =
  Components2d (first.x - second.x) (first.y - second.y)


times: Float -> Components2d -> Components2d
times scale components =
  Components2d (components.x * scale) (components.y * scale)
