module OpenSolid.Core.Components3d
  ( toTuple
  , negated
  , plus
  , minus
  , times
  ) where


import OpenSolid.Core exposing (..)


toTuple: Components3d -> (Float, Float, Float)
toTuple components =
  (components.x, components.y, components.z)


negated: Components3d -> Components3d
negated components =
  Components3d (-components.x) (-components.y) (-components.z)


plus: Components3d -> Components3d -> Components3d
plus second first =
  Components3d (first.x + second.x) (first.y + second.y) (first.z + second.z)


minus: Components3d -> Components3d -> Components3d
minus second first =
  Components3d (first.x - second.x) (first.y - second.y) (first.z - second.z)


times: Float -> Components3d -> Components3d
times scale components =
  Components3d (components.x * scale) (components.y * scale) (components.z * scale)
