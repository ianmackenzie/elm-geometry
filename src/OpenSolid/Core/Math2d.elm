module OpenSolid.Core.Math2d
  ( negated
  , plus
  , minus
  , times
  , dot
  , cross
  , squaredNorm
  , norm
  , normalized
  , projectedOnto
  , perpendicular
  ) where


type alias Components =
  { x: Float
  , y: Float
  }


negated: Components -> Components
negated components =
  Components (-components.x) (-components.y)


plus: Components -> Components -> Components
plus other components =
  Components (components.x + other.x) (components.y + other.y)


minus: Components -> Components -> Components
minus other components =
  Components (components.x - other.x) (components.y - other.y)


times: Float -> Components -> Components
times scale components =
  Components (scale * components.x) (scale * components.y)


dot: Components -> Components -> Float
dot other components =
  components.x * other.x + components.y * other.y


cross: Components -> Components -> Float
cross other components =
  components.x * other.y - components.y * other.x


squaredNorm: Components -> Float
squaredNorm components =
  components.x * components.x + components.y * components.y


norm: Components -> Float
norm =
  squaredNorm >> sqrt


normalized: Components -> Components
normalized components =
  let
    componentsSquaredNorm = squaredNorm components
  in
    if componentsSquaredNorm == 0 then
      Components 0 0
    else
      times (1 / (sqrt componentsSquaredNorm)) components


projectedOnto: Components -> Components -> Components
projectedOnto direction components =
  times (dot direction components) direction


perpendicular: Components -> Components
perpendicular components =
  Components (-components.y) components.x
