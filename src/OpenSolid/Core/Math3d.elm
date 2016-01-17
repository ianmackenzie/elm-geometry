module OpenSolid.Core.Math3d
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
  , perpendicularDirection
  ) where


type alias Components =
  { x: Float
  , y: Float
  , z: Float
  }


negated: Components -> Components
negated components =
  Components (-components.x) (-components.y) (-components.z)


plus: Components -> Components -> Components
plus other components =
  Components (components.x + other.x) (components.y + other.y) (components.z + other.z)


minus: Components -> Components -> Components
minus other components =
  Components (components.x - other.x) (components.y - other.y) (components.z - other.z)


times: Float -> Components -> Components
times scale components =
  Components (scale * components.x) (scale * components.y) (scale * components.z)


dot: Components -> Components -> Float
dot other components =
  components.x * other.x + components.y * other.y + components.z * other.z


cross: Components -> Components -> Components
cross other components =
  let
    x = components.y * other.z - components.z * other.y
    y = components.z * other.x - components.x * other.z
    z = components.x * other.y - components.y * other.x
  in
    Components x y z


squaredNorm: Components -> Float
squaredNorm components =
  components.x * components.x + components.y * components.y + components.z * components.z


norm: Components -> Float
norm =
  squaredNorm >> sqrt


normalized: Components -> Components
normalized components =
  let
    componentsSquaredNorm = squaredNorm components
  in
    if componentsSquaredNorm == 0 then
      Components 0 0 0
    else
      times (1 / (sqrt componentsSquaredNorm)) components


projectedOnto: Components -> Components -> Components
projectedOnto direction components =
  times (dot direction components) direction


perpendicularDirection: Components -> Components
perpendicularDirection components =
  let
    absX = abs components.x
    absY = abs components.y
    absZ = abs components.z
    other =
      if absX <= absY then
        if absX <= absZ then
          Components 1 0 0
        else
          Components 0 0 1
      else
        if absY <= absZ then
          Components 0 1 0
        else
          Components 0 0 1
  in
    normalized (cross other components)
