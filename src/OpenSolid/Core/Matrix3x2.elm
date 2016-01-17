module OpenSolid.Core.Matrix3x2
  ( product
  , dotProduct
  ) where


type alias Components2d =
  { x: Float
  , y: Float
  }


type alias Components3d =
  { x: Float
  , y: Float
  , z: Float
  }


product: Components3d -> Components3d -> Components2d -> Components3d
product xBasis yBasis vector =
  Components3d
    (xBasis.x * vector.x + yBasis.x * vector.y)
    (xBasis.y * vector.x + yBasis.y * vector.y)
    (xBasis.z * vector.x + yBasis.z * vector.y)


dotProduct: Components3d -> Components3d -> Components3d -> Components2d
dotProduct xBasis yBasis vector =
  Components2d
    (xBasis.x * vector.x + xBasis.y * vector.y + xBasis.z * vector.z)
    (yBasis.x * vector.x + yBasis.y * vector.y + yBasis.z * vector.z)
