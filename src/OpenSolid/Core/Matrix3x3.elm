module OpenSolid.Core.Matrix3x3
  ( product
  , dotProduct
  ) where


type alias Components =
  { x: Float
  , y: Float
  , z: Float
  }


product: Components -> Components -> Components -> Components -> Components
product xBasis yBasis zBasis vector =
  Components
    (xBasis.x * vector.x + yBasis.x * vector.y + zBasis.x * vector.z)
    (xBasis.y * vector.x + yBasis.y * vector.y + zBasis.y * vector.z)
    (xBasis.z * vector.x + yBasis.z * vector.y + zBasis.z * vector.z)


dotProduct: Components -> Components -> Components -> Components -> Components
dotProduct xBasis yBasis zBasis vector =
  Components
    (xBasis.x * vector.x + xBasis.y * vector.y + xBasis.z * vector.z)
    (yBasis.x * vector.x + yBasis.y * vector.y + yBasis.z * vector.z)
    (zBasis.x * vector.x + zBasis.y * vector.y + zBasis.z * vector.z)
