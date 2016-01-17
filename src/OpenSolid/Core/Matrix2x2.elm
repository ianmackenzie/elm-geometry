module OpenSolid.Core.Matrix2x2
  ( product
  , dotProduct
  ) where


type alias Components =
  { x: Float
  , y: Float
  }


product: Components -> Components -> Components -> Components
product xBasis yBasis vector =
  Components
    (xBasis.x * vector.x + yBasis.x * vector.y)
    (xBasis.y * vector.x + yBasis.y * vector.y)


dotProduct: Components -> Components -> Components -> Components
dotProduct xBasis yBasis vector =
  Components
    (xBasis.x * vector.x + xBasis.y * vector.y)
    (yBasis.x * vector.x + yBasis.y * vector.y)
