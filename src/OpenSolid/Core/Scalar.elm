module OpenSolid.Core.Scalar
  ( isZeroWithinTolerance
  ) where


isZeroWithinTolerance: Float -> Float -> Bool
isZeroWithinTolerance tolerance value =
  -tolerance <= value && value <= tolerance
