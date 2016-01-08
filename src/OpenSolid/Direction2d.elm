module OpenSolid.Direction2d
  ( Direction2d(Direction2d)
  , none
  , polar
  , xDirection
  , yDirection
  , xComponent
  , yComponent
  , components
  , normalDirection
  ) where


type Direction2d =
  Direction2d Float Float


none: Direction2d
none =
  Direction2d 0 0


polar: Float -> Direction2d
polar angle =
  Direction2d (cos angle) (sin angle)


xDirection: Direction2d
xDirection =
  Direction2d 1 0


yDirection: Direction2d
yDirection =
  Direction2d 0 1


xComponent: Direction2d -> Float
xComponent (Direction2d x y) =
  x


yComponent: Direction2d -> Float
yComponent (Direction2d x y) =
  y


components: Direction2d -> (Float, Float)
components (Direction2d x y) =
  (x, y)


normalDirection: Direction2d -> Direction2d
normalDirection (Direction2d x y) =
  Direction2d (-y) x
