module OpenSolid.Transformation2d
  ( Transformation2d
  , translationBy
  , rotationAbout
  , compound
  ) where


type alias Vector2d =
  { x : Float
  , y : Float
  }


type alias Point2d =
  { x : Float
  , y : Float
  }


type alias Transformation2d =
  { ofVector: Vector2d -> Vector2d
  , ofPoint: Point2d -> Point2d
  }


translationBy: Vector2d -> Transformation2d
translationBy vector =
  Transformation2d identity (\point -> Point2d (point.x + vector.x) (point.y + vector.y))


rotateVector: Float -> Float -> Vector2d -> Vector2d
rotateVector sinAngle cosAngle vector =
  let
    x = vector.x * cosAngle - vector.y * sinAngle
    y = vector.x * sinAngle + vector.y * cosAngle
  in
    Vector2d x y


rotatePoint: Point2d -> Float -> Float -> Point2d -> Point2d
rotatePoint originPoint sinAngle cosAngle point =
  let
    vector = Vector2d (point.x - originPoint.x) (point.y - originPoint.y)
    rotatedVector = rotateVector sinAngle cosAngle vector
  in
    Point2d (originPoint.x + rotatedVector.x) (originPoint.y + rotatedVector.y)


rotationAbout: Point2d -> Float -> Transformation2d
rotationAbout point angle =
  let
    sinAngle = sin angle
    cosAngle = cos angle
  in
    Transformation2d (rotateVector sinAngle cosAngle) (rotatePoint point sinAngle cosAngle)


compound: Transformation2d -> Transformation2d -> Transformation2d
compound outer inner =
  Transformation2d (outer.ofVector << inner.ofVector) (outer.ofPoint << inner.ofPoint)
