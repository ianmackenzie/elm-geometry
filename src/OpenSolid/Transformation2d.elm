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


rotateVector: Vector2d -> Float -> Float -> Vector2d
rotateVector vector sinAngle cosAngle =
  let
    x = vector.x * cosAngle - vector.y * sinAngle
    y = vector.x * sinAngle + vector.y * cosAngle
  in
    Vector2d x y


rotatePoint: Point2d -> Point2d -> Float -> Float -> Point2d
rotatePoint point originPoint sinAngle cosAngle =
  let
    vector = Vector2d (point.x - originPoint.x) (point.y - originPoint.y)
    rotatedVector = rotateVector vector sinAngle cosAngle
  in
    Point2d (originPoint.x + rotatedVector.x) (originPoint.y + rotatedVector.y)


rotationAbout: Point2d -> Float -> Transformation2d
rotationAbout point angle =
  let
    sinAngle = sin angle
    cosAngle = cos angle
    ofVector = \vector -> rotateVector vector sinAngle cosAngle
    ofPoint = \point' -> rotatePoint point' point sinAngle cosAngle
  in
    Transformation2d ofVector ofPoint


compound: Transformation2d -> Transformation2d -> Transformation2d
compound outer inner =
  Transformation2d (outer.ofVector << inner.ofVector) (outer.ofPoint << inner.ofPoint)
