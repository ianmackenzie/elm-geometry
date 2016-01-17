module OpenSolid.Core.Transformation2d
  ( translationBy
  , rotationAbout
  , localizationTo
  , globalizationFrom
  , andThen
  ) where


import OpenSolid.Core exposing (Transformation2d, Vector2d, Point2d, Frame2d)


translationBy: Vector2d -> Transformation2d
translationBy vector =
  ( identity
  , \point -> Point2d (point.x + vector.x) (point.y + vector.y)
  )


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
    ( rotateVector sinAngle cosAngle
    , rotatePoint point sinAngle cosAngle
    )


localizationTo: Frame2d -> Transformation2d
localizationTo frame =
  let
    transformVector =
      \vector ->
        let
          x = Vector2d.dot frame.xDirection vector
          y = Vector2d.dot frame.yDirection vector
        in
          Vector2d x y
  in
    ( transformVector
    , Vector2d.minus frame.originPoint >> transformVector
    )


globalizationFrom: Frame2d -> Transformation2d
globalizationFrom frame =
  let
    transformVector =
      \vector ->
        let
          xVector = Vector2d.times vector.x frame.xDirection
          yVector = Vector2d.times vector.y frame.yDirection
        in
          Vector2d.plus yVector xVector
  in
    ( transformVector
    , transformVector >> Vector2d.plus frame.originPoint
    )


andThen: Transformation2d -> Transformation2d -> Transformation2d
andThen second first =
  ( fst first >> fst second
  , snd first >> snd second
  )
