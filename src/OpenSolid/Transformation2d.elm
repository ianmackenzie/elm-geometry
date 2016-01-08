module OpenSolid.Transformation2d
    ( Transformation2d
    , translation
    , rotation
    , compose
    ) where


import OpenSolid.Vector2d as Vector2d exposing (Vector2d(Vector2d))
import OpenSolid.Point2d as Point2d exposing (Point2d(Point2d))


type alias Transformation2d =
    { transformVector: Vector2d -> Vector2d
    , transformPoint: Point2d -> Point2d
    }


translation: Vector2d -> Transformation2d
translation vector =
    { transformVector = \vector -> vector
    , transformPoint = \point -> Point2d.sum point vector
    }


rotateVector: Vector2d -> Float -> Float -> Vector2d
rotateVector (Vector2d x y) sinAngle cosAngle =
    Vector2d (x * cosAngle - y * sinAngle) (x * sinAngle + y * cosAngle)


rotatePoint: Point2d -> Point2d -> Float -> Float -> Point2d
rotatePoint point originPoint sinAngle cosAngle =
    Point2d.sum originPoint (rotateVector (Point2d.difference point originPoint) sinAngle cosAngle)


rotation: Point2d -> Float -> Transformation2d
rotation point angle =
    let
        sinAngle = sin angle
        cosAngle = cos angle
    in
        { transformVector = \v -> rotateVector v sinAngle cosAngle
        , transformPoint = \p -> rotatePoint p point sinAngle cosAngle
        }


compose: Transformation2d -> Transformation2d -> Transformation2d
compose outer inner =
    { transformVector = \v -> outer.transformVector (inner.transformVector v)
    , transformPoint = \p -> outer.transformPoint (inner.transformPoint p)
    }
