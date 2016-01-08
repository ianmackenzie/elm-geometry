module OpenSolid.Vector2d
    ( Vector2d(Vector2d)
    , zero
    , xComponent
    , yComponent
    , components
    , squaredLength
    , length
    , normalized
    , dot
    , cross
    ) where


type Vector2d =
    Vector2d Float Float


zero: Vector2d
zero =
    Vector2d 0.0 0.0


xComponent: Vector2d -> Float
xComponent (Vector2d x y) =
    x


yComponent: Vector2d -> Float
yComponent (Vector2d x y) =
    y


components: Vector2d -> (Float, Float)
components (Vector2d x y) =
    (x, y)


squaredLength: Vector2d -> Float
squaredLength (Vector2d x y) =
    x^2 + y^2


length: Vector2d -> Float
length vector =
    sqrt (squaredLength vector)


normalized: Vector2d -> Vector2d
normalized vector =
    product (1.0 / (length vector)) vector


negated: Vector2d -> Vector2d
negated (Vector2d x y) =
    Vector2d (-x) (-y)


sum: Vector2d -> Vector2d -> Vector2d
sum (Vector2d x1 y1) (Vector2d x2 y2) =
    Vector2d (x1 + x2) (y1 + y2)


difference: Vector2d -> Vector2d -> Vector2d
difference (Vector2d x1 y1) (Vector2d x2 y2) =
    Vector2d (x1 - x2) (y1 - y2)


product: Float -> Vector2d -> Vector2d
product scale (Vector2d x y) =
    Vector2d (scale * x) (scale * y)


dot: Vector2d -> Vector2d -> Float
dot (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * x2 + y1 * y2


cross: Vector2d -> Vector2d -> Float
cross (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * y2 - y1 * x2
