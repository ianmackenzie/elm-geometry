module OpenSolid.Direction2d
    (
    Direction2d,
    fromComponents,
    directionOf,
    polar,
    xDirection,
    yDirection,
    asVector,
    xComponent,
    yComponent,
    components,
    normalDirection,
    transform
    ) where


import OpenSolid.Vector2d as Vector2d exposing (Vector2d(Vector2d))
import OpenSolid.Transformation2d as Transformation2d exposing (Transformation2d)


type Direction2d =
    Direction2d Vector2d


fromComponents: Float -> Float -> Direction2d
fromComponents x y =
    Direction2d (Vector2d x y)


directionOf: Vector2d -> Direction2d
directionOf vector =
    Direction2d (Vector2d.normalized vector)


polar: Float -> Direction2d
polar angle =
    Direction2d (Vector2d (cos angle) (sin angle))


xDirection: Direction2d
xDirection =
    Direction2d (Vector2d 1.0 0.0)


yDirection: Direction2d
yDirection =
    Direction2d (Vector2d 0.0 1.0)


asVector: Direction2d -> Vector2d
asVector (Direction2d vector) =
    vector


xComponent: Direction2d -> Float
xComponent (Direction2d vector) =
    Vector2d.xComponent vector


yComponent: Direction2d -> Float
yComponent (Direction2d vector) =
    Vector2d.yComponent vector


components: Direction2d -> (Float, Float)
components (Direction2d vector) =
    Vector2d.components vector


normalDirection: Direction2d -> Direction2d
normalDirection (Direction2d (Vector2d x y)) =
    Direction2d (Vector2d (-y) x)


transform: Transformation2d -> Direction2d -> Direction2d
transform transformation (Direction2d vector) =
    Direction2d (transformation.transformVector vector)
