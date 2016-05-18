module OpenSolid.Vector2d
    exposing
        ( zero
        , fromComponents
        , fromPolarComponents
        , xComponent
        , yComponent
        , components
        , polarComponents
        , equals
        , componentIn
        , squaredLength
        , length
        , normalize
        , direction
        , perpendicularVector
        , normalDirection
        , rotateBy
        , mirrorAbout
        , relativeTo
        , placeIn
        , projectionIn
        , projectOnto
        , placeOnto
        , negate
        , plus
        , minus
        , times
        , addTo
        , subtractFrom
        , dot
        , cross
        )

import OpenSolid.Core.Types exposing (..)


zero : Vector2d
zero =
    Vector2d 0 0


fromComponents : ( Float, Float ) -> Vector2d
fromComponents ( x, y ) =
    Vector2d x y


fromPolarComponents : ( Float, Float ) -> Vector2d
fromPolarComponents =
    fromPolar >> fromComponents


xComponent : Vector2d -> Float
xComponent (Vector2d x _) =
    x


yComponent : Vector2d -> Float
yComponent (Vector2d _ y) =
    y


components : Vector2d -> ( Float, Float )
components (Vector2d x y) =
    ( x, y )


polarComponents : Vector2d -> ( Float, Float )
polarComponents =
    components >> toPolar


equals : Vector2d -> Vector2d -> Bool
equals (Vector2d x2 y2) (Vector2d x1 y1) =
    x1 == x2 && y1 == y2


componentIn : Direction2d -> Vector2d -> Float
componentIn (Direction2d vector) =
    dot vector


squaredLength : Vector2d -> Float
squaredLength (Vector2d x y) =
    x * x + y * y


length : Vector2d -> Float
length =
    squaredLength >> sqrt


normalize : Vector2d -> Maybe Vector2d
normalize vector =
    if equals zero vector then
        Nothing
    else
        Just (times (1 / length vector) vector)


direction : Vector2d -> Maybe Direction2d
direction =
    normalize >> Maybe.map Direction2d


perpendicularVector : Vector2d -> Vector2d
perpendicularVector (Vector2d x y) =
    Vector2d (-y) x


normalDirection : Vector2d -> Maybe Direction2d
normalDirection =
    perpendicularVector >> direction


rotateBy : Float -> Vector2d -> Vector2d
rotateBy angle =
    let
        cosine =
            cos angle

        sine =
            sin angle
    in
        \(Vector2d x y) ->
            Vector2d (x * cosine - y * sine) (y * cosine + x * sine)


mirrorAbout : Direction2d -> Vector2d -> Vector2d
mirrorAbout direction =
    let
        (Direction2d (Vector2d dx dy)) =
            direction

        a =
            1 - 2 * dy * dy

        b =
            2 * dx * dy

        c =
            1 - 2 * dx * dx
    in
        \(Vector2d vx vy) -> Vector2d (a * vx + b * vy) (c * vy + b * vx)


relativeTo : Frame2d -> Vector2d -> Vector2d
relativeTo frame vector =
    Vector2d (componentIn frame.xDirection vector)
        (componentIn frame.yDirection vector)


placeIn : Frame2d -> Vector2d -> Vector2d
placeIn frame =
    let
        (Direction2d (Vector2d x1 y1)) =
            frame.xDirection

        (Direction2d (Vector2d x2 y2)) =
            frame.yDirection
    in
        \(Vector2d x y) -> Vector2d (x1 * x + x2 * y) (y1 * x + y2 * y)


projectionIn : Direction2d -> Vector2d -> Vector2d
projectionIn ((Direction2d directionVector) as direction) vector =
    times (componentIn direction vector) directionVector


projectOnto : Axis2d -> Vector2d -> Vector2d
projectOnto axis =
    projectionIn axis.direction


placeOnto : Plane3d -> Vector2d -> Vector3d
placeOnto plane =
    let
        (Direction3d (Vector3d x1 y1 z1)) =
            plane.xDirection

        (Direction3d (Vector3d x2 y2 z2)) =
            plane.yDirection
    in
        \(Vector2d x y) ->
            Vector3d (x1 * x + x2 * y) (y1 * x + y2 * y) (z1 * x + z2 * y)


negate : Vector2d -> Vector2d
negate (Vector2d x y) =
    Vector2d (-x) (-y)


plus : Vector2d -> Vector2d -> Vector2d
plus (Vector2d x2 y2) (Vector2d x1 y1) =
    Vector2d (x1 + x2) (y1 + y2)


minus : Vector2d -> Vector2d -> Vector2d
minus (Vector2d x2 y2) (Vector2d x1 y1) =
    Vector2d (x1 - x2) (y1 - y2)


times : Float -> Vector2d -> Vector2d
times scale (Vector2d x y) =
    Vector2d (x * scale) (y * scale)


addTo : Point2d -> Vector2d -> Point2d
addTo (Point2d px py) (Vector2d vx vy) =
    Point2d (px + vx) (py + vy)


subtractFrom : Point2d -> Vector2d -> Point2d
subtractFrom (Point2d px py) (Vector2d vx vy) =
    Point2d (px - vx) (py - vy)


dot : Vector2d -> Vector2d -> Float
dot (Vector2d x2 y2) (Vector2d x1 y1) =
    x1 * x2 + y1 * y2


cross : Vector2d -> Vector2d -> Float
cross (Vector2d x2 y2) (Vector2d x1 y1) =
    x1 * y2 - y1 * x2
