module Transformation2d exposing (Allowed, Transformation2d, at, at_, followedBy, identity, relativeTo, scaleAbout, translateBy, translateIn, unsafe, unwrap)

import Geometry.Types as Types exposing (Allowed, Axis2d, Direction2d, Frame2d, Point2d, Transformation2d(..), Vector2d)
import Quantity exposing (Quantity(..), Rate, Squared, Unitless)


type alias Transformation2d unitFn coordFn restrictions =
    Types.Transformation2d unitFn coordFn restrictions


type alias Allowed =
    Types.Allowed


identity : Transformation2d (units -> units) (coords -> coords) restrictions
identity =
    Transformation2d
        { m11 = 1
        , m12 = 0
        , m13 = 0
        , m21 = 0
        , m22 = 1
        , m23 = 0
        , m31 = 0
        , m32 = 0
        , m33 = 1
        }


translateBy : Vector2d units coords -> Transformation2d (units -> units) (coords -> coords) restrictions
translateBy (Types.Vector2d v) =
    Transformation2d
        { m11 = 1
        , m12 = 0
        , m13 = v.x
        , m21 = 0
        , m22 = 1
        , m23 = v.y
        , m31 = 0
        , m32 = 0
        , m33 = 1
        }


translateIn :
    Direction2d coords
    -> Quantity Float units
    -> Transformation2d (units -> units) (coords -> coords) restrictions
translateIn (Types.Direction2d d) (Quantity distance) =
    Transformation2d
        { m11 = 1
        , m12 = 0
        , m13 = distance * d.x
        , m21 = 0
        , m22 = 1
        , m23 = distance * d.y
        , m31 = 0
        , m32 = 0
        , m33 = 1
        }


at : Quantity Float (Rate destinationUnits sourceUnits) -> Transformation2d (sourceUnits -> destinationUnits) (coords -> coords) { a | scale : Allowed }
at (Quantity rate) =
    Transformation2d
        { m11 = rate
        , m12 = 0
        , m13 = 0
        , m21 = 0
        , m22 = rate
        , m23 = 0
        , m31 = 0
        , m32 = 0
        , m33 = 1
        }


at_ : Quantity Float (Rate sourceUnits destinationUnits) -> Transformation2d (sourceUnits -> destinationUnits) (coords -> coords) { a | scale : Allowed }
at_ (Quantity rate) =
    Transformation2d
        { m11 = 1 / rate
        , m12 = 0
        , m13 = 0
        , m21 = 0
        , m22 = 1 / rate
        , m23 = 0
        , m31 = 0
        , m32 = 0
        , m33 = 1
        }


scaleAbout :
    Point2d units coordinates
    -> Float
    -> Transformation2d (units -> units) (coordinates -> coordinates) { a | scale : Allowed }
scaleAbout (Types.Point2d p) k =
    Transformation2d
        { m11 = k
        , m12 = 0
        , m13 = p.x - k * p.x
        , m21 = 0
        , m22 = k
        , m23 = p.y - k * p.y
        , m31 = 0
        , m32 = 0
        , m33 = 1
        }


relativeTo :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Transformation2d (units -> units) (globalCoordinates -> localCoordinates) restrictions
relativeTo (Types.Frame2d frame) =
    Debug.todo "Eh... How do we do this again?"


unsafe :
    { m11 : Float
    , m12 : Float
    , m13 : Float
    , m21 : Float
    , m22 : Float
    , m23 : Float
    , m31 : Float
    , m32 : Float
    , m33 : Float
    }
    -> Transformation2d unitFn coordsFn restrictions
unsafe transformation =
    Transformation2d transformation


unwrap :
    Transformation2d unitFn coordsFn restrictions
    ->
        { m11 : Float
        , m12 : Float
        , m13 : Float
        , m21 : Float
        , m22 : Float
        , m23 : Float
        , m31 : Float
        , m32 : Float
        , m33 : Float
        }
unwrap (Transformation2d transformation) =
    transformation


followedBy :
    Transformation2d (units2 -> units3) (coordinates2 -> coordinates3) restrictions
    -> Transformation2d (units1 -> units2) (coordinates1 -> coordinates2) restrictions
    -> Transformation2d (units1 -> units3) (coordinates1 -> coordinates3) restrictions
followedBy (Transformation2d b) (Transformation2d a) =
    Transformation2d
        { m11 = a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31
        , m12 = a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32
        , m13 = a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33
        , m21 = a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31
        , m22 = a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32
        , m23 = a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33
        , m31 = a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31
        , m32 = a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32
        , m33 = a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33
        }
