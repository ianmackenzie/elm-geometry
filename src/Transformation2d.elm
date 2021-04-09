module Transformation2d exposing (Allowed, Transformation2d, at, at_, followedBy, identity, relativeTo, scaleAbout, translateBy, translateIn, unsafe, unwrap)

import Geometry.Types as Types exposing (Allowed, Axis2d, Direction2d, Frame2d, Point2d, Transformation2d(..), Vector2d)
import Quantity exposing (Quantity(..), Rate, Squared, Unitless)


type alias Transformation2d unitFn coordFn restrictions =
    Types.Transformation2d unitFn coordFn restrictions


type alias Allowed =
    Types.Allowed


identity : Transformation2d (units -> units) (coords -> coords) restrictions
identity =
    Transformation2d 1 0 0 0 1 0


translateBy : Vector2d units coords -> Transformation2d (units -> units) (coords -> coords) restrictions
translateBy (Types.Vector2d v) =
    Transformation2d 1 0 v.x 0 1 v.y


translateIn :
    Direction2d coords
    -> Quantity Float units
    -> Transformation2d (units -> units) (coords -> coords) restrictions
translateIn (Types.Direction2d d) (Quantity distance) =
    Transformation2d 1 0 (distance * d.x) 0 1 (distance * d.y)


at : Quantity Float (Rate destinationUnits sourceUnits) -> Transformation2d (sourceUnits -> destinationUnits) (coords -> coords) { a | scale : Allowed }
at (Quantity rate) =
    Transformation2d rate 0 0 0 rate 0


at_ : Quantity Float (Rate sourceUnits destinationUnits) -> Transformation2d (sourceUnits -> destinationUnits) (coords -> coords) { a | scale : Allowed }
at_ (Quantity rate) =
    Transformation2d (1 / rate) 0 0 0 (1 / rate) 0


scaleAbout :
    Point2d units coordinates
    -> Float
    -> Transformation2d (units -> units) (coordinates -> coordinates) { a | scale : Allowed }
scaleAbout (Types.Point2d p) k =
    Transformation2d k 0 (k * p.x - p.x) 0 k (k * p.y - p.y)


relativeTo :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Transformation2d (units -> units) (globalCoordinates -> localCoordinates) restrictions
relativeTo (Types.Frame2d frame) =
    Debug.todo "Eh... How do we do this again?"


unsafe : ( ( Float, Float, Float ), ( Float, Float, Float ) ) -> Transformation2d unitFn coordsFn restrictions
unsafe ( ( a11, a12, a13 ), ( a21, a22, a23 ) ) =
    Transformation2d a11 a12 a13 a21 a22 a23


unwrap : Transformation2d unitFn coordsFn restrictions -> ( ( Float, Float, Float ), ( Float, Float, Float ) )
unwrap (Transformation2d a11 a12 a13 a21 a22 a23) =
    ( ( a11, a12, a13 ), ( a21, a22, a23 ) )


followedBy :
    Transformation2d (units2 -> units3) (coordinates2 -> coordinates3) restrictions
    -> Transformation2d (units1 -> units2) (coordinates1 -> coordinates2) restrictions
    -> Transformation2d (units1 -> units3) (coordinates1 -> coordinates3) restrictions
followedBy (Transformation2d b11 b12 b13 b21 b22 b23) (Transformation2d a11 a12 a13 a21 a22 a23) =
    Transformation2d
        (a11 * b11 + a12 * b21)
        (a11 * b12 + a12 * b22)
        (a11 * b13 + a12 * b23 + a13)
        (a21 * b11 + a22 * b21)
        (a21 * b12 + a22 * b22)
        (a21 * b13 + a22 * b23 + a23)
