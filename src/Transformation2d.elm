module Transformation2d exposing (Allowed, Transformation2d, at, identity, relativeTo, translateBy, translateIn, unsafe, unwrap)

import Geometry.Types as Types exposing (Axis2d, Direction2d, Frame2d, Vector2d)
import Quantity exposing (Quantity(..), Rate, Squared, Unitless)


type Transformation2d info
    = Transformation2d Float Float Float Float Float Float


type Allowed
    = Allowed Never


identity :
    Transformation2d
        { inputUnits : unit
        , outputUnits : unit
        , inputCoordinates : coords
        , outputCoords : coords
        , scaling : scaling
        , shear : shear
        , rotation : rotation
        }
identity =
    Transformation2d 1 0 0 0 1 0


translateBy :
    Vector2d units coords
    -> Transformation2d { a | outputUnits : units, outputCoords : coords }
    -> Transformation2d { a | outputUnits : units, outputCoords : coords }
translateBy (Types.Vector2d v) =
    mult 1 0 v.x 0 1 v.y


translateIn :
    Direction2d coordinates
    -> Quantity Float units
    -> Transformation2d { a | outputUnits : units, outputCoords : coords }
    -> Transformation2d { a | outputUnits : units, outputCoords : coords }
translateIn (Types.Direction2d d) (Quantity distance) =
    mult 1 0 (distance * d.x) 0 1 (distance * d.y)


at :
    Quantity Float (Rate destinationUnits sourceUnits)
    -> Transformation2d { a | outputUnits : sourceUnits }
    -> Transformation2d { a | outputUnits : destinationUnits }
at (Quantity rate) =
    mult 1 0 rate 0 1 rate


relativeTo :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Transformation2d { a | outputUnits : units, outputCoords : globalCoordinates }
    -> Transformation2d { a | outputUnits : units, outputCoords : localCoordinates }
relativeTo (Types.Frame2d frame) =
    Debug.todo "Eh... How do we do this again?"


unsafe : ( ( Float, Float, Float ), ( Float, Float, Float ) ) -> Transformation2d a
unsafe ( ( a11, a12, a13 ), ( a21, a22, a23 ) ) =
    Transformation2d a11 a12 a13 a21 a22 a23


unwrap : Transformation2d a -> ( ( Float, Float, Float ), ( Float, Float, Float ) )
unwrap (Transformation2d a11 a12 a13 a21 a22 a23) =
    ( ( a11, a12, a13 ), ( a21, a22, a23 ) )



-- Private


mult : Float -> Float -> Float -> Float -> Float -> Float -> Transformation2d a -> Transformation2d b
mult b11 b12 b13 b21 b22 b23 (Transformation2d a11 a12 a13 a21 a22 a23) =
    Transformation2d
        (a11 * b11 + a12 * b21)
        (a11 * b12 + a12 * b22)
        (a11 * b13 + a12 * b23 + a13 * 1)
        (a21 * b11 + a22 * b21)
        (a21 * b12 + a22 * b22)
        (a21 * b13 + a22 * b23 + a23 * 1)
