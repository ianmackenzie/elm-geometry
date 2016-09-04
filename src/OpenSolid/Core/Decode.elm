module OpenSolid.Core.Decode
    exposing
        ( vector2d
        , vector3d
        , direction2d
        , direction3d
        , point2d
        , point3d
        , axis2d
        , axis3d
        , plane3d
        , frame2d
        , frame3d
        , sketchPlane3d
        , boundingBox2d
        , boundingBox3d
        )

import Json.Decode as Decode exposing (Decoder, (:=))
import OpenSolid.Core.Types exposing (..)


{-| Decoder for Vector2d values from objects with 'x' and 'y' fields.
-}
vector2d : Decoder Vector2d
vector2d =
    Decode.object2 (\x y -> Vector2d ( x, y ))
        ("x" := Decode.float)
        ("y" := Decode.float)


{-| Decoder for Vector3d values from objects with 'x', 'y' and 'z' fields.
-}
vector3d : Decoder Vector3d
vector3d =
    Decode.object3 (\x y z -> Vector3d ( x, y, z ))
        ("x" := Decode.float)
        ("y" := Decode.float)
        ("z" := Decode.float)


{-| Decoder for Direction2d values from objects with 'x' and 'y' fields.
-}
direction2d : Decoder Direction2d
direction2d =
    Decode.object2 (\x y -> Direction2d ( x, y ))
        ("x" := Decode.float)
        ("y" := Decode.float)


{-| Decoder for Direction3d values from objects with 'x', 'y' and 'z' fields.
-}
direction3d : Decoder Direction3d
direction3d =
    Decode.object3 (\x y z -> Direction3d ( x, y, z ))
        ("x" := Decode.float)
        ("y" := Decode.float)
        ("z" := Decode.float)


{-| Decoder for Point2d values from objects with 'x' and 'y' fields.
-}
point2d : Decoder Point2d
point2d =
    Decode.object2 (\x y -> Point2d ( x, y ))
        ("x" := Decode.float)
        ("y" := Decode.float)


{-| Decoder for Point3d values from objects with 'x', 'y' and 'z' fields.
-}
point3d : Decoder Point3d
point3d =
    Decode.object3 (\x y z -> Point3d ( x, y, z ))
        ("x" := Decode.float)
        ("y" := Decode.float)
        ("z" := Decode.float)


{-| Decoder for Axis2d values from objects with 'originPoint' and 'direction'
fields.
-}
axis2d : Decoder Axis2d
axis2d =
    Decode.object2
        (\originPoint direction ->
            Axis2d { originPoint = originPoint, direction = direction }
        )
        ("originPoint" := point2d)
        ("direction" := direction2d)


{-| Decoder for Axis3d values from objects with 'originPoint' and 'direction'
fields.
-}
axis3d : Decoder Axis3d
axis3d =
    Decode.object2
        (\originPoint direction ->
            Axis3d { originPoint = originPoint, direction = direction }
        )
        ("originPoint" := point3d)
        ("direction" := direction3d)


{-| Decoder for Plane3d values from objects with 'originPoint' and
'normalDirection' fields.
-}
plane3d : Decoder Plane3d
plane3d =
    Decode.object2
        (\originPoint normalDirection ->
            Plane3d
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
        )
        ("originPoint" := point3d)
        ("normalDirection" := direction3d)


extremum : Decoder Float
extremum =
    let
        parse string =
            case string of
                "NaN" ->
                    Ok (0 / 0)

                "Infinity" ->
                    Ok (1 / 0)

                "-Infinity" ->
                    Ok (-1 / 0)

                _ ->
                    Err "Expected 'NaN', 'Infinity' or '-Infinity'"

        specialValueDecoder =
            Decode.customDecoder Decode.string parse
    in
        Decode.oneOf [ specialValueDecoder, Decode.float ]
