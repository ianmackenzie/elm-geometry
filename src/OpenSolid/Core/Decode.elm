module OpenSolid.Core.Decode (interval, vector2d, vector3d, direction2d, direction3d, point2d, point3d, boundingBox2d, boundingBox3d, axis2d, axis3d, plane3d, frame2d, frame3d) where

{-| JSON decoders for the core OpenSolid types.
-}

import Json.Decode exposing (..)
import OpenSolid.Core.Types exposing (..)


intervalEndpoint : Decoder Float
intervalEndpoint =
  let
    decodeString string =
      case string of
        "NaN" ->
          Ok (0 / 0)

        "Infinity" ->
          Ok (1 / 0)

        "-Infinity" ->
          Ok (-1 / 0)

        _ ->
          Err "Expected 'NaN', 'Infinity' or '-Infinity'"
  in
    oneOf float (customDecoder string decodeString)


interval : Decoder Interval
interval =
  tuple2 Interval intervalEndpoint intervalEndpoint


vector2d : Decoder Vector2d
vector2d =
  tuple2 Vector2d float float


vector3d : Decoder Vector3d
vector3d =
  tuple3 Vector3d float float float


direction2d : Decoder Direction2d
direction2d =
  tuple2 (Vector2d >> Direction2d) float float


direction3d : Decoder Direction3d
direction3d =
  tuple3 (Vector3d >> Direction3d) float float float


point2d : Decoder Point2d
point2d =
  tuple2 Point2d float float


point3d : Decoder Point3d
point3d =
  tuple3 Point3d float float float


boundingBox2d : Decoder BoundingBox2d
boundingBox2d =
  tuple2 BoundingBox2d interval interval


boundingBox3d : Decoder BoundingBox3d
boundingBox3d =
  tuple3 BoundingBox3d interval interval interval


axis2d : Decoder Axis2d
axis2d =
  object2 Axis2d ("originPoint" := point2d) ("direction" := direction2d)


axis3d : Decoder Axis3d
axis3d =
  object2 Axis3d ("originPoint" := point3d) ("direction" := direction3d)


plane3d : Decoder Plane3d
plane3d =
  object4
    Plane3d
    ("originPoint" := point3d)
    ("xDirection" := direction3d)
    ("yDirection" := direction3d)
    ("normalDirection" := direction3d)


frame2d : Decoder Frame2d
frame2d =
  object3
    Frame2d
    ("originPoint" := point2d)
    ("xDirection" := direction2d)
    ("yDirection" := direction2d)


frame3d : Decoder Frame3d
frame3d =
  object3
    Frame3d
    ("originPoint" := point3d)
    ("xDirection" := direction3d)
    ("yDirection" := direction3d)
    ("zDirection" := direction3d)
