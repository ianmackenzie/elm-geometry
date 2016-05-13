module OpenSolid.Frame2d (global, xAxis, yAxis, point, vector, scaleAbout, rotateAbout, translateBy, mirrorAbout, relativeTo, placeIn) where

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d


global : Frame2d
global =
  Frame2d Point2d.origin Direction2d.x Direction2d.y


xAxis : Frame2d -> Axis2d
xAxis frame =
  Axis2d frame.originPoint frame.xDirection


yAxis : Frame2d -> Axis2d
yAxis frame =
  Axis2d frame.originPoint frame.yDirection


point : Float -> Float -> Frame2d -> Point2d
point x y frame =
  Point2d.plus (vector x y frame) frame.originPoint


vector : Float -> Float -> Frame2d -> Vector2d
vector x y frame =
  Vector2d.plus (Direction2d.times y frame.yDirection) (Direction2d.times x frame.xDirection)


scaleAbout : Point2d -> Float -> Frame2d -> Frame2d
scaleAbout centerPoint scale frame =
  { frame | originPoint = Point2d.scaleAbout centerPoint scale frame.originPoint }


rotateAbout : Point2d -> Float -> Frame2d -> Frame2d
rotateAbout centerPoint angle =
  let
    rotatePoint =
      Point2d.rotateAbout centerPoint angle

    rotateDirection =
      Direction2d.rotateBy angle
  in
    \frame ->
      let
        originPoint =
          rotatePoint frame.originPoint

        xDirection =
          rotateDirection frame.xDirection

        yDirection =
          rotateDirection frame.yDirection
      in
        Frame2d originPoint xDirection yDirection


translateBy : Vector2d -> Frame2d -> Frame2d
translateBy vector frame =
  { frame | originPoint = Point2d.plus vector frame.originPoint }


mirrorAbout : Axis2d -> Frame2d -> Frame2d
mirrorAbout axis =
  let
    mirrorPoint =
      Point2d.mirrorAbout axis

    mirrorDirection =
      Direction2d.mirrorAbout axis.direction
  in
    \frame ->
      let
        originPoint =
          mirrorPoint frame.originPoint

        xDirection =
          mirrorDirection frame.xDirection

        yDirection =
          mirrorDirection frame.yDirection
      in
        Frame2d originPoint xDirection yDirection


relativeTo : Frame2d -> Frame2d -> Frame2d
relativeTo otherFrame =
  let
    localizePoint =
      Point2d.relativeTo otherFrame

    localizeDirection =
      Direction2d.relativeTo otherFrame
  in
    \frame ->
      let
        originPoint =
          localizePoint frame.originPoint

        xDirection =
          localizeDirection frame.xDirection

        yDirection =
          localizeDirection frame.yDirection
      in
        Frame2d originPoint xDirection yDirection


placeIn : Frame2d -> Frame2d -> Frame2d
placeIn frame =
  let
    globalizePoint =
      Point2d.placeIn frame

    globalizeDirection =
      Direction2d.placeIn frame
  in
    \plane ->
      let
        originPoint =
          globalizePoint plane.originPoint

        xDirection =
          globalizeDirection plane.xDirection

        yDirection =
          globalizeDirection plane.yDirection
      in
        Frame2d originPoint xDirection yDirection
