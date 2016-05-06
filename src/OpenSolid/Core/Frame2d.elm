module OpenSolid.Core.Frame2d
  ( global
  , xAxis
  , yAxis
  , scaledAbout
  , rotatedAbout
  , translatedBy
  , mirroredAbout
  , relativeTo
  , placedIn
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Direction2d as Direction2d


global: Frame2d
global =
  Frame2d Point2d.origin Direction2d.x Direction2d.y


xAxis: Frame2d -> Axis2d
xAxis frame =
  Axis2d frame.originPoint frame.xDirection


yAxis: Frame2d -> Axis2d
yAxis frame =
  Axis2d frame.originPoint frame.yDirection


scaledAbout: Point2d -> Float -> Frame2d -> Frame2d
scaledAbout centerPoint scale frame =
  { frame | originPoint = Point2d.scaledAbout centerPoint scale frame.originPoint }


rotatedAbout: Point2d -> Float -> Frame2d -> Frame2d
rotatedAbout centerPoint angle =
  let
    rotatePoint = Point2d.rotatedAbout centerPoint angle
    rotateDirection = Direction2d.rotatedBy angle
  in
    \frame ->
      let
        originPoint = rotatePoint frame.originPoint
        xDirection = rotateDirection frame.xDirection
        yDirection = rotateDirection frame.yDirection
      in
        Frame2d originPoint xDirection yDirection


translatedBy: Vector2d -> Frame2d -> Frame2d
translatedBy vector frame =
  { frame | originPoint = Point2d.plus vector frame.originPoint }


mirroredAbout: Axis2d -> Frame2d -> Frame2d
mirroredAbout axis =
  let
    mirrorPoint = Point2d.mirroredAbout axis
    mirrorDirection = Direction2d.mirroredAbout axis.direction
  in
    \frame ->
      let
        originPoint = mirrorPoint frame.originPoint
        xDirection = mirrorDirection frame.xDirection
        yDirection = mirrorDirection frame.yDirection
      in
        Frame2d originPoint xDirection yDirection


relativeTo: Frame2d -> Frame2d -> Frame2d
relativeTo otherFrame =
  let
    localizePoint = Point2d.relativeTo otherFrame
    localizeDirection = Direction2d.relativeTo otherFrame
  in
    \frame ->
      let
        originPoint = localizePoint frame.originPoint
        xDirection = localizeDirection frame.xDirection
        yDirection = localizeDirection frame.yDirection
      in
        Frame2d originPoint xDirection yDirection


placedIn: Frame2d -> Frame2d -> Frame2d
placedIn frame =
  let
    globalizePoint = Point2d.placedIn frame
    globalizeDirection = Direction2d.placedIn frame
  in
    \plane ->
      let
        originPoint = globalizePoint plane.originPoint
        xDirection = globalizeDirection plane.xDirection
        yDirection = globalizeDirection plane.yDirection
      in
        Frame2d originPoint xDirection yDirection
