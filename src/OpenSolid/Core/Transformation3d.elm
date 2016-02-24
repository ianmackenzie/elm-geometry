module OpenSolid.Core.Transformation3d
  ( translationBy
  , rotationAbout
  , localizationTo
  , globalizationFrom
  , andThen
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Components3d as Components3d
import OpenSolid.Core.Matrix3x3 as Matrix3x3


translationBy: Vector3d -> Transformation3d
translationBy vector =
  Transformation3d identity (Components3d.plus vector)


rotateVector: (Direction3d, Direction3d, Direction3d) -> Vector3d -> Vector3d
rotateVector (xDirection, yDirection, zDirection) =
  Matrix3x3.product xDirection yDirection zDirection


rotatePoint: Point3d -> (Direction3d, Direction3d, Direction3d) -> Point3d -> Point3d
rotatePoint centerPoint basis point =
  let
    rotatedVector = rotateVector basis (Components3d.minus centerPoint point)
  in
    Components3d.plus rotatedVector centerPoint


rotationBasis: Direction3d -> Float -> (Direction3d, Direction3d, Direction3d)
rotationBasis direction angle =
  let
    halfAngle = 0.5 * angle
    sinHalfAngle = sin halfAngle
    x = direction.x * sinHalfAngle
    y = direction.y * sinHalfAngle
    z = direction.z * sinHalfAngle
    w = cos halfAngle
    wx = w * x
    wy = w * y
    wz = w * z
    xx = x * x
    xy = x * y
    xz = x * z
    yy = y * y
    yz = y * z
    zz = z * z
    xDirection = Direction3d (1 - 2 * (yy + zz)) (2 * (xy + wz)) (2 * (xz - wy))
    yDirection = Direction3d (2 * (xy - wz)) (1 - 2 * (xx + zz)) (2 * (yz + wx))
    zDirection = Direction3d (2 * (xz + wy)) (2 * (yz - wx)) (1 - 2 * (xx + yy))
  in
    (xDirection, yDirection, zDirection)


rotationAbout: Axis3d -> Float -> Transformation3d
rotationAbout axis angle =
  let
    basis = rotationBasis axis.direction angle
  in
    Transformation3d (rotateVector basis) (rotatePoint axis.originPoint basis)


localizationTo: Frame3d -> Transformation3d
localizationTo frame =
  let
    transformVector = Matrix3x3.dotProduct frame.xDirection frame.yDirection frame.zDirection
    transformPoint = Components3d.minus frame.originPoint >> transformVector
  in
    Transformation3d transformVector transformPoint


globalizationFrom: Frame3d -> Transformation3d
globalizationFrom frame =
  let
    transformVector = Matrix3x3.product frame.xDirection frame.yDirection frame.zDirection
    transformPoint = transformVector >> Components3d.plus frame.originPoint
  in
    Transformation3d transformVector transformPoint


andThen: Transformation3d -> Transformation3d -> Transformation3d
andThen second first =
  let
    transformVector = first.transformVector >> second.transformVector
    transformPoint = first.transformPoint >> second.transformPoint
  in
    Transformation3d transformVector transformPoint
