module OpenSolid.Arc3d
    exposing
        ( throughPoints
        , axis
        , centerPoint
        , radius
        , startPoint
        , endPoint
        , sweptAngle
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Arc2d as Arc2d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.SketchPlane3d as SketchPlane3d


throughPoints : Point3d -> Point3d -> Point3d -> Maybe Arc3d
throughPoints firstPoint secondPoint thirdPoint =
    SketchPlane3d.throughPoints firstPoint secondPoint thirdPoint
        |> Maybe.andThen
            (\sketchPlane ->
                let
                    project =
                        Point3d.projectInto sketchPlane
                in
                    Arc2d.throughPoints
                        (project firstPoint)
                        (project secondPoint)
                        (project thirdPoint)
                        |> Maybe.map (Arc2d.placeOnto sketchPlane)
            )


axis : Arc3d -> Axis3d
axis (Arc3d properties) =
    properties.axis


centerPoint : Arc3d -> Point3d
centerPoint arc =
    Point3d.projectRadiallyOnto (axis arc) (startPoint arc)


radius : Arc3d -> Float
radius arc =
    Point3d.radialDistanceFrom (axis arc) (startPoint arc)


startPoint : Arc3d -> Point3d
startPoint (Arc3d properties) =
    properties.startPoint


endPoint : Arc3d -> Point3d
endPoint arc =
    Point3d.rotateAround (axis arc) (sweptAngle arc) (startPoint arc)


sweptAngle : Arc3d -> Float
sweptAngle (Arc3d properties) =
    properties.sweptAngle


scaleAbout : Point3d -> Float -> Arc3d -> Arc3d
scaleAbout point scale arc =
    let
        scalePoint =
            Point3d.scaleAbout point scale

        currentAxis =
            axis arc

        scaledOrigin =
            scalePoint (Axis3d.originPoint currentAxis)

        currentAxisDirection =
            Axis3d.direction currentAxis

        scaledDirection =
            if scale < 0.0 then
                Direction3d.flip currentAxisDirection
            else
                currentAxisDirection

        scaledAxis =
            Axis3d { originPoint = scaledOrigin, direction = scaledDirection }
    in
        Arc3d
            { axis = scaledAxis
            , startPoint = scalePoint (startPoint arc)
            , sweptAngle = sweptAngle arc
            }


rotateAround : Axis3d -> Float -> Arc3d -> Arc3d
rotateAround rotationAxis angle =
    let
        rotateAxis =
            Axis3d.rotateAround rotationAxis angle

        rotatePoint =
            Point3d.rotateAround rotationAxis angle
    in
        \arc ->
            Arc3d
                { axis = rotateAxis (axis arc)
                , startPoint = rotatePoint (startPoint arc)
                , sweptAngle = sweptAngle arc
                }


translateBy : Vector3d -> Arc3d -> Arc3d
translateBy displacement arc =
    Arc3d
        { axis = Axis3d.translateBy displacement (axis arc)
        , startPoint = Point3d.translateBy displacement (startPoint arc)
        , sweptAngle = sweptAngle arc
        }


mirrorAcross : Plane3d -> Arc3d -> Arc3d
mirrorAcross plane =
    let
        mirrorAxis =
            Axis3d.mirrorAcross plane

        mirrorPoint =
            Point3d.mirrorAcross plane
    in
        \arc ->
            Arc3d
                { axis = mirrorAxis (axis arc)
                , startPoint = mirrorPoint (startPoint arc)
                , sweptAngle = -(sweptAngle arc)
                }


relativeTo : Frame3d -> Arc3d -> Arc3d
relativeTo frame arc =
    Arc3d
        { axis = Axis3d.relativeTo frame (axis arc)
        , startPoint = Point3d.relativeTo frame (startPoint arc)
        , sweptAngle =
            if Frame3d.isRightHanded frame then
                (sweptAngle arc)
            else
                -(sweptAngle arc)
        }


placeIn : Frame3d -> Arc3d -> Arc3d
placeIn frame arc =
    Arc3d
        { axis = Axis3d.placeIn frame (axis arc)
        , startPoint = Point3d.placeIn frame (startPoint arc)
        , sweptAngle =
            if Frame3d.isRightHanded frame then
                (sweptAngle arc)
            else
                -(sweptAngle arc)
        }
