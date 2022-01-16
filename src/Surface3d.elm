module Surface3d exposing
    ( Surface3d
    , triangle, rectangle, circle, ellipse, extrusion, revolution, planar
    , flip
    --, translateBy, scaleAbout, rotateAround, mirrorAcross
    -- , placeIn, relativeTo
    -- , toMesh
    )

{-|

@docs Surface3d

@docs triangle, rectangle, circle, ellipse, extrusion, revolution, planar

@docs toMesh

@docs flip, translateBy, scaleAbout, rotateAround, mirrorAcross

@docs placeIn, relativeTo

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Circle3d exposing (Circle3d)
import Curve3d exposing (Curve3d)
import Ellipse3d exposing (Ellipse3d)
import Frame2d
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Rectangle3d exposing (Rectangle3d)
import Region2d exposing (Region2d)
import SketchPlane3d exposing (SketchPlane3d)
import Triangle3d exposing (Triangle3d)
import Vector3d exposing (Vector3d)


type alias Surface3d units coordinates =
    Types.Surface3d units coordinates


triangle : Triangle3d units coordinates -> Surface3d units coordinates
triangle =
    Types.TriangularSurface Types.RightHandedSurface


rectangle : Rectangle3d units coordinates -> Surface3d units coordinates
rectangle =
    Types.RectangularSurface Types.RightHandedSurface


circle : Circle3d units coordinates -> Surface3d units coordinates
circle =
    Types.CircularSurface


ellipse : Ellipse3d units coordinates -> Surface3d units coordinates
ellipse =
    Types.EllipticalSurface Types.RightHandedSurface


extrusion : Curve3d units coordinates -> Vector3d units coordinates -> Surface3d units coordinates
extrusion =
    Types.ExtrusionSurface Types.RightHandedSurface


revolution : Curve3d units coordinates -> Axis3d units coordinates -> Angle -> Surface3d units coordinates
revolution =
    Types.RevolutionSurface Types.RightHandedSurface


planar : Region2d units sketchCoordinates -> SketchPlane3d units coordinates { defines : sketchCoordinates } -> Surface3d units coordinates
planar givenRegion givenSketchPlane =
    Types.PlanarSurface Types.RightHandedSurface
        (givenRegion |> Region2d.placeIn Frame2d.atOrigin)
        (Frame2d.atOrigin |> SketchPlane3d.on givenSketchPlane)


toggle : Types.SurfaceHandedness -> Types.SurfaceHandedness
toggle givenHandedness =
    case givenHandedness of
        Types.RightHandedSurface ->
            Types.LeftHandedSurface

        Types.LeftHandedSurface ->
            Types.RightHandedSurface


flip : Surface3d units coordinates -> Surface3d units coordinates
flip givenSurface =
    case givenSurface of
        Types.TriangularSurface handedness givenTriangle ->
            Types.TriangularSurface (toggle handedness) givenTriangle

        Types.RectangularSurface handedness givenRectangle ->
            Types.RectangularSurface (toggle handedness) givenRectangle

        Types.CircularSurface givenCircle ->
            Types.CircularSurface (Circle3d.flip givenCircle)

        Types.EllipticalSurface handedness givenEllipse ->
            Types.EllipticalSurface (toggle handedness) givenEllipse

        Types.ExtrusionSurface handedness profile displacement ->
            Types.ExtrusionSurface (toggle handedness) profile displacement

        Types.RevolutionSurface handedness profile axis angle ->
            Types.RevolutionSurface (toggle handedness) profile axis angle

        Types.PlanarSurface handedness region sketchPlane ->
            Types.PlanarSurface (toggle handedness) region sketchPlane
