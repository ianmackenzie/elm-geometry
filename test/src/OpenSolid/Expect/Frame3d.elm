module OpenSolid.Expect.Frame3d exposing (frame3d, frame3dWithin)

import Expect exposing (Expectation)
import OpenSolid.Types exposing (..)
import OpenSolid.Compare.Frame3d as Compare
import OpenSolid.Expect as Expect


frame3d : Frame3d -> Frame3d -> Expectation
frame3d =
    Expect.by Compare.frame3d


frame3dWithin : Float -> Frame3d -> Frame3d -> Expectation
frame3dWithin =
    Expect.by << Compare.frame3dWithin
