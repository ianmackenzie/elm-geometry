module OpenSolid.Expect.Frame2d exposing (frame2d, frame2dWithin)

import Expect exposing (Expectation)
import OpenSolid.Types exposing (..)
import OpenSolid.Compare.Frame2d as Compare
import OpenSolid.Expect as Expect


frame2d : Frame2d -> Frame2d -> Expectation
frame2d =
    Expect.by Compare.frame2d


frame2dWithin : Float -> Frame2d -> Frame2d -> Expectation
frame2dWithin =
    Expect.by << Compare.frame2dWithin
