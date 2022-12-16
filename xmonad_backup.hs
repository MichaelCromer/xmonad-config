import XMonad hiding ( (|||) )

import XMonad.Layout.Gaps
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed

main = xmonad defaultConfig
        { modMask = mod4Mask -- Use Super instead of Alt
        , terminal = "urxvt"
        -- more changes
        }

myConfig = def {
      borderWidth       = border
    , clickJustFocuses  = myClickJustFocuses
    , focusFollowsMouse = myFocusFollowsMouse
    , normalBorderColor = myNormalBorderColor
    --, manageHook        = myManageHook
    --, handleEventHook   = myHandleEventHook
    , layoutHook        = myLayoutHook
    , modMask           = myModMask
    --, mouseBindings     = myMouseBindings
    --, startupHook       = myStartupHook
    , terminal          = myTerminal
    , workspaces        = myWorkspaces
}

------------------------------------------------------------
-- Bindings and Presets
------------------------------------------------------------

myModMask           = mod4Mask
myFocusFollowsMouse = False
myClickJustFocuses  = True
myTerminal          = "urxvt"

------------------------------------------------------------
-- Themes
------------------------------------------------------------

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

-- sizes
gap         = 10
topbar      = 10
border      = 0
prompt      = 20
status      = 20

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

myFont      = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"
myBigFont   = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
}

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
}

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
}

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
}

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
}

myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.5
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
}

------------------------------------------------------------
-- Workspaces
------------------------------------------------------------

ws0 = "0"
ws1 = "1"
ws2 = "2"
ws3 = "3"
ws4 = "4"
ws5 = "5"
ws6 = "6"
ws7 = "7"
ws8 = "8"
ws9 = "9"

myWorkspaces = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0] 

------------------------------------------------------------
-- Layouts
------------------------------------------------------------

myLayoutHook    = showWorkspaceName
                $ fullScreenToggle
                $ fullBarToggle
                $ mirrorToggle
                $ reflectToggle
                $ flex ||| tabs
    where
        fullScreenToggle    = mkToggle (single FULL)
        fullBarToggle       = mkToggle (single FULLBAR)
        mirrorToggle        = mkToggle (single MIRROR)
        reflectToggle       = mkToggle (single REFLECTX)
        smallMonResWidth    = 1920
        showWorkspaceName   = showName' myShowWNameTheme

        named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
        trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                               (XMonad.Layout.Renamed.PrependWords n)]
        suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
        trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                               (XMonad.Layout.Renamed.AppendWords n)]

        addTopBar           = noFrillsDeco shrinkText topBarTheme

        mySpacing           = spacing gap
        sGap                = quot gap 2
        myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
        mySmallGaps         = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
        myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

        threeCol = named "Unflexed"
            $ avoidStruts
            $ addTopBar
            $ myGaps
            $ mySpacing
            $ ThreeColMid 1 (1/10) (1/2)
        
        tabs = named "Tabs"
            $ avoidStruts
            $ addTopBar
            $ addTabs shrinkText myTabTheme
            $ Simplest

