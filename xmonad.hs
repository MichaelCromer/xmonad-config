{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
---------------------------------------------------------------------------
--                                                                       --
--     _|      _|  _|      _|                                      _|    --
--       _|  _|    _|_|  _|_|    _|_|    _|_|_|      _|_|_|    _|_|_|    --
--         _|      _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--       _|  _|    _|      _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--     _|      _|  _|      _|    _|_|    _|    _|    _|_|_|    _|_|_|    --
--                                                                       --
---------------------------------------------------------------------------

{-

    Michael Cromer's XMonad config
        based on 
            - https://xmonad.org/TUTORIAL.html
            - https://github.com/altercation/dotfiles-tilingwm
-}

---------------------------------------------------------------------------

------------------------------------------------------------------------}}}
-- Imports                                                              {{{
---------------------------------------------------------------------------
import Data.List
import qualified Data.Map as M
import System.Exit
import System.IO                            -- for xmonbar

import XMonad hiding ( (|||) )              -- ||| from X.L.LayoutCombinators
import qualified XMonad.StackSet as W       -- myManageHookShift

import XMonad.Actions.ConditionalKeys       -- bindings per workspace or layout
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatSnap
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote               -- promote window to master
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll               -- action all the things

import XMonad.Config.Prime (X)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks             -- avoid xmobar
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing                -- this makes smart space around windows
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import XMonad.Prompt                        -- to get my old key bindings working
import XMonad.Prompt.ConfirmPrompt          -- don't just hard quit

import XMonad.Util.Cursor
import XMonad.Util.EZConfig                 -- removeKeys, additionalKeys
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Paste as P               -- testing
import XMonad.Util.Run                      -- for spawnPipe and hPutStrLn
import XMonad.Util.WorkspaceCompare         -- custom WS functions filtering NSP

import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen

------------------------------------------------------------------------}}}
-- Main                                                                 {{{
---------------------------------------------------------------------------

main = do
    xmonad 
        $ withNavigation2DConfig myNav2DConf
        $ withUrgencyHook LibNotifyUrgencyHook
        $ ewmh
        $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
        $ myConfig 

------------------------------------------------------------------------}}}
-- Configuration                                                        {{{
---------------------------------------------------------------------------

myConfig = def
        { borderWidth        = border
        , clickJustFocuses   = myClickJustFocuses
        , focusFollowsMouse  = myFocusFollowsMouse
        , normalBorderColor  = myNormalBorderColour
        , focusedBorderColor = myFocusedBorderColour
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , modMask            = myModMask
        , mouseBindings      = myMouseBindings
        , startupHook        = myStartupHook
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        }

myClickJustFocuses  = True
myFocusFollowsMouse = False
myModMask           = mod4Mask -- super (and on my system, hyper) keys

myTerminal          = "gnome-terminal"
myAltTerminal       = "cool-retro-term"
myBrowser           = "qutebrowser"
myLauncher          = "dmenu_run"
myLatexEditor       = "texstudio"
myTextEditor        = "vim"

myWorkspaces = map show [1..9]

------------------------------------------------------------------------}}}
-- Theme                                                                {{{
---------------------------------------------------------------------------

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
ocelott = "#fdcc1b"
lime    = "#11CC11"

-- sizes
gap         = 5
topbar      = 10
border      = 0
prompt      = 10
status      = 10

myNormalBorderColour     = "#000000"
myFocusedBorderColour    = active

active      = lime
activeWarn  = red
inactive    = base02
focusColour  = blue
unfocusColour = base02

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

------------------------------------------------------------------------}}}
-- Layouts                                                              {{{
--
-- WARNING: WORK IN PROGRESS AND A LITTLE MESSY
---------------------------------------------------------------------------

-- Tell X.A.Navigation2D about specific layouts and how to handle them

myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full",          centerNavigation)
    -- line/center same results   ,("Simple Tabs", lineNavigation)
    --                            ,("Simple Tabs", centerNavigation)
                                  ]
    , unmappedWindowRect        = [("Full", singleWindowRect)
    -- works but breaks tab deco  ,("Simple Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Simple Tabs", fullScreenRect)
                                  ]
    }


data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)

-- tabBarFull = avoidStruts $ noFrillsDeco shrinkText topBarTheme $ addTabs shrinkText myTabTheme $ Simplest
barFull = avoidStruts $ Simplest

-- cf http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Droundy.html

myLayoutHook = showWorkspaceName
             $ fullScreenToggle
             $ fullBarToggle
             $ mirrorToggle
             $ reflectToggle
             $ flex ||| tabs
  where

--    testTall = Tall 1 (1/50) (2/3)
--    myTall = subLayout [] Simplest $ trackFloating (Tall 1 (1/20) (1/2))

    fullBarToggle       = mkToggle (single FULLBAR)
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    smallMonResWidth    = 1920
    showWorkspaceName   = showWName' myShowWNameTheme

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

    --------------------------------------------------------------------------
    -- Tabs Layout                                                          --
    --------------------------------------------------------------------------

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

    -----------------------------------------------------------------------
    -- Flexi SubLayouts                                                  --
    -----------------------------------------------------------------------
    --
    -- In many ways the best solution. Acts like ThreeColumns, Tall, BSP,
    -- or any other container layout style. Can use this layout just as you
    -- would those without tabs at all, or you can easily merge any windows
    -- into a tabbed group.
    --
    -- Diagrams:
    --
    -- (examples only... this is a very flexible layout and as such the
    -- layout style and arrangement isn't limited as much as the other
    -- attempts below)
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |   Tabs   |
    -- |          |                    |          |
    -- |----------|       Master       |----------|
    -- |          |                    |          |
    -- |   Tabs   |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |----------|
    -- |                    |          |
    -- |                    |   Tabs   |
    -- |                    |          |
    -- ---------------------------------
    --
    --
    -- Advantages
    --
    --   * tab group is movable as a unit and acts like any other window
    --
    --   * this is the "cleanest" of the dynamic layouts I've worked with
    --     and leaves no "pixel dust" on the screen when switching to a WS
    --     on a different monitor
    --
    --   * navigation and window/group movement is trivial with
    --     X.A.Navigation2D
    --
    --   * master window remains master when switching screens (unlike
    --     the "X.L.Master" based solution below)
    --
    --   * unlike some of the other solutions, it is trivial to change
    --     the exterior layout format and so I could potentially add in
    --     some layout change to BSP or other layout that I want to test
    --     while still retaining the tab functionality
    --
    -- Disadvantages
    --
    --   * layout starts without any tabs (could be considered a feature
    --     since in that case the layout performs exactly as the parent/
    --     container layout does)
    --
    --   * To move a window into or out of the tabbed group requires
    --     special key bindings unique to X.L.SubLayouts
    --
    --  Understanding XMonad.Layouts.SubLayouts
    --
    --  It took me a while to grok this.
    --
    --  the subLayout hook is used with the following format:
    --
    --    subLayout advanceInnerLayouts innerLayout outerLayout
    --
    --  It works like this: subLayout modifies an entire other layout (or
    --  layouts), enabling you to turn what would be a normal window into
    --  a little group of windows managed by an entirely different layout.
    --
    --  In my case, I'm using layouts like "Three Column" and "Tall" as the
    --  nominal "container" layout (what SubLayouts calls the "outerLayout").
    --
    --  The "inner layout" in my case is just "Simplest". I'm also adding tabs
    --  which are only applied to my sublayouts. Not sure how that works
    --  but it's apparent from the X.L.SubLayouts documentation that this is
    --  the intended use/behavior. Essential X.L.SubLayouts is hijacking these
    --  added tabs and applying them just to the Simplest layout, and then that
    --  in turn is stuck inside the rectangle that would normally hold a window
    --  in my normal layouts.
    --
    --  One of the confusing things for me at first was that the layout doesn't
    --  start with any subLayouts. So it appears to just be a normal layout.
    --  You have to "merge all" to suck everything up into a Simplest tabbed
    --  group and then you can add other windows normally and you'll
    --  have a sublayout with tabs.
    --
    --  Note: subLayouts has some other features. For example, you can give it
    --  a list of layouts to work through and it will advance through them in
    --  series (or possibly in an order your provide) and will apply different
    --  layouts to different subLayout groups. Each time you add a new window
    --  to your layout, it acquires the sublayout, even if you don't know it.
    --
    --  In my case, my list is one long and is just the first window I add.
    --
    --  Ex. The second group is Tall, the third is Circle, all others are
    --  tabbed with:
    --
    --  myLayout = addTabs shrinkText def
    --           $ subLayout [0,1,2] (Simplest ||| Tall 1 0.2 0.5 ||| Circle)
    --                    $ Tall 1 0.2 0.5 ||| Full
   
    -- this is a flexible sublayout layout that has only one container
    -- layout style (depending on screen)
    --     flexiSub = named "Flexi SubLayouts"
    --               $ avoidStruts
    --               $ windowNavigation
    --               $ addTopBar
    --               $ myGaps
    --               $ addTabs shrinkText myTabTheme
    --               $ mySpacing
    --               $ subLayout [] Simplest
    --               $ ifWider smallMonResWidth wideLayout standardLayout
    --               where
    --                   wideLayout = ThreeColMid 1 (1/100) (1/2)
    --                   standardLayout = ResizableTall 1 (1/50) (2/3) []

    -- retained during development: safe to remove later

    flex = trimNamed 5 "Flex"
              $ avoidStruts
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTopBar
              $ addTabs shrinkText myTabTheme
              -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts = myGaps $ mySpacing
                      $ (suffixed "Wide 3Col" $ ThreeColMid 1 (1/20) (1/2))
                    ||| (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)
                  --  ||| fullTabs
                  standardLayouts = myGaps $ mySpacing
                      $ (suffixed "Std 2/3" $ ResizableTall 1 (1/20) (2/3) [])
                    ||| (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])

                  --  ||| fullTabs
                  --fullTabs = suffixed "Tabs Full" $ Simplest
                  --
                  -- NOTE: removed this from the two (wide/std) sublayout
                  -- sequences. if inside the ifWider, the ||| combinator
                  -- from X.L.LayoutCombinators can't jump to it directly (
                  -- or I'm doing something wrong, either way, it's simpler
                  -- to solve it by just using a tabbed layout in the main
                  -- layoutHook). The disadvantage is that I lose the "per
                  -- screen" memory of which layout was where if using the
                  -- tabbed layout (if using the the ifWider construct as
                  -- I am currently, it seems to work fine)
                  --
                  -- Using "Full" here (instead of Simplest) will retain the
                  -- tabbed sublayout structure and allow paging through each
                  -- group/window in full screen mode. However my preference
                  -- is to just see all the windows as tabs immediately.  
                  -- Using "Simplest" here will do this: display all windows
                  -- as tabs across the top, no "paging" required. However
                  -- this is misleading as the sublayouts are of course still
                  -- there and you will have to use the nornmal W.focusUp/Down
                  -- to successfully flip through them. Despite this
                  -- limitation I prefer this to the results with "Full".

{-|
    -----------------------------------------------------------------------
    -- Simple Flexi                                                      --
    -----------------------------------------------------------------------
    --
    -- Simple dynamically resizing layout as with the other variations in
    -- this config. This layout has not tabs in it and simply uses
    -- Resizable Tall and Three Column layouts.
    simpleFlexi = named "Simple Flexible"
              $ ifWider smallMonResWidth simpleThree simpleTall
    simpleTall = named "Tall"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ ResizableTall 1 (1/300) (2/3) []
              
    simpleThree = named "Three Col"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ ThreeColMid 1 (3/100) (1/2)
    -----------------------------------------------------------------------
    -- Other Misc Layouts                                                --
    -----------------------------------------------------------------------
    --
    --
    masterTabbedP   = named "MASTER TABBED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ mastered (1/100) (1/2) $ tabbed shrinkText myTabTheme
    bsp       = named "BSP"
              $ borderResize (avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ emptyBSP )
              -- $ borderResize (emptyBSP)
    oneBig    = named "1BG"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ OneBig (3/4) (3/4)
    tiledP    = named "TILED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ consoleOn
              $ tiled'
    oneUp =   named "1UP"
              $ avoidStruts
              $ myGaps
              $ combineTwoP (ThreeCol 1 (3/100) (1/2))
                            (Simplest)
                            (Tall 1 0.03 0.5)
                            (ClassName "Google-chrome-beta")
    -----------------------------------------------------------------------
    -- Master-Tabbed Dymamic                                             --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.Master
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |  Master  |       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \____________________ _____________________/
    --                      '
    --                 all one layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- ---------------------------------
    -- \_______________ _______________/
    --                 '
    --            all one layout
    --
    -- Advantages to this use of X.L.Master to created this dynamic
    -- layout include:
    --
    --   * No fussing with special keys to swap windows between the
    --     Tabs and Master zones
    --
    --   * Window movement and resizing is very straightforward
    --
    --   * Limited need to maintain a mental-map of the layout
    --     (pretty easy to understand... it's just a layout)
    --
    -- Disadvantages include:
    --
    --   * Swapping a window from tabbed area will of necessity swap
    --     one of the Master windows back into tabs (since there can
    --     only be two master windows)
    --
    --   * Master area can have only one/two windows in std/wide modes
    --     respectively
    --
    --   * When switching from wide to standard, the leftmost pane
    --     (which is visually secondary to the large central master
    --     window) becomes the new dominant master window on the
    --     standard display (this is easy enough to deal with but
    --     is a non-intuitive effect)
    masterTabbedDynamic = named "Master-Tabbed Dynamic"
              $ ifWider smallMonResWidth masterTabbedWide masterTabbedStd
    masterTabbedStd = named "Master-Tabbed Standard"
              $ addTopBar
              $ avoidStruts
              $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
              $ mastered (1/100) (2/3)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ tabbed shrinkText myTabTheme
    masterTabbedWide = named "Master-Tabbed Wide"
              $ addTopBar
              $ avoidStruts
              $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
              $ mastered (1/100) (1/4)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ mastered (1/100) (2/3)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ tabbed shrinkText myTabTheme
    -----------------------------------------------------------------------
    -- Tall-Tabbed Dymamic                                               --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.ComboP
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |----------|       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \______________ _______________/\____ _____/
    --                '                     '
    --        this set of panes is      This is a
    --        its' own layout in a      separate
    --        Tall configuration        tab format
    --                                  layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |--------------------|          |
    -- |         |          |          |
    -- ---------------------------------
    -- \_________ _________/\____ _____/
    --           '               '
    -- this set of panes is  This is a
    -- its' own layout in a  separate
    -- Tall configuration    tab format
    --                       layout
    --
    -- Advantages to this use of ComboP to created this dynamic
    -- layout include:
    --
    --   * the center Master stays the same when the layout
    --     changes (unlike the X.L.Master based dyn. layout)
    --
    --   * the Master can have a set of panes under it on the
    --     small screen (standard) layout
    --
    --   * on ultrawide the leftmost pane may be divided into
    --     multiple windows
    --
    --   * possible to toss a tabbed window to the "Master" area
    --     without swapping a window back into tabs
    --
    --   * use of ComboP allows redirection windows to either
    --     left or right section
    --
    -- Disadvantages include:
    --
    --   * normal window swaps fail between the two separate
    --     layouts. There must be a special swap-between-layouts
    --     binding (normal window NAVIGATION works, at least using
    --     X.A.Navigation2D).
    --
    --   * switching between screens can leave title bar clutter
    --     that hasn't been cleaned up properly (restarting
    --     XMonad works to clean this up, but that's hacky)
    --
    --   * somewhat greater need to maintain a mental-map of the
    --     layout (you need to have a sense for the windows being
    --     in separate sections of the different layouts)
    smartTallTabbed = named "Smart Tall-Tabbed"
            $ avoidStruts
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
            wideScreen   = combineTwoP (TwoPane 0.03 (3/4))
                           (smartTall)
                           (smartTabbed)
                           (ClassName "Google-chrome-beta")
            normalScreen = combineTwoP (TwoPane 0.03 (2/3))
                           (smartTall)
                           (smartTabbed)
                           (ClassName "Google-chrome-beta")
    smartTall = named "Smart Tall"
            $ addTopBar
        $ mySpacing
            $ myGaps
        $ boringAuto
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
                wideScreen = reflectHoriz $ Tall 1 0.03 (2/3)
                normalScreen = Mirror $ Tall 1 0.03 (4/5)
    smartTabbed = named "Smart Tabbed"
              $ addTopBar
              $ myCustomGaps
              $ tabbed shrinkText myTabTheme
-}
    -----------------------------------------------------------------------
    -- Flexi Combinators                                                 --
    -----------------------------------------------------------------------
    --
    -- failed attempt. creates a nice looking layout but I'm not sure
    -- how to actually direct tabs to the tabbed area
    --
    --     flexiCombinators = named "Flexi Combinators"
    --             $ avoidStruts
    --             $ ifWider smallMonResWidth wideScreen normalScreen
    --             where
    --             wideScreen   = smartTall ****||* smartTabbed
    --             normalScreen = smartTall ***||** smartTabbed




---------------------------------------------------------------------------
-- Keybindings     
---------------------------------------------------------------------------

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- some of the structure of the following cribbed from 
-- https://github.com/SimSaladin/configs/blob/master/.xmonad/xmonad.hs
-- https://github.com/paul-axe/dotfiles/blob/master/.xmonad/xmonad.hs
-- https://github.com/pjones/xmonadrc (+ all the dyn project stuff)

wsKeys = map show $ [1..9] ++ [0]

-- hidden, non-empty workspaces 
nextNonEmptyWS = findWorkspace getSortByIndex Next HiddenNonEmptyWS 1
        -- >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndex Prev HiddenNonEmptyWS 1
        -- >>= \t -> (windows . W.view $ t)

nextEmptyWS    = findWorkspace getSortByIndex Next EmptyWS 1
prevEmptyWS    = findWorkspace getSortByIndex Prev EmptyWS 1

myShiftTo      = (>>= (windows . W.shift))
myMoveTo       = (>>= (windows . W.view))
myForceMoveTo  = (>>= (windows . W.greedyView))
myFollowTo     = (>>= (windows . (\w -> W.greedyView w . W.shift w)))

testFollowFunction = myFollowTo nextEmptyWS

myKeys conf = let
    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    dirKeys   = ["j","k","h","l"]
    arrowKeys = ["<D>","<U>","<L>","<R>"]
    dirs      = [ D,  U,  L,  R ]

    zipM  m nm ks as f   = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    -- try sending one message, fallback if unreceived, then refresh
    tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                   then W.sink w s
                                   else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s)
                            )

    in

    -----------------------------------------------------------------------
    -- System / Utilities
    -----------------------------------------------------------------------
    subKeys "System"
    [ ("M-q"                    , addName "Rebuild & restart XMonad"        $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q"                  , addName "Quit XMonad"                     $ confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
    , ("M-S-z"                  , addName "Lock screen"                     $ spawn "xscreensaver-command -lock")
    ] ^++^

    -----------------------------------------------------------------------
    -- Launchers
    -----------------------------------------------------------------------
    subKeys "Launchers"
    [ ("M-<Space>"              , addName "Launcher"                        $ spawn myLauncher)
    , ("M-<Return>"             , addName "Terminal"                        $ spawn myTerminal)
    , ("M-\\"                   , addName "Browser"                         $ spawn myBrowser)
    , ("M-t"                    , addName "Latex"                           $ spawn myLatexEditor)
    ] ^++^

    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------

    subKeys "Windows" (
     [ ("M-<Backspace>"         , addName "Kill"                              kill1)
     , ("M-S-<Backspace>"       , addName "Kill all"                        $ confirmPrompt hotPromptTheme "kill all" $ killAll)
     , ("M-p"                   , addName "Hide window to stack"            $ withFocused hideWindow)
     , ("M-S-p"                 , addName "Restore hidden window (FIFO)"    $ popOldestHiddenWindow)
     , ("M-S-m"                 , addName "Promote"                         $ promote) 
     , ("M-S-g"                 , addName "Un-merge from sublayout"         $ withFocused (sendMessage . UnMerge))
     , ("M-g"                   , addName "Merge all into sublayout"        $ withFocused (sendMessage . MergeAll))
     , ("M-u"                   , addName "Focus urgent"                      focusUrgent)
     , ("M-m"                   , addName "Focus master"                    $ windows W.focusMaster)
     , ("M-<Tab>"               , addName "Navigate tabs D"                 $ bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
     , ("M-S-<Tab>"             , addName "Navigate tabs U"                 $ bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])
     ]
     ++ zipM' "M-"                "Navigate window"                           dirKeys dirs windowGo True
     ++ zipM' "M-S-"              "Move window"                               dirKeys dirs windowSwap True
     ++ zipM  "M-C-"              "Merge w/sublayout"                         dirKeys dirs (sendMessage . pullGroup)
     ++ zipM' "M-"                "Navigate screen"                           arrowKeys dirs screenGo True
     ++ zipM' "M-S-"              "Swap workspace to screen"                  arrowKeys dirs screenSwap True
     ) ^++^

     -----------------------------------------------------------------------
     -- Workspaces & Projects
     -----------------------------------------------------------------------

     subKeys "Workspaces & Projects" (
      [ ("M-n"                  , addName "Next empty workspace"            $ moveTo Next EmptyWS)
      , ("M-S-n"                , addName "Shift to next empty workspace"   $ myFollowTo nextEmptyWS)
      , ("M-."                  , addName "Move to next non-empty WS"       $ myMoveTo nextNonEmptyWS)
      , ("M-,"                  , addName "Move to prev non-empty WS"       $ myMoveTo prevNonEmptyWS)
      , ("M-a"                  , addName "Toggle last workspace"           $ toggleWS)
      , ("M-S-w"                , addName "Swap screens"                    $ swapNextScreen)
      , ("M-w"                  , addName "Focus other screen"              $ nextScreen)
      ]
      ++ zipM "M-"                "View      ws"                              wsKeys [0..] (withNthWorkspace W.view)
      ++ zipM "M-S-"              "Move w to ws"                              wsKeys [0..] (withNthWorkspace W.shift)
      ++ zipM "M-S-C-"            "Copy w to ws"                              wsKeys [0..] (withNthWorkspace copy)
     ) ^++^

     -----------------------------------------------------------------------
     -- Layouts & Sublayouts
     -----------------------------------------------------------------------

     subKeys "Layout Management"
     [ ("M-="                   , addName "Cycle all layouts"               $ sendMessage NextLayout)
     , ("M-C-="                 , addName "Cycle sublayout"                 $ toSubl NextLayout)
     , ("M-S-="                 , addName "Reset layout"                    $ setLayout $ XMonad.layoutHook conf)
     , ("M-o"                   , addName "Float tiled w"                   $ withFocused toggleFloat)
     , ("M-S-o"                 , addName "Tile all floating w"             $ sinkAll)
     , ("M-r"                   , addName "Reflect/Rotate"                  $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle REFLECTX))
     , ("M-S-r"                 , addName "Force Reflect (even on BSP)"     $ sendMessage (XMonad.Layout.MultiToggle.Toggle REFLECTX))
     , ("M-f"                   , addName "Fullscreen"                      $ sequence_ [ (withFocused $ windows . W.sink)
                                                                                        , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) 
                                                                                        ])
     , ("M-S-f"                  , addName "Fake fullscreen"                $ sequence_ [ (P.sendKey P.noModMask xK_F11)
                                                                                        , (tryMsgR (ExpandTowards L) (Shrink))
                                                                                        , (tryMsgR (ExpandTowards R) (Expand)) 
                                                                                        ])
     ] ^++^

    -----------------------------------------------------------------------
    -- Resizing
    -----------------------------------------------------------------------

    subKeys "Resize"
    [ ("M-["                    , addName "Expand (L on BSP)"               $ tryMsgR (ExpandTowards L) (Shrink))
    , ("M-]"                    , addName "Expand (R on BSP)"               $ tryMsgR (ExpandTowards R) (Expand))
    , ("M-S-["                  , addName "Expand (U on BSP)"               $ tryMsgR (ExpandTowards U) (MirrorShrink))
    , ("M-S-]"                  , addName "Expand (D on BSP)"               $ tryMsgR (ExpandTowards D) (MirrorExpand))
    ]


-- Mouse bindings: default actions bound to mouse events
-- Includes window snapping on move/resize using X.A.FloatSnap
-- Includes window w/h ratio constraint (square) using X.H.ConstrainedResize
myMouseBindings (XConfig {XMonad.modMask = myModMask}) = M.fromList $

    [ ((myModMask,               button1) ,(\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button1), (\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask,               button3), (\w -> focus w
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button3), (\w -> focus w
      >> Sqr.mouseResizeWindow w True
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster ))

--    , ((mySecondaryModMask,      button4), (\w -> focus w
--      >> prevNonEmptyWS))
--
--    , ((mySecondaryModMask,      button5), (\w -> focus w
--      >> nextNonEmptyWS))

    ]

------------------------------------------------------------------------}}}
-- Startup                                                              {{{
---------------------------------------------------------------------------

myStartupHook = do

    -- init-tilingwm sets up all major "desktop environment" like components
    -- spawnOnce "$HOME/bin/wm/init-tilingwm"
    -- spawn "/home/ethan/bin/wm/init-tilingwm"
    spawn "xsetroot -grey"
    spawn "~/.screenlayout/myScreenLayout.sh"
    spawn "xscreensaver &"
    -- init-tray kills and restarts stalone tray, hence just "spawn" so it
    -- runs on restart and will suffice to reposition tray on display changes
    -- TODO: evaluate moving to a "restart tray only" option on display change
    -- spawn     "$HOME/bin/wm/init-tray"

    setDefaultCursor xC_left_ptr

quitXmonad :: X ()
quitXmonad = io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = do
    spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = do
    spawn "xmonad --restart"

---------------------------------------------------------------------------
-- Urgency Hook                                                            
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]
-- cf https://github.com/pjones/xmonadrc


---------------------------------------------------------------------------
-- New Window Actions
---------------------------------------------------------------------------

-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples
-- <+> manageHook defaultConfig

myManageHook :: ManageHook
myManageHook =
        manageSpecific
    <+> manageDocks
    <+> namedScratchpadManageHook []
    <+> fullscreenManageHook
    <+> manageSpawn
    where
        manageSpecific = composeOne
            [ resource =? "desktop_window" -?> doIgnore
            , resource =? "stalonetray"    -?> doIgnore
            , resource =? "vlc"    -?> doFloat
            , transience
            , isRole =? gtkFile  -?> forceCenterFloat
            , isDialog -?> doCenterFloat
            , isRole =? "pop-up" -?> doCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
            , resource =? "console" -?> tileBelowNoFocus
            , isFullscreen -?> doFullFloat
            , pure True -?> tileBelow 
            ]

        gtkFile = "GtkFileChooserDialog"
        isRole = stringProperty "WM_WINDOW_ROLE"
        -- insert WHERE and focus WHAT
        tileBelow = insertPosition Below Newer
        tileBelowNoFocus = insertPosition Below Older

---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------

-- for reference, the following line is the same as dynamicTitle myDynHook
-- <+> dynamicPropertyChange "WM_NAME" myDynHook

-- I'm not really into full screens without my say so... I often like to
-- fullscreen a window but keep it constrained to a window rect (e.g.
-- for videos, etc. without the UI chrome cluttering things up). I can
-- always do that and then full screen the subsequent window if I want.
-- THUS, to cut a long comment short, no fullscreenEventHook
-- <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook

myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> handleEventHook def
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook

---------------------------------------------------------------------------
-- Custom hook helpers
---------------------------------------------------------------------------

-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2

-- I left this here because I want to explore using tags more
-- ... did I crib this from pjones config?
--
---- | If the given condition is 'True' then add the given tag name to
---- the 
----window being mapped.  Always returns 'Nothing' to continue
---- processing other manage hooks.
--addTagAndContinue :: Query Bool -> String -> MaybeManageHook
--addTagAndContinue p tag = do
--  x <- p
--  when x (liftX . addTag tag =<< ask)
--  return Nothing


-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:shiftwidth=4

