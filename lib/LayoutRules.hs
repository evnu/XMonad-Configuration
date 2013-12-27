module LayoutRules
where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed

-- Make a given layout display without borders -> smartBorders: only show border if really
-- needed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiColumns

import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators hiding ((|||))

-- show workspace name on switch
import XMonad.Layout.ShowWName


-- Data.Ratio for IM layout
import Data.Ratio ((%))

-- color theme
import SolarizedColors

-- special layouts
layoutRules = showWName' swnConfig  $ onWorkspace "1:im" chatLayout $ 
  onWorkspace "2:www" webLayout $ 
  onWorkspace "3:mail" mailLayout $ 
  onWorkspace "4:dev0" devLayout $ 
  onWorkspace "5:dev1" devLayout $
  onWorkspace "6:dev2" devLayout $
  onWorkspace "7:dev3" devLayout $
  onWorkspace "8:write" writingLayout $
  onWorkspace "9:gimp" gimp $ standardLayouts
    where
        standardLayouts = Tall 1 (3/100) (1/2) ||| smartBorders Full  ||| Mirror tiled

        --Layouts
        tiled        = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        full         = noBorders Full
        tabLayout    = noBorders (tabbed shrinkText myTheme)

        --Im Layout
        chatLayout = smartBorders $
            reflectHoriz $ withIM pidginRatio pidginRoster (Grid ||| tabLayout ||| writingLayout) where
                pidginRatio     = (1%7)
                pidginRoster    = (ClassName "Pidgin") `And` (Role "buddy_list")

        --Web Layout
        webLayout = (tabLayout ||| Tall 1(3/100) (1/2)) ||| (ThreeCol 1 (3/100) (1/2))

        --terminal/dev layout
        devLayout = (Grid ||| full ||| Mirror tiled ||| (multiCol [6] 6 0.01 0.5))

        --mail layout
        mailLayout = standardLayouts
        
        -- write with big master
        writingLayout = Mirror tiled

        -- gimp
        gimp = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full ||| Grid

-- themeing for tab layout
myTheme = defaultTheme  {
    activeColor = base03
    , inactiveColor = base02
    , urgentColor = yellow
    , activeBorderColor = base03
    , inactiveBorderColor = base03
    , urgentBorderColor = yellow
    , activeTextColor = base2
    , inactiveTextColor = base01
    , urgentTextColor = yellow
    }

swnConfig :: SWNConfig
swnConfig = SWNC {
    swn_font    = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
        , swn_bgcolor = "black"
        , swn_color   = "white"
        , swn_fade    = 1
}
