module LayoutRules
where

import XMonad
-- ManageDocks: Handle dock type programs, e.g. xmobar
import XMonad.Hooks.ManageDocks
-- Apparently, xmobar does not set the STRUTS property correctly;
-- fix the problem by setting a manual gap at the top of the screen
import XMonad.Layout.Gaps

import XMonad.Layout.Tabbed

-- Make a given layout display without borders -> smartBorders: only show border if really
-- needed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed

import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators hiding ((|||))

-- show workspace name on switch
import XMonad.Layout.ShowWName


-- Data.Ratio for IM layout
import Data.Ratio ((%))

-- color theme
import XMonad.Util.Themes

-- special layouts
layoutRules = gaps [(U,20)] $ showWName' swnConfig  $ onWorkspace "1:im" chatLayout $
  onWorkspace "2:www" webLayout $
  onWorkspace "3:dev0" devLayout $
  onWorkspace "4:dev1" devLayout $
  onWorkspace "5:dev2" devLayout $
  onWorkspace "6:dev3" devLayout $
  onWorkspace "7:dev4" devLayout $
  onWorkspace "9:gimp" gimp $ standardLayouts
    where
        standardLayouts = avoidStruts(Tall 1 (3/100) (1/2) ||| Full ||| Mirror tiled)

        --Layouts
        tiled        = (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        full         = noBorders Full
        tabLayout    = noBorders (tabbed shrinkText $ theme smallClean)

        --Im Layout
        chatLayout =
            avoidStruts $
                reflectHoriz $ withIM pidginRatio pidginRoster (tabLayout ||| writingLayout) where
                    pidginRatio     = (1%7)
                    pidginRoster    = (ClassName "Pidgin") `And` (Role "buddy_list")

        --Web Layout
        webLayout = avoidStruts(tabLayout ||| Tall 1(3/100) (1/2)) ||| (ThreeCol 1 (3/100) (1/2))

        --terminal/dev layout
        devLayout = avoidStruts(full ||| Mirror tiled)

        -- write with big master
        writingLayout = Mirror tiled

        gimp = devLayout

swnConfig :: SWNConfig
swnConfig = SWNC {
    swn_font    = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
        , swn_bgcolor = "black"
        , swn_color   = "white"
        , swn_fade    = 1
}
