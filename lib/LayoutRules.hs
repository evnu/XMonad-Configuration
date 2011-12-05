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
	onWorkspace "5:dev" devLayout $
  onWorkspace "6:ddd" devLayout $
  onWorkspace "7:write" writingLayout $
	onWorkspace "9:gimp" gimp $
	standardLayouts
    where
        standardLayouts = avoidStruts $ Tall 1 (3/100) (1/2) ||| smartBorders Full  ||| Mirror tiled

        --Layouts
        tiled        = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        full         = noBorders Full
        tabLayout    = noBorders (tabbed shrinkText myTheme)

        --Im Layout
        chatLayout = avoidStruts $ smartBorders $ reflectHoriz $ withIM pidginRatio pidginRoster (Grid ||| tabLayout ||| writingLayout) where
                pidginRatio     = (1%7)
                pidginRoster    = (ClassName "Pidgin") `And` (Role "buddy_list")

        --Web Layout
        webLayout      = avoidStruts $ (tabLayout ||| Tall 1(3/100) (1/2)) ||| (ThreeCol 1 (3/100) (1/2))

        --terminal/dev layout
        devLayout = avoidStruts $ (Grid ||| full) ||| Mirror tiled

        --mail layout
        mailLayout = standardLayouts
        
        -- write with big master
        writingLayout = avoidStruts $ Mirror tiled
        -- TODO doesn't work. fuck gimp.
        gimp = withIM (toolboxRatio) (toolboxRole) $ (reflectHoriz $ withIM (dockRatio) (dockRole) $ Grid ||| tabLayout)
					where
						toolboxRatio = 1%10
						toolboxRole = Role "gimp-toolbox"
						dockRatio  = 15%100
						dockRole = Role "gimp-dock"
				

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
