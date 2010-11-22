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

-- show workspace name on switch
import XMonad.Layout.ShowWName


-- Data.Ratio for IM layout
import Data.Ratio ((%))

-- special layouts
layoutRules = showWName' swnConfig  $ onWorkspace "1:im" chatLayout $ onWorkspace "2:www" webLayout $ 
	onWorkspace "3:mail" mailLayout $ 
	onWorkspace "5:dev" devLayout $
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
        chatLayout = avoidStruts $ smartBorders $ reflectHoriz $ withIM pidginRatio pidginRoster (Grid ||| tabLayout) where
                pidginRatio     = (1%7)
                pidginRoster    = (ClassName "Pidgin") `And` (Role "buddy_list")

        --Web Layout
        webLayout      = avoidStruts $ (full ||| tabLayout ||| Tall 1(3/100) (1/2)) ||| tiled

        --terminal/dev layout
        devLayout = avoidStruts $ (Grid ||| full) ||| Mirror tiled

        --mail layout
        mailLayout = standardLayouts

        -- TODO doesn't work. fuck gimp.
        gimp = withIM (toolboxRatio) (toolboxRole) $ (reflectHoriz $ withIM (dockRatio) (dockRole) $ Grid ||| tabLayout)
					where
						toolboxRatio = 1%10
						toolboxRole = Role "gimp-toolbox"
						dockRatio  = 15%100
						dockRole = Role "gimp-dock"
				

-- themeing for tab layout
myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }

swnConfig :: SWNConfig
swnConfig = SWNC {
									swn_font    = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
								, swn_bgcolor = "black"
								, swn_color   = "white"
								, swn_fade    = 1
}
