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
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.Circle

-- Data.Ratio for IM layout
import Data.Ratio ((%))

-- special layouts
layoutRules = onWorkspace "1:im" chatLayout $ onWorkspace "2:www" webLayout $ 
	onWorkspace "3:mail" mailLayout $ 
	onWorkspace "5:dev" devLayout $
	standardLayouts
    where
        standardLayouts = avoidStruts $ Tall 1 (3/100) (1/2) ||| smartBorders Full 

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

        --terminal layout
        devLayout = avoidStruts $ (Grid ||| full)

        --mail layout
        mailLayout = avoidStruts $ Circle

-- themeing for tab layout
myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }
