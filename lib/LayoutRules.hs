module LayoutRules
where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed

-- Make a given layout display without borders -> smartBorders: only show border if really
-- needed
import XMonad.Layout.NoBorders

layoutRules = avoidStruts $ Tall 1 (3/100) (1/2) ||| simpleTabbed  ||| smartBorders Full
