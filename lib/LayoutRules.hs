module LayoutRules
where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed

layoutRules = avoidStruts $ Tall 1 (3/100) (1/2) ||| simpleTabbed
