-- Haskell modules
import Data.List

-- XMonad modules
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- find an empty workspace :)
import XMonad.Actions.FindEmptyWorkspace

-- Keybindings 'n stuff
import XMonad.Util.EZConfig

-- fullscreen?
import XMonad.Hooks.ManageHelpers

-- scratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Util.Run -- spawnPipe and hPutStrLn

-- my configuration modules
import ApplicationRules
import LayoutRules
import Workspaces


main = do
 -- xmonad =<< xmobar myConfig 
  output <- spawnPipe "xmobar"
  xmonad $ myConfig output

myConfig output = defaultConfig { 
           terminal    = myTerminal
         , modMask     = windowsKey
         , borderWidth = 2 
         , manageHook  = myHooks
         , layoutHook  = LayoutRules.layoutRules 
         , workspaces  = myWorkSpaces
         -- set colors
         , normalBorderColor = "#666666"
         , focusedBorderColor = "#00FF40"
         , logHook = dynamicLogWithPP $ myPP output
         } `additionalKeys` myKeyBindings

myTerminal = "urxvt"

{- 
  - manage Hooks
-}

myHooks :: ManageHook
myHooks = composeAll [
      manageHook defaultConfig
    , manageDocks, manageWorkspaces
    , composeOne [ isFullscreen -?> doFullFloat ] -- manage fullscreeen windows
    , namedScratchpadManageHook scratchPads
  ]

scratchPads = [ NS "mixer" spawnMixer findMixer manageMixer -- mixer scratchpad
              , NS "mocp"  spawnMocp  findMocp  manageMocp  -- music on console
              ]
              where
                spawnMixer  = "aumix"
                findMixer   = resource =? "aumix"
                manageMixer = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.3
                    w = 0.6
                    t = (1 - h)/2
                    l = (1 - w)/2
                spawnMocp   = myTerminal ++ " -e mocp"
                findMocp    = fmap ("MOC" `isPrefixOf`) title
                manageMocp  = defaultFloating

{-
  - key bindings
-}
myKeyBindings = [
    ((windowsKey, xK_e), viewEmptyWorkspace)  -- jump to empty workspace
  , ((windowsKey, xK_a), scratchAlsamixer)
  , ((windowsKey, xK_F11), scratchMocp)
  , ((windowsKey, xK_Print), spawn "scrot")
  , ((windowsKey, xK_b), spawn "showbatt")
  ]
  where
    scratchAlsamixer = namedScratchpadAction scratchPads "mixer"
    scratchMocp      = namedScratchpadAction scratchPads "mocp"


-- put some applications on specific workspaces
manageWorkspaces = composeAll .  concat $ rules

-- some aliases
windowsKey = mod4Mask

{-
  - xmobar style
-}
myPP output = defaultPP { 
    ppCurrent = xmobarColor "blue" "" . wrap "[" "]"
  , ppVisible = wrap "(" ")"
  , ppHidden = noScratchPad
  , ppHiddenNoWindows = const ""
  , ppSep    = " -> " 
  , ppTitle  = xmobarColor "blue" "" . shorten 30
  , ppUrgent = xmobarColor "red" "yellow"
  , ppWsSep  = " : "
  , ppLayout = const ""
  -- receives three formatted strings: workspace, layout, current window title
  , ppOrder  = \(ws:_:t:_) -> [ws,t] -- ignore layout
  , ppOutput = hPutStrLn output 
 }
  where
    noScratchPad ws = if ws == "NSP" then "" else ws
