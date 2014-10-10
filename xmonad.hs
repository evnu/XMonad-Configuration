-- Haskell modules
import Data.List

-- XMonad modules
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- find an empty workspace :)
import XMonad.Actions.FindEmptyWorkspace

-- Keybindings , greedyview 'n stuff
import XMonad.Util.EZConfig

-- fullscreen?
import XMonad.Hooks.ManageHelpers

-- scratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Util.Run -- spawnPipe and hPutStrLn

-- urgent notifications
import XMonad.Hooks.UrgencyHook

-- one-line file edits
import XMonad.Prompt
import XMonad.Prompt.AppendFile

-- cycle workspaces
import XMonad.Actions.CycleWS
-- my configuration modules
import ApplicationRules
import LayoutRules
import Workspaces

stdOutUrgencyHook :: StdoutUrgencyHook
stdOutUrgencyHook = StdoutUrgencyHook

main = do
	-- momma needs a bar.
	output <- spawnPipe "xmobar"
	xmonad $ withUrgencyHook stdOutUrgencyHook $ (myConfig output)


myTerminal = "urxvt"

{-
 define some colors
-}
foreground = "#f5f5f5"
background = "#000000"
red = "#ff0000"
white = "#ffffff"

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

scratchPads = [NS "ncmpcpp" spawnNcmpcpp  findNcmpcpp  manageNcmpcpp  -- mpd
              , NS "ding"  spawnDing  findDing  manageDing  -- ding dictionary lookup
              , NS "htop"  spawnhtop  findhtop  managehtop  -- htop
              , NS "alot"  spawnAlot  findAlot  manageAlot  -- alot
              , NS "bc"  spawnBC  findBC  manageBC  -- bc
              , NS "pry" spawnPry findPry managePry -- pry
              , NS "erl" spawnErl findErl manageErl -- erl
              ]
              where
                spawnMixer  = "aumix"
                findMixer   = resource =? "aumix"
                manageMixer = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.4
                    w = 0.6
                    t = (1 - h)/2
                    l = (1 - w)/2
                spawnNcmpcpp   = myTerminal ++ " -name mpd -e ncmpcpp"
                findNcmpcpp    = resource =? "mpd"
                manageNcmpcpp  = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.5
                    w = 0.8
                    t = (1 - h)/2
                    l = (1 - w)/2
                spawnDing   = "ding"
                findDing    = className =? "Ding"
                manageDing  = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.5
                    w = 0.5
                    t = (1 - h)/2
                    l = (1 - w)/2
                spawnhtop   = myTerminal ++ " -title htop -e htop"
                findhtop    = fmap ("htop" `isPrefixOf`) title
                managehtop  = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.7
                    w = 0.8
                    t = (1 - h)/2
                    l = (1 - w)/2
                spawnAlot   = myTerminal ++ " -name alot -e alot"
                findAlot    = resource =? "alot"
                manageAlot  = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.8
                    w = 0.8
                    t = (1 - h)/2
                    l = (1 - w)/2
                spawnBC   = myTerminal ++ " -name bc -e bc -l"
                findBC    = resource =? "bc"
                manageBC  = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.5
                    w = 0.8
                    t = (1 - h)/2
                    l = (1 - w)/2
                spawnPry   = myTerminal ++ " -name pry -e pry"
                findPry    = resource =? "pry"
                managePry  = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.5
                    w = 0.8
                    t = (1 - h)/2
                    l = (1 - w)/2
                spawnErl   = myTerminal ++ " -name erl -e erl"
                findErl    = resource =? "erl"
                manageErl  = customFloating $ W.RationalRect l t w h
                  where
                    h = 0.5
                    w = 0.8
                    t = (1 - h)/2
                    l = (1 - w)/2

{-
  - key bindings
-}
myKeyBindings = [
    ((windowsKey, xK_e),     viewEmptyWorkspace)  -- jump to empty workspace
  , ((windowsKey, xK_F11),   scratchNcmpcpp)
  , ((windowsKey, xK_d),     scratchDing)
  , ((windowsKey, xK_g),     scratchHtop)
  , ((windowsKey, xK_n),     scratchAlot)
  , ((windowsKey, xK_a),     scratchBC)
  , ((windowsKey, xK_o),     scratchPry)
  , ((windowsKey, xK_u),     scratchErl)
  , ((windowsKey, xK_Print), spawn "scrot")
  , ((windowsKey, xK_b),     spawn "showbatt")
  , ((windowsKey, xK_p),     spawn "dmenu_run")
  , ((windowsKey, xK_l),     toggleWS) -- cycle to previous workspace
  {-
      control mpd
  -}
  , ((0, 0x1008FF17), spawn "ncmpcpp prev")
  , ((0, 0x1008ff17), spawn "ncmpcpp next")
  , ((0, 0x1008FF14), spawn "ncmpcpp toggle")
  , ((0, 0x1008FF15), spawn "ncmpcpp stop")
  ]
  ++
  [
      -- build view instead of greedyview - if we want a view on a screen, we'll stick with it
      -- and don't allow another screen to access it.
      ((m .|. mod4Mask, k), windows $ f i) | (i,k) <- zip myWorkSpaces [xK_1 .. xK_9], (f,m) <- [(W.view,0),(W.shift, shiftMask)]
  ]
  where
    scratchNcmpcpp   = namedScratchpadAction scratchPads "ncmpcpp"
    scratchDing      = namedScratchpadAction scratchPads "ding"
    scratchHtop      = namedScratchpadAction scratchPads "htop"
    scratchAlot      = namedScratchpadAction scratchPads "alot"
    scratchBC        = namedScratchpadAction scratchPads "bc"
    scratchPry       = namedScratchpadAction scratchPads "pry"
    scratchErl       = namedScratchpadAction scratchPads "erl"


-- put some applications on specific workspaces
manageWorkspaces = composeAll . concat $ rules

-- some aliases
windowsKey = mod4Mask

{-
	xmonad style
-}
myConfig output = defaultConfig {
           terminal    = myTerminal
         , modMask     = windowsKey
         , borderWidth = 0
         , manageHook  = myHooks
         , layoutHook  = LayoutRules.layoutRules
         , workspaces  = myWorkSpaces
         , logHook = dynamicLogWithPP $ myPP output
         } `additionalKeys` myKeyBindings
{-
  - xmobar style
-}
myPP output = defaultPP {
    ppCurrent = xmobarColor foreground background . wrap "[" "]"
  , ppVisible = wrap "(" ")"
  , ppHidden = noScratchPad
  , ppHiddenNoWindows = const ""
  , ppSep    = " -> "
  , ppTitle  = xmobarColor foreground "" . shorten 60
  , ppUrgent = xmobarColor red white
  , ppWsSep  = " : "
  , ppOutput = hPutStrLn output
 }
  where
    noScratchPad ws = if ws == "NSP" then "" else ws
