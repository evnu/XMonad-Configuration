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

-- my configuration modules
import ApplicationRules
import LayoutRules
import Workspaces

stdOutUrgencyHook :: StdoutUrgencyHook
stdOutUrgencyHook = StdoutUrgencyHook

main = do
	-- momma needs a bar.
	output <- spawnPipe "xmobar"
	-- trayer
	--none  <- spawn "trayer.sh"
	--mocpBar <- spawnPipe "xmobar '%StdinReader%' -t -o "
	xmonad $ withUrgencyHook stdOutUrgencyHook $ (myConfig output)


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
              , NS "ding"  spawnDing  findDing  manageDing  -- ding dictionary lookup
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
                spawnMocp   = myTerminal ++ " -title MOC -e mocp"
                findMocp    = fmap ("MOC" `isPrefixOf`) title
                manageMocp  = customFloating $ W.RationalRect l t w h
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

{-
  - key bindings
-}
myKeyBindings = [
    ((windowsKey, xK_e), viewEmptyWorkspace)  -- jump to empty workspace
  , ((windowsKey, xK_a), scratchAlsamixer)
  , ((windowsKey, xK_F11), scratchMocp)
  , ((windowsKey, xK_d), scratchDing)
  , ((windowsKey, xK_Print), spawn "scrot")
  , ((windowsKey, xK_b), spawn "showbatt")
	, ((windowsKey, xK_p), spawn "~/bin/dmenu_run")
  ]
  ++
  [
  -- build view instead of greedyview - if we want a view on a screen, we'll stick with it
  -- and don't allow another screen to access it.
    ((m .|. mod4Mask, k), windows $ f i) | (i,k) <- zip myWorkSpaces [xK_1 .. xK_9], (f,m) <- [(W.view,0),(W.shift, shiftMask)]
  ]
  where
    scratchAlsamixer = namedScratchpadAction scratchPads "mixer"
    scratchMocp      = namedScratchpadAction scratchPads "mocp"
    scratchDing      = namedScratchpadAction scratchPads "ding"


-- put some applications on specific workspaces
manageWorkspaces = composeAll .  concat $ rules

-- some aliases
windowsKey = mod4Mask

{-
	xmonad style
-}
myConfig output = defaultConfig { 
           terminal    = myTerminal
         , modMask     = windowsKey
         , borderWidth = 2 
         , manageHook  = myHooks
         , layoutHook  = LayoutRules.layoutRules 
         , workspaces  = myWorkSpaces
         -- set colors
         , normalBorderColor = "#202020"
         , focusedBorderColor = "#FF0000"
         , logHook = dynamicLogWithPP $ myPP output
         } `additionalKeys` myKeyBindings
{-
  - xmobar style
-}
myPP output = defaultPP { 
    ppCurrent = xmobarColor "#7B79B1" "#0F141F" . wrap "[" "]"
  , ppVisible = wrap "(" ")"
  , ppHidden = noScratchPad
  , ppHiddenNoWindows = const ""
  , ppSep    = " -> " 
  , ppTitle  = xmobarColor "#7B79B1" "" . shorten 60 
  , ppUrgent = xmobarColor "#2BA624" "0FA3A3"
  , ppWsSep  = " : "
  , ppLayout = const ""
  -- receives three formatted strings: workspace, layout, current window title
  , ppOrder  = \(ws:_:t:_) -> [ws,t] -- ignore layout
  , ppOutput = hPutStrLn output 
 }
  where
    noScratchPad ws = if ws == "NSP" then "" else ws
