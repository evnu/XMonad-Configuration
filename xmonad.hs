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

-- my configuration modules
import ApplicationRules
import LayoutRules
import Workspaces
import SolarizedColors

stdOutUrgencyHook :: StdoutUrgencyHook
stdOutUrgencyHook = StdoutUrgencyHook

main = do
	-- momma needs a bar.
	output <- spawnPipe "xmobar"
	-- trayer
	--none  <- spawn "trayer.sh"
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
              , NS "ncmpcpp" spawnNcmpcpp  findNcmpcpp  manageNcmpcpp  -- mpd
              , NS "ding"  spawnDing  findDing  manageDing  -- ding dictionary lookup
              , NS "htop"  spawnhtop  findhtop  managehtop  -- htop
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

{-
  - key bindings
-}
myKeyBindings = [
    ((windowsKey, xK_e),     viewEmptyWorkspace)  -- jump to empty workspace
  , ((windowsKey, xK_a),     scratchAlsamixer)
  , ((windowsKey, xK_F11),   scratchNcmpcpp)
  , ((windowsKey, xK_d),     scratchDing)
  , ((windowsKey, xK_g),     scratchHtop)
  , ((windowsKey, xK_Print), spawn "scrot")
  , ((windowsKey, xK_b),     spawn "showbatt")
	, ((windowsKey, xK_p),     spawn "~/bin/dmenu_run")
  , ((windowsKey, xK_n),     do
      spawn ("date | perl -pe 'chomp and $_ .= \" \"'>>" ++ "/home/evnu/todos")
      appendFilePrompt defaultXPConfig "/home/evnu/todos")
  ]
  ++
  [
      -- build view instead of greedyview - if we want a view on a screen, we'll stick with it
      -- and don't allow another screen to access it.
      ((m .|. mod4Mask, k), windows $ f i) | (i,k) <- zip myWorkSpaces [xK_1 .. xK_9], (f,m) <- [(W.view,0),(W.shift, shiftMask)]
  ]
  where
    scratchAlsamixer = namedScratchpadAction scratchPads "mixer"
    scratchNcmpcpp   = namedScratchpadAction scratchPads "ncmpcpp"
    scratchDing      = namedScratchpadAction scratchPads "ding"
    scratchHtop      = namedScratchpadAction scratchPads "htop"


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
         , normalBorderColor = base02
         , focusedBorderColor = cyan
         , logHook = dynamicLogWithPP $ myPP output
         } `additionalKeys` myKeyBindings
{-
  - xmobar style
-}
myPP output = defaultPP { 
    ppCurrent = xmobarColor base02 base00 . wrap "[" "]"
  , ppVisible = wrap "(" ")"
  , ppHidden = noScratchPad
  , ppHiddenNoWindows = const ""
  , ppSep    = " -> " 
  , ppTitle  = xmobarColor base01 "" . shorten 60 
  , ppUrgent = xmobarColor yellow base3
  , ppWsSep  = " : "
  , ppLayout = const ""
  -- receives three formatted strings: workspace, layout, current window title
  , ppOrder  = \(ws:_:t:_) -> [ws,t] -- ignore layout
  , ppOutput = hPutStrLn output 
 }
  where
    noScratchPad ws = if ws == "NSP" then "" else ws
