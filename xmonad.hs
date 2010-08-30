import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- find an empty workspace :)
import XMonad.Actions.FindEmptyWorkspace

-- Keybindings 'n stuff
import XMonad.Util.EZConfig

-- fullscreen?
import XMonad.Hooks.ManageHelpers

-- my configuration modules
import ApplicationRules
import LayoutRules

main = do 
  xmonad =<< xmobar myConfig 

myConfig = defaultConfig { 
           terminal    = "urxvt"
         , modMask     = windowsKey
         , borderWidth = 2 
         , manageHook  = composeAll myHooks
         , layoutHook  = LayoutRules.layoutRules 
         , workspaces = myWorkSpaces
         -- set colors
         , normalBorderColor = "#666666"
         , focusedBorderColor = "#00FF40"
         } `additionalKeys` myKeyBindings

{- 
  - manage Hooks
-}
myHooks = [
      manageHook defaultConfig
    , manageDocks, manageWorkspaces
    , composeOne [ isFullscreen -?> doFullFloat ] -- manage fullscreeen windows
  ]

{-
  - key bindings
-}
myKeyBindings = [
  ((windowsKey, xK_e), viewEmptyWorkspace)  -- jump to empty workspace
  ]


-- put some applications on specific workspaces
manageWorkspaces = composeAll .  concat $ rules

-- some aliases
windowsKey = mod4Mask

-- define workspaces
myWorkSpaces :: [WorkspaceId]
myWorkSpaces = ["1:IM", "2:Browser", "3:Mail", "4:IRC", "5:Dev"] ++ map show [6..9]
