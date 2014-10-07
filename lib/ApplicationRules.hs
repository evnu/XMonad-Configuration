module ApplicationRules
where

-- Haskell modules
import Data.List

-- XMonad modules

import XMonad

-- My modules
import Workspaces

import qualified XMonad.StackSet as W

import XMonad.Util.WindowProperties
import XMonad.Layout.IM

match a action = className =? a --> action

rules = [
       [ className =? a --> doShift "1:im" | a <- instantMessageApplications ]
     , [ className =? a --> doShift "2:www" | a <- webApplications ]
     , [ className =? a --> doShift "3:vsn" | a <- vsnApplications ]
     , [ className =? a --> doShift "3:mail"| a <- mailApplications ]
     , [ className =? a --> doShift "4:IRC" | a <- ircApplications ]
     , [ className =? a --> doShift "5:dev" | a <- devApplications ]
		 , [ className =? a --> doShift "6:ddd" | a <- debuggerApplications ]
     , [(role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)]
		 , [ className =? "Gimp" --> doShift "9:gimp"]
		 , [ className =? a --> doFloat | a <- animatorApplications ]
     , [ className =? a --> doFloat | a <- generalRules ]
     , [ className =? a --> doFloat | a <- libreOffice ]
  ] where role = stringProperty "WM_WINDOW_ROLE"


-- use list comprehension to make the configuration more readable
instantMessageApplications = ["Pidgin"]
webApplications = ["Firefox", "Liferea", "Namoroka", "Vimprobable"]
vsnApplications = ["thg"]
mailApplications = ["Sylpheed", "sylpheed"]
ircApplications = ["Xchat", "xchat"]
devApplications = []
debuggerApplications = ["Ddd"]
generalRules = ["Xmessage"]
animatorApplications = ["animator-CorbaServer", "sun-awt-X11-XFramePeer", "animator-NoCorbaClient"]
libreOffice = ["VCLSalFrame"]
