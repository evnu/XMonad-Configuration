module ApplicationRules
where

import XMonad


rules = [
       [ className =? "Pidgin" --> doShift "1:IM" ]
     , [ className =? "Namoroka" --> doShift "2:Browser" ]
     , [ className =? "Xmessage" --> doFloat ]
     , [ className =? "sylpheed" --> doShift "3:Mail" ]
     , [ className =? "Sylpheed" --> doShift "3:Mail" ]
     , [ className =? "xchat" --> doShift "4:IRC" ]
     , [ className =? "Xchat" --> doShift "4:IRC" ]
  ]
