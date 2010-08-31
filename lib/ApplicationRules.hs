module ApplicationRules
where

import XMonad

match a action = className =? a --> action

rules = [
       [ className =? a --> doShift "1:IM" | a <- instantMessageApplications ]
     , [ className =? a --> doShift "2:Browser" | a <- webApplications ]
     , [ className =? a --> doShift "3:Mail"| a <- mailApplications ]
     , [ className =? a --> doShift "4:IRC" | a <- ircApplications ]
     , [ className =? a --> doFloat | a <- generalRules]
  ]
listOfWorkspaces = ["1:IM", "2: Browser", "3:Mail", "4:IRC"]


-- use list comprehension to make the configuration more readable
instantMessageApplications = ["Pidgin"]
webApplications = ["Firefox", "Liferea", "Namoroka"]
mailApplications = ["Sylpheed", "sylpheed"]
ircApplications = ["Xchat", "xchat"]
generalRules = ["Xmessage"]
