module ApplicationRules
where

import XMonad

-- My modules
import Workspaces

match a action = className =? a --> action

rules = [
       [ className =? a --> doShift "1:im" | a <- instantMessageApplications ]
     , [ className =? a --> doShift "2:www" | a <- webApplications ]
     , [ className =? a --> doShift "3:mail"| a <- mailApplications ]
     , [ className =? a --> doShift "4:IRC" | a <- ircApplications ]
     , [ className =? a --> doShift "4:dev" | a <- devApplications ]
     , [ className =? a --> doFloat | a <- generalRules]
  ]


-- use list comprehension to make the configuration more readable
instantMessageApplications = ["Pidgin"]
webApplications = ["Firefox", "Liferea", "Namoroka"]
mailApplications = ["Sylpheed", "sylpheed"]
ircApplications = ["Xchat", "xchat"]
devApplications = []
generalRules = ["Xmessage"]
