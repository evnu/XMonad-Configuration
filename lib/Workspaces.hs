module Workspaces
where

import XMonad

myWorkSpaces :: [WorkspaceId]
myWorkSpaces = ["1:im", "2:www", "3:mail", "4:IRC", "5:dev"] ++ map show [6..9]


