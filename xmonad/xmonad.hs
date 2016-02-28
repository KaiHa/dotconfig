module Main (main) where

import XMonad

main :: IO ()
main = xmonad defaults

defaults = defaultConfig
  {borderWidth        = 2
  ,modMask            = mod4Mask
  ,focusFollowsMouse  = False
  }

