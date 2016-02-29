module Main (main) where

import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, ppOutput, ppTitle, shorten, xmobarColor, xmobarPP)
import XMonad.Util.EZConfig    (additionalKeys)
import XMonad.Util.Run         (hPutStrLn, spawnPipe)

main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
  xmonad $ defaults
    { logHook = dynamicLogWithPP xmobarPP
               {ppOutput = hPutStrLn xmproc
               ,ppTitle = xmobarColor "green" "" . shorten 50
               }
    } `additionalKeys`
    [ ((controlMask .|. mod1Mask, xK_l), spawn "xscreensaver-command -lock")
    ]

defaults = defaultConfig
  {borderWidth        = 2
  ,modMask            = mod4Mask
  ,focusFollowsMouse  = False
  }

