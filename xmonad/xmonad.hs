{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import           Data.String.Utils (replace)
import           XMonad
import           XMonad.Actions.CycleWS (toggleWS)
import           XMonad.Actions.WindowBringer (gotoMenu)
import           XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import qualified XMonad.Hooks.DynamicLog as L
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Hooks.ManageDocks (AvoidStruts)
import           XMonad.Hooks.ManageDocks (docksEventHook)
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.Magnifier (magnifiercz)
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys)
import           XMonad.Util.WorkspaceCompare (getSortByXineramaPhysicalRule)


main :: IO ()
main = xmonad =<< myxmobar defaults


defaults = def
  { borderWidth        = 2
  , focusFollowsMouse  = True
  , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
  , layoutHook         = Tall 1 (3/100) (1/2) ||| Full
                         ||| Mirror (Tall 1 (3/100) (1/2))
                         ||| magnifiercz 1.5 (Tall 1 (3/100) (1/2))
  , manageHook         = composeAll
                           [ (className =? "Firefox") --> doShift "web"
                           , (className =? "Emacs")   --> doShift "emacs"
                           ]
  , modMask            = mod4Mask
  , startupHook        = windows $ W.view "web"
  , terminal           = "urxvtc"
  , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "emacs", "web"]
  } `additionalKeys`
  [ ((mod4Mask,                 xK_0),       windows $ W.greedyView "web")
  , ((mod4Mask .|. shiftMask,   xK_0),       windows $ W.shift "web")
  , ((controlMask .|. mod1Mask, xK_l),       spawn "xautolock -locknow")
  , ((mod4Mask,                 xK_o),       gotoMenu)
  , ((mod4Mask,                 xK_Escape),  toggleWS)
  , ((mod4Mask,                 xK_F8),      spawn "amixer sset Master 2%-")
  , ((mod4Mask,                 xK_F9),      spawn "amixer sset Master 2%+")
  , ((mod4Mask .|. mod1Mask,    xK_Return),  spawn "urxvtc -bg black -fg white")
  , ((noModMask,                xK_Launch2), runOrRaise "firefox" (className =? "Firefox"))
  , ((noModMask,                xK_Launch3), raiseMaybe (spawn "emacsclient -c") (className =? "Emacs"))
  ]


myxmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myxmobar conf =
  L.statusBar "xmobar ~/.config/xmonad/xmobarrc" myPP toggleStrutsKey conf
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )


myPP:: L.PP
myPP = def { L.ppCurrent = L.xmobarColor "yellow" "#666600" . L.wrap "[" "]"
           , L.ppLayout  = replace "NoMaster " ""
           , L.ppSep     = " | "
           , L.ppWsSep   = ""
           , L.ppSort    = getSortByXineramaPhysicalRule
           , L.ppTitle   = L.xmobarColor "white"  "" . L.shorten 60
           , L.ppVisible = L.xmobarColor "#cccccc" "#666600". L.wrap "." "."
           , L.ppHidden  = L.wrap " " " "
           , L.ppUrgent  = L.xmobarColor "red" "yellow"
           }


xK_LowerVol, xK_RaiseVol :: KeySym
xK_LowerVol    = stringToKeysym "XF86AudioLowerVolume"
xK_RaiseVol    = stringToKeysym "XF86AudioRaiseVolume"
xK_ScreenSaver = stringToKeysym "XF86ScreenSaver"
xK_Launch2     = stringToKeysym "XF86Launch2"
xK_Launch3     = stringToKeysym "XF86Launch3"
