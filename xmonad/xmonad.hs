{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import           Adapt2Environment
import           Data.String.Utils (replace)
import           XMonad
import           XMonad.Actions.CycleWS (toggleWS)
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer (gotoMenu)
import           XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import qualified XMonad.Hooks.DynamicLog as L
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Hooks.ManageDocks (AvoidStruts)
import           XMonad.Hooks.ManageDocks (docksEventHook)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.Magnifier (magnifiercz)
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys)
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WorkspaceCompare (getSortByXineramaPhysicalRule)


main :: IO ()
main = xmonad =<< myxmobar defaults


defaults = def
  { borderWidth        = 2
  , focusFollowsMouse  = True
  , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
  , layoutHook         = Tall 1 (3/100) (1/2) ||| Full ||| Grid
                         ||| Mirror (Tall 1 (3/100) (1/2))
                         ||| magnifiercz 1.5 (Tall 1 (3/100) (1/2))
  , logHook            = do
                         fadeInactiveLogHook 0.9
                         updatePointer (0.5, 0.5) (0.9, 0.9)
  , manageHook         = composeAll
                           [ (className =? "Firefox")  --> doShift "web"
                           , (className =? "Emacs")    --> doShift "emacs"
                           , (appName   =? "gimp")     --> doFloat
                           , (appName   =? "kuake")    --> doSideFloat NC
                           , (appName   =? "pinentry") --> doFloat
                           , (appName   =? "wttr.in")  --> doFloat
                           ]
  , modMask            = mod4Mask
  , startupHook        = startup
  , terminal           = "urxvtc -e tmux"
  , workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "emacs", "web"]
  } `additionalKeys` shortcuts


startup :: X ()
startup = do
  adapt2environment
  spawn "xset b off dpms 300 300 300"
  spawn "feh --bg-center ~/.config/xmonad/background"
  spawnOnce "xcompmgr"
  spawnOnce "urxvtd --quiet --opendisplay --fork"
  spawnOnce "urxvt -name kuake -title kuake -kuake-hotkey F12 -geometry 140x40+0+0 -e tmux new-session -s Q"
  spawnOnce "stalonetray"
  spawnOnce "nitrokey-app"
  spawnOnce "blueman-applet"
  spawnOnce "usermount"  -- automount of removable media
  spawnOnce "unclutter --timeout 3"
  windows $ W.view "1"
  spawn "xsetroot -cursor_name left_ptr"


myxmobar :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myxmobar conf =
  L.statusBar "xmobar ~/.config/xmonad/xmobarrc" myPP toggleStrutsKey conf
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )


myPP:: L.PP
myPP = def { L.ppCurrent = L.xmobarColor "yellow" "#666600" . L.wrap "[<fn=1>" "</fn>]"
           , L.ppLayout  = \_ -> ""
           , L.ppSep     = " | "
           , L.ppSort    = getSortByXineramaPhysicalRule
           , L.ppTitle   = L.wrap "<fn=3>" "</fn>" . L.xmobarColor "white"  "" . L.shorten 100
           , L.ppVisible = L.xmobarColor "#cccccc" "#666600". L.wrap ".<fn=1>" "</fn>."
           , L.ppHidden  = L.wrap "<fn=1>" "</fn>"
           , L.ppUrgent  = L.xmobarColor "red" "yellow"
           }


shortcuts :: [((KeyMask, KeySym), X ())]
shortcuts =
  [ ((mod4Mask,                 xK_0),          windows $ W.greedyView "web")
  , ((mod4Mask .|. shiftMask,   xK_0),          windows $ W.shift "web")
  , ((controlMask .|. mod1Mask, xK_l),          spawn "sudo physlock -d")
  , ((mod4Mask,                 xK_o),          gotoMenu)
  , ((mod4Mask,                 xK_Escape),     toggleWS)
  , ((noModMask,                xK_Mute),       spawn "amixer sset Master toggle")
  , ((noModMask,                xK_LowerVol),   spawn "amixer sset Master 2%-")
  , ((noModMask,                xK_RaiseVol),   spawn "amixer sset Master 2%+")
  , ((noModMask,                xK_BrightUp),   spawn "xbacklight -inc 5")
  , ((noModMask,                xK_BrightDown), spawn "xbacklight -dec 5")
  , ((mod4Mask .|. mod1Mask,    xK_Return),     spawn "urxvtc -bg black -fg white")
  , ((noModMask,                xK_Launch6),    runOrRaise "firefox" (className =? "Firefox"))
  , ((noModMask,                xK_Launch5),    raiseMaybe (spawn "emacsclient -c") (className =? "Emacs"))
  , ((noModMask,                xK_Launch1),    raiseMaybe (spawn "emacsclient -c") (className =? "Emacs"))
  , ((controlMask,              xK_space),      spawn "~/.config/xmonad/togglekb")
  , ((noModMask,                xK_Display),    adapt2environment)
  ]
  where
    xK_LowerVol    = stringToKeysym "XF86AudioLowerVolume"
    xK_RaiseVol    = stringToKeysym "XF86AudioRaiseVolume"
    xK_Mute        = stringToKeysym "XF86AudioMute"
    xK_ScreenSaver = stringToKeysym "XF86ScreenSaver"
    xK_Launch1     = stringToKeysym "XF86Launch1"
    xK_Launch5     = stringToKeysym "XF86Launch5"
    xK_Launch6     = stringToKeysym "XF86Launch6"
    xK_BrightUp    = stringToKeysym "XF86MonBrightnessUp"
    xK_BrightDown  = stringToKeysym "XF86MonBrightnessDown"
    xK_Display     = stringToKeysym "XF86Display"
