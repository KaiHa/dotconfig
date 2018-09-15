{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Main (main) where

import           Adapt2Environment
import           Numeric
import           XMonad
import           XMonad.Actions.CycleWS (toggleWS)
import           XMonad.Actions.GridSelect
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer (gotoMenuArgs)
import           XMonad.Actions.WindowGo (runOrRaise, runOrRaiseAndDo, raiseMaybe)
import qualified XMonad.Hooks.DynamicLog as L
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Hooks.ManageDocks (AvoidStruts)
import           XMonad.Hooks.ManageDocks (docksEventHook)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.Magnifier (magnifiercz)
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys)
import           XMonad.Util.Loggers (logCmd)
import           XMonad.Util.Paste (sendKeyWindow)
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WorkspaceCompare (getSortByXineramaPhysicalRule)


main :: IO ()
main = xmonad . withHook =<< myxmobar defaults
  where
    withHook = withUrgencyHook (BorderUrgencyHook {urgencyBorderColor = "gold"})


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
                         L.dynamicLogString rightPP >>= L.xmonadPropLog
                         logHook def
  , manageHook         = composeAll
                           [ (className =? "Firefox")  --> doShift "web"
                           , (className =? "Nightly")  --> doShift "web"
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
  spawnOnce "xcompmgr"
  spawn     "urxvtd --quiet --opendisplay --fork"
  spawn     "urxvt -name kuake -title kuake -kuake-hotkey F12 -geometry 140x40+0+0 -e tmux new-session -s Q"
  spawnOnce "nitrokey-app"
  spawnOnce "blueman-applet"
  spawnOnce "usermount"  -- automount of removable media
  spawnOnce "unclutter --timeout 3"
  windows $ W.view "1"
  spawn "xsetroot -cursor_name left_ptr"


myxmobar :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myxmobar conf =
  L.statusBar "xmobar ~/.config/xmonad/xmobarrc" leftPP toggleStrutsKey conf
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )


leftPP:: L.PP
leftPP = def { L.ppCurrent = L.xmobarColor "black" "yellow" . L.wrap "[<fn=1>" "</fn>]"
             , L.ppSep     = "<fc=#888888> | </fc>"
#if MIN_VERSION_xmonad_contrib(0,14,0)
             , L.ppSort    = getSortByXineramaPhysicalRule def
#else
             , L.ppSort    = getSortByXineramaPhysicalRule
#endif
             , L.ppTitle   = L.wrap "<fn=3>" "</fn>" . L.xmobarColor "black"  "" . L.shorten 100
             , L.ppVisible = L.xmobarColor "#cccccc" "#666600". L.wrap ".<fn=1>" "</fn>."
             , L.ppHidden  = L.wrap "<fn=1>" "</fn>"
             , L.ppUrgent  = L.xmobarColor "yellow" "red" . L.wrap "<fn=1>" "*</fn>"
             , L.ppOrder   =  \(w:_:t:xs) -> [w, t]
             }


rightPP :: L.PP
rightPP = def { L.ppSep     = " "
              , L.ppExtras  = [ notmuch, load ]
              , L.ppOrder   = \(_:_:_:xs) -> xs
              }


notmuch = do
  count <- logCmd "notmuch count tag:inbox and tag:unread"
  return $ case count of
    Just "0" -> Nothing
    Just a   -> Just $ "<fc=#AA0000><icon=/home/kai/.xmonad/icons/mail.xbm/>" ++ a ++ "</fc>"
    Nothing  -> Nothing


load = do
  load <- logCmd "awk '{print $1, $2, $3}' /proc/loadavg"
  return $ case load of
    Just load' -> Just $ icon ++ (unwords $ map colorize $ map read $ words load')
    Nothing    -> Nothing
  where
    limit = 0.8 :: Double
    colorize a =
      if a < limit
      then "<fc=black>" ++ showFFloat (Just 2) a "" ++ "</fc>"
      else "<fc=red>"   ++ showFFloat (Just 2) a "" ++ "</fc>"
    icon = "<icon=/home/kai/.xmonad/icons/cpu.xbm/>"


shortcuts :: [((KeyMask, KeySym), X ())]
shortcuts =
  [ ((mod4Mask,                 xK_0),          windows $ W.greedyView "web")
  , ((mod4Mask .|. shiftMask,   xK_0),          windows $ W.shift "web")
  , ((mod4Mask,                 xK_i),          goToSelected def)
  , ((controlMask .|. mod1Mask, xK_l),          spawn "sudo physlock -d")
  , ((mod4Mask,                 xK_o),          gotoMenuArgs ["-i", "-nf", "black", "-nb", "#99FF99"])
  , ((mod4Mask,                 xK_p),          spawn "dmenu_run -i -nf black -nb '#FFFF66'")
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
  , ((noModMask,                xK_Mail),       runOrRaiseAndDo "emacsclient -c" (className =? "Emacs")
      (\a -> sendKeyWindow controlMask xK_x a >> sendKeyWindow noModMask xK_n a))
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
    xK_Mail        = stringToKeysym "XF86Mail"
    xK_BrightUp    = stringToKeysym "XF86MonBrightnessUp"
    xK_BrightDown  = stringToKeysym "XF86MonBrightnessDown"
    xK_Display     = stringToKeysym "XF86Display"
