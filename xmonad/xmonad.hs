{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.String.Utils            ( replace)
import XMonad
import XMonad.Actions.CycleWS       ( toggleWS)
import XMonad.Actions.WindowBringer ( gotoMenu)
import XMonad.Hooks.DynamicLog      ( dynamicLog
                                    , shorten
                                    , statusBar
                                    , wrap
                                    , xmobarColor
                                    , PP(..)
                                    )
import XMonad.Hooks.ManageDocks     ( docksEventHook )
import XMonad.Hooks.FadeInactive    ( fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks     ( AvoidStruts)
import XMonad.Layout.LayoutModifier ( ModifiedLayout)
import XMonad.Layout.Magnifier      ( magnifiercz)
import XMonad.Util.EZConfig         ( additionalKeys)
import XMonad.Util.WorkspaceCompare ( getSortByXineramaPhysicalRule)


main :: IO ()
main = xmonad =<< myxmobar defaults


defaults = def
  { borderWidth        = 2
  , focusFollowsMouse  = False
  , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
  , layoutHook         = Tall 1 (3/100) (1/2) ||| Full
                         ||| Mirror (Tall 1 (3/100) (1/2))
                         ||| magnifiercz 1.5 (Tall 1 (3/100) (1/2))
  , logHook            = do
                         fadeInactiveLogHook 0.9
                         dynamicLog
  , modMask            = mod4Mask
  , terminal           = "urxvtc"
  } `additionalKeys`
  [ ((controlMask .|. mod1Mask, xK_l), spawn "xautolock -locknow")
  , ((mod4Mask, xK_o),                 gotoMenu)
  , ((mod4Mask, xK_Escape),            toggleWS)
  , ((noModMask, xK_LowerVol),         spawn "amixer -D pulse sset Master 2%-")
  , ((noModMask, xK_RaiseVol),         spawn "amixer -D pulse sset Master 2%+")
  ]


myxmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myxmobar conf =
  statusBar "xmobar ~/.config/xmonad/xmobarrc" myPP toggleStrutsKey conf
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )


myPP:: PP
myPP = def { ppCurrent = xmobarColor "yellow" "#666600" . wrap "[" "]"
           , ppLayout  = replace "NoMaster " ""
           , ppSep     = " | "
           , ppWsSep   = ""
           , ppSort    = getSortByXineramaPhysicalRule
           , ppTitle   = xmobarColor "white"  "" . shorten 60
           , ppVisible = xmobarColor "#cccccc" "#666600". wrap "." "."
           , ppHidden  = wrap " " " "
           , ppUrgent  = xmobarColor "red" "yellow"
           }


xK_LowerVol, xK_RaiseVol :: KeySym
xK_LowerVol = stringToKeysym "XF86AudioLowerVolume"
xK_RaiseVol = stringToKeysym "XF86AudioRaiseVolume"
