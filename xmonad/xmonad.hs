{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import XMonad
import XMonad.Actions.WindowBringer (gotoMenu)
import XMonad.Actions.UpdatePointer ( updatePointer
                                    , PointerPosition(..))
import XMonad.Hooks.DynamicLog      ( defaultPP
                                    , dynamicLog
                                    , shorten
                                    , statusBar
                                    , wrap
                                    , xmobarColor
                                    , PP(..)
                                    )
import XMonad.Hooks.ManageDocks     (AvoidStruts)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig         (additionalKeys)
import XMonad.Util.WorkspaceCompare (getSortByXineramaPhysicalRule)


main :: IO ()
main = xmonad =<< myxmobar defaults


defaults = defaultConfig
  { borderWidth        = 2
  , modMask            = mod4Mask
  , focusFollowsMouse  = False
  , logHook            = dynamicLog >> updatePointer (Relative 0.95 0.95)
  } `additionalKeys`
  [ ((controlMask .|. mod1Mask, xK_l), spawn "xscreensaver-command -lock")
  , ((mod4Mask, xK_o), gotoMenu)
  ]

myxmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myxmobar conf =
  statusBar "xmobar ~/.config/xmonad/xmobarrc" myPP toggleStrutsKey conf
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

myPP:: PP
myPP = defaultPP { ppCurrent = xmobarColor "yellow" "#666600" . wrap "[" "]"
                 , ppSep     = " | "
                 , ppWsSep   = ""
                 , ppSort    = getSortByXineramaPhysicalRule
                 , ppTitle   = xmobarColor "white"  "" . shorten 60
                 , ppVisible = xmobarColor "#cccccc" "#666600". wrap "." "."
                 , ppHidden  = wrap " " " "
                 , ppUrgent  = xmobarColor "red" "yellow"
                 }
