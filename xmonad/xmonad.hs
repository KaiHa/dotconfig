{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.String.Utils            ( replace)
import XMonad
import XMonad.Actions.CycleWS       ( toggleWS)
import XMonad.Actions.UpdatePointer ( updatePointer
                                    , PointerPosition(..))
import XMonad.Actions.WindowBringer ( gotoMenu)
import XMonad.Hooks.DynamicLog      ( defaultPP
                                    , dynamicLog
                                    , shorten
                                    , statusBar
                                    , wrap
                                    , xmobarColor
                                    , PP(..)
                                    )
import XMonad.Hooks.FadeInactive    ( fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks     ( AvoidStruts)
import XMonad.Layout.LayoutModifier ( ModifiedLayout)
import XMonad.Layout.Magnifier      ( magnifiercz')
import XMonad.Util.EZConfig         ( additionalKeys)
import XMonad.Util.WorkspaceCompare ( getSortByXineramaPhysicalRule)


main :: IO ()
main = xmonad =<< myxmobar defaults


defaults = defaultConfig
  { borderWidth        = 2
  , focusFollowsMouse  = False
  , layoutHook         = magnifiercz' 1.02 (Tall 1 (3/100) (1/2))
                         ||| Tall 1 (3/100) (1/2)
                         ||| Mirror (Tall 1 (3/100) (1/2)) ||| Full
  , logHook            = do
                         fadeInactiveLogHook 0.85
                         dynamicLog
                         updatePointer (Relative 0.95 0.95)
  , modMask            = mod4Mask
  } `additionalKeys`
  [ ((controlMask .|. mod1Mask, xK_l), spawn "xscreensaver-command -lock")
  , ((mod4Mask, xK_o),                 gotoMenu)
  , ((mod4Mask, xK_Escape),            toggleWS)
  ]

myxmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myxmobar conf =
  statusBar "xmobar ~/.config/xmonad/xmobarrc" myPP toggleStrutsKey conf
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

myPP:: PP
myPP = defaultPP { ppCurrent = xmobarColor "yellow" "#666600" . wrap "[" "]"
                 , ppLayout  = replace "NoMaster " ""
                 , ppSep     = " | "
                 , ppWsSep   = ""
                 , ppSort    = getSortByXineramaPhysicalRule
                 , ppTitle   = xmobarColor "white"  "" . shorten 60
                 , ppVisible = xmobarColor "#cccccc" "#666600". wrap "." "."
                 , ppHidden  = wrap " " " "
                 , ppUrgent  = xmobarColor "red" "yellow"
                 }
