{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import XMonad
import XMonad.Actions.WindowBringer (gotoMenu)
import XMonad.Hooks.DynamicLog      ( defaultPP
                                    , shorten
                                    , statusBar
                                    , wrap
                                    , xmobarColor
                                    , PP(..)
                                    )
import XMonad.Hooks.ManageDocks     (AvoidStruts)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig         (additionalKeys)
import XMonad.Util.Loggers          ( battery
                                    , wrapL
                                    , xmobarColorL
                                    )
import XMonad.Util.WorkspaceCompare (getSortByXineramaPhysicalRule)


main :: IO ()
main = xmonad =<< myxmobar defaults


defaults = defaultConfig
  { borderWidth        = 2
  , modMask            = mod4Mask
  , focusFollowsMouse  = False
  } `additionalKeys`
  [ ((controlMask .|. mod1Mask, xK_l), spawn "xscreensaver-command -lock")
  , ((mod4Mask, xK_o), gotoMenu)
  ]

myxmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myxmobar conf =
  statusBar "xmobar" myPP toggleStrutsKey conf
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

myPP:: PP
myPP = defaultPP { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                 , ppExtras  = [xmobarColorL "green" "#2A4C3F" $ wrapL "[" "]" battery]
                 , ppSep     = " | "
                 , ppSort    = getSortByXineramaPhysicalRule
                 , ppTitle   = xmobarColor "white"  "" . alignM 60
                 , ppVisible = wrap "(" ")"
                 , ppUrgent  = xmobarColor "red" "yellow"
                 }


alignM :: Int -> String -> String
alignM width txt
  | length txt < width  = txt ++ replicate (width - length txt) ' '
  | length txt == width = txt
  | otherwise           = shorten width txt
