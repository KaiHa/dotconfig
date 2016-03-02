{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import XMonad
import XMonad.Hooks.DynamicLog      ( defaultPP
                                    , dzenColor
                                    , dzenEscape
                                    , pad
                                    , statusBar
                                    , PP(..)
                                    )
import XMonad.Hooks.ManageDocks     (AvoidStruts)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig         (additionalKeys)
import XMonad.Util.Font             (Align(..))
import XMonad.Util.Loggers          ( battery
                                    , date
                                    , dzenColorL
                                    , fixedWidthL
                                    , logTitle
                                    , padL
                                    , wrapL
                                    )


main :: IO ()
main = do
  xmonad =<< mydzen defaults


defaults = defaultConfig
  { borderWidth        = 2
  , modMask            = mod4Mask
  , focusFollowsMouse  = False
  } `additionalKeys`
  [ ((controlMask .|. mod1Mask, xK_l), spawn "xscreensaver-command -lock")
  ]


mydzen :: LayoutClass l Window
     => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
mydzen conf = statusBar ("dzen2 " ++ flags) myPP toggle conf
 where
    font  = " -fn 'Hack-9'"
    color = " -fg '#EEEEEE' -bg '#000000' "
    misc  = " -e 'onstart=lower' -ta l "
    flags = font ++ color ++ misc
    toggle XConfig{modMask = modm} = (modm, xK_b)


myPP :: PP
myPP = defaultPP { ppCurrent  = dzenColor "black"   "white" . pad
                 , ppVisible  = dzenColor "black"   "#555555" . pad
                 , ppHidden   = dzenColor "#cccccc" "black" . pad
                 , ppHiddenNoWindows = const ""
                 , ppUrgent   = dzenColor "red" "yellow" . pad
                 , ppWsSep    = ""
                 , ppSep      = " | "
                 , ppLayout   = dzenColor "#cccccc" "black" .
                                (\ x -> pad $ case x of
                                          "TilePrime Horizontal" -> "TTT"
                                          "TilePrime Vertical"   -> "[]="
                                          "Hinted Full"          -> "[ ]"
                                          _                      -> x
                                )
                 , ppTitle    = \_ -> "" -- ("^bg(#000000) " ++) . dzenEscape
                 , ppExtras   = [ fixedWidthL AlignCenter " " 80 $ logTitle
                                , dzenColorL "green" "#2A4C3F"
                                    $ wrapL "[" "]"
                                    $ fixedWidthL AlignRight " " 5 battery
                                , date "%a, %d. %b"
                                ]
                 }
