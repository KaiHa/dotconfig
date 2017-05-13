{-# LANGUAGE ExistentialQuantification #-}

module Adapt2Environment (adapt2environment) where

import           Data.List
import           XMonad
import           XMonad.Util.Run


adapt2environment :: X ()
adapt2environment =
  isConnected DP1 >>= \c ->
  if c
    then xrandr [m DP1,   m Auto, m LVDS1, m Off]
    else xrandr [m LVDS1, m Auto, m DP1  , m Off]
  where
    m :: Option_ a => a -> Option
    m = MkOption


isConnected :: Output -> X Bool
isConnected o = any matchesOutput <$> xrandr'
  where
    xrandr' = lines <$> runProcessWithInput "xrandr" ["--query", "--dryrun"] ""
    matchesOutput = ((show o ++ " connected") `isPrefixOf`)


xrandr :: [Option] -> X ()
xrandr o = spawn $ "xrandr " ++ unwords (concatMap toOption o)


data Output = LVDS1
            | DP1
            deriving (Eq, Ord, Show)

instance Option_ Output where
  toOption a = ["--output", show a]


data Mode = Mode1024x768
          | Mode1366x768  -- x230 Laptop
          | Mode2560x1440 -- Dell Display
          | Auto
          | Off
          deriving (Eq, Ord)

instance Option_ Mode where
  toOption Mode1024x768  = ["--mode", "1024x768"]
  toOption Mode1366x768  = ["--mode", "1366x768"]
  toOption Mode2560x1440 = ["--mode", "2560x1440"]
  toOption Auto          = ["--auto"]
  toOption Off           = ["--off"]



class Option_ a where
  toOption :: a -> [String]

data Option = forall a . Option_ a => MkOption a
instance Option_ Option where
  toOption (MkOption a) = toOption a
