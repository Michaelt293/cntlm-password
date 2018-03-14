{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           Lib
import           Turtle

main :: IO ()
main = do
  void $ shell "pkill cntlm" empty
  password <- getPassword
  sh $ sedCommand password
  void $ shell "cntlm" empty
