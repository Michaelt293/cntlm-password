{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad   (void)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Turtle

keyHashes :: Text -> (Text, Text)
keyHashes l = case (T.splitAt 16 . T.take 48) l of
              (k, h) -> (T.stripEnd k, h)

sedCommand :: (Text, Text) -> IO ()
sedCommand (key, hash) = do
  let command = mconcat [ "sed -E '/"
                        , key
                        , " +/ s/[A-F0-9]{32}/"
                        , hash
                        , "/g' /usr/local/etc/cntlm.conf"
                        ]
  -- Horrible hack
  newHash <- strict $ inshell command empty
  TIO.writeFile "/usr/local/etc/cntlm.conf" newHash

main :: IO ()
main = do
  void $ shell "pkill cntlm" empty
  TIO.putStr "Enter password: "
  out <- strict $ inshell "cntlm -H" stdin
  let outLine = T.lines out
  let hashes = if length outLine == 4
                 then keyHashes <$> tail outLine
                 else error "Output not formatted as expected: 4 lines expected"
  mapM_ sedCommand hashes
