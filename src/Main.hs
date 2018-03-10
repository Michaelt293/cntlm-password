{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad   (void)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Turtle

confFilePath :: IsString a => a
confFilePath = "/usr/local/etc/cntlm.conf"

keyHashes :: Text -> (Text, Text)
keyHashes l = case (T.splitAt 16 . T.take 48) l of
              (k, h) -> (T.stripEnd k, h)

sedCommand :: (Text, Text) -> IO ()
sedCommand (key, hash) = do
  let command = mconcat [ "sed -E '/"
                        , key
                        , " +/ s/[A-F0-9]{32}/"
                        , hash
                        , "/g' "
                        , confFilePath
                        ]
  -- Horrible hack
  newHash <- strict $ inshell command empty
  TIO.writeFile confFilePath newHash

main :: IO ()
main = do
  void $ shell "pkill cntlm" empty
  TIO.putStr "Enter password: "
  out <- strict $ inshell "cntlm -H" stdin
  let outLines = T.lines out
  let hashes = if length outLines == 4
                 then keyHashes <$> tail outLines
                 else error "Output error: 4 lines expected from cntlm -H"
  mapM_ sedCommand hashes
  void $ shell "cntlm" empty
