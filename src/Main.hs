{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Turtle
import System.Posix.Terminal
import System.Posix.IO (stdInput)

confFilePath :: IsString a => a
confFilePath = "/usr/local/etc/cntlm.conf"

data Key = PassLM | PassNT | PassNTLMv2 deriving (Show, Eq)

hashPattern :: Pattern Key
hashPattern = ("PassLM" *> return PassLM) <|>
              ("PassNT" *> return PassNT) <|>
              ("PassNTLMv2" *> return PassNTLMv2)

keyHashPattern :: Pattern (Key, Text)
keyHashPattern = do
  key <- hashPattern
  void spaces
  hash <- count 32 hexDigit
  return (key, T.pack hash)

matchKeyHash :: Line -> [(Key, Text)]
matchKeyHash = match (prefix keyHashPattern) . lineToText

replaceHashPattern :: [(Key, Text)] -> Pattern Text
replaceHashPattern [] = empty
replaceHashPattern ((key, newHash) : _) =
  let keyText = T.pack (show key)
  in do
     key <- text keyText
     ss <- spaces
     void $ count 32 hexDigit
     return $ keyText <> ss <> newHash

getPassword :: IO Line
getPassword = do
  tc <- getTerminalAttributes stdInput
  setTerminalAttributes stdInput (withoutMode tc EnableEcho) Immediately
  loop tc
  where
    loop tc' = do
      TIO.putStrLn "Enter password: "
      password1 <- TIO.getLine
      TIO.putStrLn "Re-enter password: "
      password2 <- TIO.getLine
      if password1 == password2
        then setTerminalAttributes stdInput tc' Immediately *>
               return (unsafeTextToLine password1)
        else stdout "Passwords did not match" *> loop tc'

sedCommand :: Line -> Shell ()
sedCommand password = do
  newHashes <- inshell "cntlm -H" $ return password
  let pattern = replaceHashPattern . matchKeyHash $ newHashes
  liftIO $ inplacePrefix pattern confFilePath

main :: IO ()
main = do
  void $ shell "pkill cntlm" empty
  password <- getPassword
  sh $ sedCommand password
  void $ shell "cntlm" empty
