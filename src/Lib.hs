{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Monad         (void)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.Posix.IO       (stdInput)
import           System.Posix.Terminal
import           Turtle

confFilePath :: IsString a => a
confFilePath = "/usr/local/etc/cntlm.conf"

data Key = PassLM | PassNT | PassNTLMv2 deriving (Show, Eq)

keyPattern :: Pattern Key
keyPattern = ("PassLM" *> return PassLM) <|>
              ("PassNT" *> return PassNT) <|>
              ("PassNTLMv2" *> return PassNTLMv2)

keyHashPattern :: Pattern (Key, Text)
keyHashPattern = do
  key <- keyPattern
  void spaces1
  hash <- count 32 hexDigit
  return (key, T.pack hash)

matchKeyHash :: Line -> [(Key, Text)]
matchKeyHash = match (prefix keyHashPattern) . lineToText

replaceHashPattern :: [(Key, Text)] -> Pattern Text
replaceHashPattern [] = empty
replaceHashPattern ((key, newHash) : _) =
  let keyText = T.pack (show key)
  in do
     ss0 <- spaces
     void $ text keyText
     ss1 <- spaces1
     void $ count 32 hexDigit
     ending <- comment <|> spaces
     return $ ss0 <> keyText <> ss1 <> newHash <> ending

comment :: Pattern Text
comment = do
  ss <- spaces1
  void $ char '#'
  commentText <- star anyChar
  return $ ss <> "#" <> commentText

lineToPattern :: Line -> Pattern Text
lineToPattern = replaceHashPattern . matchKeyHash

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
  let pattern = lineToPattern newHashes
  liftIO $ inplaceEntire pattern confFilePath
