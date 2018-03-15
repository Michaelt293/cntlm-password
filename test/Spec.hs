{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib
import           Test.Hspec
import           Turtle

main :: IO ()
main = hspec $ do
  describe "keyPattern" $ do
    it "PassLM" $
      match keyPattern "PassLM" `shouldBe` [PassLM]
    it "PassNT" $
      match keyPattern "PassNT" `shouldBe` [PassNT]
    it "PassNTLMv2" $
      match keyPattern "PassNTLMv2" `shouldBe` [PassNTLMv2]
  describe "keyHashPattern should collect the key and hash as a pair" $ do
    it "\"Password:\" does not match" $
      match keyHashPattern "Password:" `shouldBe` []
    it "PassLM          24552C1592F6981C552C4BCA4AEBFB11" $
      match keyHashPattern "PassLM          24552C1592F6981C552C4BCA4AEBFB11"
       `shouldBe` [(PassLM, "24552C1592F6981C552C4BCA4AEBFB11")]
    it "PassNT          DFB3E9B191C1182D7D103403C0F43F59" $
      match keyHashPattern "PassNT          DFB3E9B191C1182D7D103403C0F43F59"
      `shouldBe` [(PassNT, "DFB3E9B191C1182D7D103403C0F43F59")]
    it "PassNTLMv2      251A353F8B4355D3DA42088E476E4070" $
      match keyHashPattern "PassNTLMv2      251A353F8B4355D3DA42088E476E4070"
      `shouldBe` [(PassNTLMv2, "251A353F8B4355D3DA42088E476E4070")]
  describe "matchKeyHashshould collect the key and hash as a pair from the expected output of \"cntlm -H\"" $ do
    it "\"PassLM          24552C1592F6981C552C4BCA4AEBFB11\" -> [(PassLM, \"24552C1592F6981C552C4BCA4AEBFB11\")]" $
      matchKeyHash "PassLM          24552C1592F6981C552C4BCA4AEBFB11"
       `shouldBe` [(PassLM, "24552C1592F6981C552C4BCA4AEBFB11")]
    it "\"PassNT          DFB3E9B191C1182D7D103403C0F43F59\" -> [(PassNT, \"DFB3E9B191C1182D7D103403C0F43F59\")]" $
      matchKeyHash "PassNT          DFB3E9B191C1182D7D103403C0F43F59"
      `shouldBe` [(PassNT, "DFB3E9B191C1182D7D103403C0F43F59")]
    it "\"PassNTLMv2      251A353F8B4355D3DA42088E476E4070    # Only for user 'user', domain 'domain'\" -> [(PassNTLMv2, \"251A353F8B4355D3DA42088E476E4070\")]" $
      matchKeyHash "PassNTLMv2      251A353F8B4355D3DA42088E476E4070    # Only for user 'user', domain 'domain'"
      `shouldBe` [(PassNTLMv2, "251A353F8B4355D3DA42088E476E4070")]
  describe "replaceHashPattern should create a pattern with the new hash" $ do
    it "PassLM" $
      match (replaceHashPattern [(PassLM, "24552C1592F6981C552C4BCA4AEBFB11")]) "PassLM          B6E55A7269274D963DF96B1DBD26DB48"
       `shouldBe` ["PassLM          24552C1592F6981C552C4BCA4AEBFB11"]
    it "PassNT" $
      match (replaceHashPattern [(PassNT, "DFB3E9B191C1182D7D103403C0F43F59")]) "PassNT          2CFD960F5E307C35DFB042A952BD9679"
      `shouldBe` ["PassNT          DFB3E9B191C1182D7D103403C0F43F59"]
    it "PassNTLMv2 - includes comment" $
      match (replaceHashPattern [(PassNTLMv2, "251A353F8B4355D3DA42088E476E4070")]) "PassNTLMv2      C420233A0A0EB4F388E6649B70B6A9DD    # Only for user 'user', domain 'domain'"
      `shouldBe` ["PassNTLMv2      251A353F8B4355D3DA42088E476E4070    # Only for user 'user', domain 'domain'"]
    it "No match with non matching keys" $
      match (replaceHashPattern [(PassNT, "251A353F8B4355D3DA42088E476E4070")]) "PassNTLMv2      C420233A0A0EB4F388E6649B70B6A9DD"
      `shouldBe` []
  describe "lineToPattern should take a line from the cntlm conf and produce a new pattern" $ do
    it "PassLM" $
      match (lineToPattern "PassLM          24552C1592F6981C552C4BCA4AEBFB11") "PassLM          B6E55A7269274D963DF96B1DBD26DB48"
       `shouldBe` ["PassLM          24552C1592F6981C552C4BCA4AEBFB11"]
    it "PassNT" $
      match (lineToPattern "PassNT          DFB3E9B191C1182D7D103403C0F43F59") "PassNT          2CFD960F5E307C35DFB042A952BD9679"
      `shouldBe` ["PassNT          DFB3E9B191C1182D7D103403C0F43F59"]
    it "PassNTLMv2 - includes comment" $
      match (lineToPattern "PassNTLMv2      251A353F8B4355D3DA42088E476E4070") "PassNTLMv2      C420233A0A0EB4F388E6649B70B6A9DD     # Only for user 'user', domain 'domain'"
      `shouldBe` ["PassNTLMv2      251A353F8B4355D3DA42088E476E4070     # Only for user 'user', domain 'domain'"]
    it "Commented out line should not match" $
      match (lineToPattern "PassNTLMv2      251A353F8B4355D3DA42088E476E4070") "# PassNTLMv2      C420233A0A0EB4F388E6649B70B6A9DD"
      `shouldBe` []
    it "Leading spaces should still result in a match" $
      match (lineToPattern "PassNTLMv2      251A353F8B4355D3DA42088E476E4070") "  PassNTLMv2      C420233A0A0EB4F388E6649B70B6A9DD"
      `shouldBe` ["  PassNTLMv2      251A353F8B4355D3DA42088E476E4070"]
    it "Malformed line should not match" $
      match (lineToPattern "PassNTLMv2      251A353F8B4355D3DA42088E476E4070") "PassNTLMv2      C420233A0A0EB4F388E6649B70B6A9DD Oh no! Forgot the pound :("
      `shouldBe` []
