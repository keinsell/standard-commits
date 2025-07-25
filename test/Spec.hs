-- Test suite for parseVerb function using HSpec

import Data.Either (isLeft)
import Lib (Scope (..), Verb (..), parseScope, parseVerb)
import Test.Hspec
import Text.Parsec (parse)

main :: IO ()
main = hspec $ do
  describe "parse verb" $ do
    it "should parse addition" $
      parse parseVerb "" "add" `shouldBe` Right Add
    it "should parse removal" $
      parse parseVerb "" "rem" `shouldBe` Right Remove
    it "should parse refactor" $
      parse parseVerb "" "ref" `shouldBe` Right Refactor
    it "should parse fix" $
      parse parseVerb "" "fix" `shouldBe` Right Fix
    it "should parse undo" $
      parse parseVerb "" "undo" `shouldBe` Right Undo
    it "should parse release" $
      parse parseVerb "" "release" `shouldBe` Right Release
    it "should fail on unknown verb" $
      parse parseVerb "" "unknown" `shouldSatisfy` isLeft
  -- TODO: Is there sort of matrix testing for such cases?
  describe "parse scope" $ do
    it "should parse no scope" $ parse parseScope "" "" `shouldBe` Right Nothing
    it "should parse executable without sub" $ parse parseScope "" "(exe)" `shouldBe` Right (Just (Executable Nothing))
    it "should parse backend library without sub" $ parse parseScope "" "(lib)" `shouldBe` Right (Just (BackendLibrary Nothing))
    it "should parse backend library with sub" $ parse parseScope "" "(lib/api)" `shouldBe` Right (Just (BackendLibrary (Just "api")))
    it "should parse test with sub" $ parse parseScope "" "(test/foo)" `shouldBe` Right (Just (Testing (Just "foo")))
    it "should parse documentation without sub" $ parse parseScope "" "(docs)" `shouldBe` Right (Just (Documentation Nothing))
    it "should parse ci with sub" $ parse parseScope "" "(ci/bar)" `shouldBe` Right (Just (ContinousIntegration (Just "bar")))
    it "should parse continuous delivery with sub" $ parse parseScope "" "(cd/baz)" `shouldBe` Right (Just (ContinuousDelivery (Just "baz")))
    it "should fail on unknown scope" $ parse parseScope "" "(unknown)" `shouldSatisfy` isLeft
