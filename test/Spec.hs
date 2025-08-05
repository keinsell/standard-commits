import Data.Either (isLeft)
import Lib (Scope (..), Verb (..), parseScope, parseVerb, parseReason, Reason(..))
import Test.Hspec
import Text.Parsec (parse)

parseVerbSpec :: Spec
parseVerbSpec = describe "parse verb" $ do
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

parseScopeSpec :: Spec
parseScopeSpec =   describe "parse scope" $ do
  it "should parse no scope" $ parse parseScope "" "" `shouldBe` Right Nothing
  it "should parse executable without sub" $ parse parseScope "" "(exe)" `shouldBe` Right (Just (Executable Nothing))
  it "should parse backend library without sub" $ parse parseScope "" "(lib)" `shouldBe` Right (Just (BackendLibrary Nothing))
  it "should parse backend library with sub" $ parse parseScope "" "(lib/api)" `shouldBe` Right (Just (BackendLibrary (Just "api")))
  it "should parse test with sub" $ parse parseScope "" "(test/foo)" `shouldBe` Right (Just (Testing (Just "foo")))
  it "should parse documentation without sub" $ parse parseScope "" "(docs)" `shouldBe` Right (Just (Documentation Nothing))
  it "should parse ci with sub" $ parse parseScope "" "(ci/bar)" `shouldBe` Right (Just (ContinousIntegration (Just "bar")))
  it "should parse continuous delivery with sub" $ parse parseScope "" "(cd/baz)" `shouldBe` Right (Just (ContinuousDelivery (Just "baz")))
  it "should fail on unknown scope" $ parse parseScope "" "(unknown)" `shouldSatisfy` isLeft

parseReasonSpec :: Spec
parseReasonSpec = describe "parse reason" $ do
  it "should parse no reason when input isn't bracketed" $
    parse parseReason "" "rel" `shouldBe` Right Nothing

  -- valid reasons
  it "should parse introduction" $
    parse parseReason "" "[int]" `shouldBe` Right (Just Introduction)
  it "should parse preliminary" $
    parse parseReason "" "[pre]" `shouldBe` Right (Just Preliminary)
  it "should parse efficiency" $
    parse parseReason "" "[eff]" `shouldBe` Right (Just Efficiency)
  it "should parse reliability" $
    parse parseReason "" "[rel]" `shouldBe` Right (Just Reliability)
  it "should parse compatibility" $
    parse parseReason "" "[cmp]" `shouldBe` Right (Just Compatibility)
  it "should parse temporary" $
    parse parseReason "" "[tmp]" `shouldBe` Right (Just Temporary)
  it "should parse experiment" $
    parse parseReason "" "[exp]" `shouldBe` Right (Just Experiment)
  it "should parse security" $
    parse parseReason "" "[sec]" `shouldBe` Right (Just Security)
  it "should parse upgrade" $
    parse parseReason "" "[upg]" `shouldBe` Right (Just Upgrade)
  it "should parse user-experience" $
    parse parseReason "" "[ux]" `shouldBe` Right (Just UserExperience)
  it "should parse policy" $
    parse parseReason "" "[pol]" `shouldBe` Right (Just Policy)
  it "should parse styling" $
    parse parseReason "" "[sty]" `shouldBe` Right (Just Styling)

  -- invalid reason input should error
  it "should fail on unknown reason" $
    parse parseReason "" "[unknown]" `shouldSatisfy` isLeft

main :: IO ()
main = hspec $ do
  parseVerbSpec
  parseScopeSpec
  parseReasonSpec
