module InputUtilsSpec (spec) where

import Test.Hspec
import InputUtils

spec :: Spec
spec = do
    describe "dos2unix" $ do
        context "when string has no \r characters" $ do
            it "returns original string" $ do
                dos2unix "teststring" `shouldBe` "teststring"
        context "when string has \r characters" $ do
            it "returns string with no \r characters" $ do
                dos2unix "teststring\r" `shouldBe` "teststring"

    describe "dropBom" $ do
        context "when no BOM present" $ do
            it "returns original string" $ do
                dropBom "teststring" `shouldBe` "teststring"
        context "when BOM is present" $ do
            it "returns the string ignoring BOM" $ do
                dropBom "\xfeffteststring" `shouldBe` "teststring"
