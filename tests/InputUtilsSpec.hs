module InputUtilsSpec (spec) where

import Test.Hspec
import InputUtils

spec :: Spec
spec = do
    describe "dos2unix" $ do
        context "when string has no \r characters" $
            it "returns original string" $
                dos2unix "teststring" `shouldBe` "teststring"
        context "when string has \r characters" $
            it "returns string with no \r characters" $
                dos2unix "teststring\r" `shouldBe` "teststring"

    describe "dropBom" $ do
        context "when no BOM present" $
            it "returns original string" $
                dropBom "teststring" `shouldBe` "teststring"
        context "when BOM is present" $
            it "returns the string ignoring BOM" $
                dropBom "\xfeffteststring" `shouldBe` "teststring"
