import Day1

import Test.Hspec
-- import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        it "given `100\\n1721\\n979\\n366\\n299\\n675\\n1456`, day1_1 should return 514579" $ do
            day1_1 "100\n1721\n979\n366\n299\n675\n1456" `shouldBe` "514579"
        it "given `100\\n1721\\n979\\n366\\n299\\n675\\n1456`, day1_2 should return 241861950" $ do
            day1_2 "100\n1721\n979\n366\n299\n675\n1456" `shouldBe` "241861950"


