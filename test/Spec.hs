import Day1

import Test.Hspec
-- import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        it "given `1010\\n2000\\n20`, Task 1 should return 40000" $ do
            day1_1 "1010\n2000\n20" `shouldBe` show (2000 * 20)
        it "given `100\\n1721\\n979\\n366\\n299\\n675\\n1456`, Task 1 should return 514579" $ do
            day1_1 "100\n1721\n979\n366\n299\n675\n1456" `shouldBe` show (1721 * 299)
        it "given `100\\n1721\\n979\\n366\\n299\\n675\\n1456`, Task 2 should return 241861950" $ do
            day1_2 "100\n1721\n979\n366\n299\n675\n1456" `shouldBe` show (979 * 366 * 675)


