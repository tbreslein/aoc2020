{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Day1
import Day2
import Day3
import Test.Hspec
import Text.RawString.QQ

day3_testForest :: String
day3_testForest = [r|..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#|]

main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        -- it "given `1010\\n2000\\n20`, Task 1 should return 40000" $ do
        --     day1_1 "1010\n2000\n20" `shouldBe` show (2000 * 20)
        it "given `100\\n1721\\n979\\n366\\n299\\n675\\n1456`, Task 1 should return 514579" $ do
            day1_1 "100\n1721\n979\n366\n299\n675\n1456" `shouldBe` show (1721 * 299)
        it "given `100\\n1721\\n979\\n366\\n299\\n675\\n1456`, Task 2 should return 241861950" $ do
            day1_2 "100\n1721\n979\n366\n299\n675\n1456" `shouldBe` show (979 * 366 * 675)
    describe "Day 2" $ do
        it "given `1-3 a: abcde\\n1-3 b: cdefg\\n2-9 c: ccccccccc`, Task 1 should return 2" $ do
            day2_1 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" `shouldBe` show 2
        it "given `1-3 a: abcde\\n1-3 b: cdefg\\n2-9 c: ccccccccc`, Task 2 should return 1" $ do
            day2_2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" `shouldBe` show 1
    describe "Day 3" $ do
        it "given the test forest, Task 1 should return 7" $ do
            day3_1 day3_testForest `shouldBe` show 7
        it "given the test forest, Task 2 should return 336" $ do
            day3_2 day3_testForest `shouldBe` show 336