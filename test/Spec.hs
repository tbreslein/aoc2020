{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
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

day4_testPassports :: String
day4_testPassports = [r|ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in|]

day4_invalidPassports :: String
day4_invalidPassports = [r|eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007|]

day4_validPassports :: String
day4_validPassports = [r|pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719|]

day6_test :: String
day6_test = [r|abc

a
b
c

ab
ac

a
a
a
a

b|]

day7_test1 :: String
day7_test1 = [r|light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.|]

day7_test2 :: String
day7_test2 = [r|shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.|]

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
    describe "Day 4" $ do
        it "given the test passports, Task 1 should return 2" $ do
            day4_1 day4_testPassports `shouldBe` show 2
        it "given the valid test passports, Task 2 should return 4" $ do
            day4_2 day4_validPassports `shouldBe` show 4
        it "given the invalid test passports, Task 2 should return 4" $ do
            day4_2 day4_invalidPassports `shouldBe` show 0
    describe "Day 5" $ do
        it "given `BFFFBBFRRR`, Task 1 should return 567" $ do
            day5_1 "BFFFBBFRRR" `shouldBe` show 567
        it "given `FFFBBBFRRR`, Task 1 should return 119" $ do
            day5_1 "FFFBBBFRRR" `shouldBe` show 119
        it "given `BBFFBBFRLL`, Task 1 should return 820" $ do
            day5_1 "BBFFBBFRLL" `shouldBe` show 820
        it "given the previous test cases, Task 1 should return 820" $ do
            day5_1 "BFFFBBFRRR\nBBFFBBFRLL\nFFFBBBFRRR" `shouldBe` show 820
    describe "Day 6" $ do
        it "given the example set, Task 1 should return 11" $ do
            day6_1 day6_test `shouldBe` show 11
        it "given the example set, Task 2 should return 6" $ do
            day6_2 day6_test `shouldBe` show 6
    describe "Day 7" $ do
        it "given the example set, Task 1 should return 4" $ do
            day7_1 day7_test1 `shouldBe` show 4 
        it "given the first example set, Task 2 should return 32" $ do
            day7_2 day7_test1 `shouldBe` show 32
        it "given the second example set, Task 2 should return 126" $ do
            day7_2 day7_test2 `shouldBe` show 126