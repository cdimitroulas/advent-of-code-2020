import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
import Test.Hspec

day3TestInput :: [[Char]]
day3TestInput =
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm",
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929",
    "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm",
    "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"
  ]

main :: IO ()
main = hspec $ do
  -- describe "Day4 exercises" $ do
  --   it "returns the correct number of valid passports" $ do
  --     D4.getNoOfValidPassports day3TestInput `shouldBe` 2

  describe "Day5 exercises" $ do
    it "returns the correct seat ID given seat character string" $ do
      D5.getSeatId "BFFFBBFRRR" `shouldBe` 567
      D5.getSeatId "FFFBBBFRRR" `shouldBe` 119
      D5.getSeatId "BBFFBBFRLL" `shouldBe` 820

  describe "Day6 exercises" $ do
    describe "Part1" $ do
      it "returns the correct sum based on the example input" $ do
        input <- readFile "test/day6-example.txt"
        D6.getNumberOfYesAnswers input `shouldBe` 11

    describe "Part2" $ do
      it "returns the correct sum based on the example input" $ do
        input <- readFile "test/day6-example.txt"
        D6.getNumberOfYesAnswers' input `shouldBe` 6

  describe "Day7 exercises" $ do
    describe "Part1" $ do
      it "returns the correct number of bags based on the example input" $ do
        input <- readFile "test/day7-example.txt"
        D7.getNumberOfBagThatCanContainGold input `shouldBe` 4

  describe "Day8 exercises" $ do
    describe "Part1" $ do
      it "returns the correct accumulator value based on the example input" $ do
        input <- readFile "test/day8-example.txt"
        D8.getProgramAccumValue input `shouldBe` Just 5
