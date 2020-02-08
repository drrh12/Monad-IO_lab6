module Tests where

import Test.HUnit
import MainFunctions

test0 = TestCase (do result <- convertNumber 0
                     assertEqual "Result of test 1:" "zero" result)
test1 = TestCase (do result <- convertNumber 5
                     assertEqual "Result of test 2:" "five" result)
test2 = TestCase (do result <- convertNumber 10
                     assertEqual "Result of test 3:" "ten" result)
test3 = TestCase (do result <- convertNumber 11
                     assertEqual "Result of test 4:" "eleven" result)
test4 = TestCase (do result <- convertNumber 12
                     assertEqual "Result of test 5:" "twelve" result)
test5 = TestCase (do result <- convertNumber 13
                     assertEqual "Result of test 6:" "thirteen" result)
test6 = TestCase (do result <- convertNumber 17
                     assertEqual "Result of test 7:" "seventeen" result)
test7 = TestCase (do result <- convertNumber 20
                     assertEqual "Result of test 8:" "twenty" result)
test8 = TestCase (do result <- convertNumber 35
                     assertEqual "Result of test 9:" "thirty-five" result)
test9 = TestCase (do result <- convertNumber 100
                     assertEqual "Result of test 10:" "one hundred" result)
test10 = TestCase (do result <- convertNumber 1000
                      assertEqual "Result of test 11:" "one thousand" result)
test11 = TestCase (do result <- convertNumber 1000000
                      assertEqual "Result of test 12:" "one million" result)
test12 = TestCase (do result <- convertNumber 1000000000
                      assertEqual "Result of test 13:" "one billion" result)
test13 = TestCase (do result <- convertNumber 1234567890
                      assertEqual "Result of test 14:" "one billion two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety" result)
test14 = TestCase (do result <- convertNumber -1234567890
                      assertEqual "Result of test 15:" "minus one billion two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety" result)
test15 = TestCase (do result <- convertNumber 999999999
                      assertEqual "Result of test 16:" "nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine" result)

tests = TestList [TestLabel "Test with number 0" test0,
                  TestLabel "Test with number 5" test1,
                  TestLabel "Test with number 10" test2,
                  TestLabel "Test with number 11" test3,
                  TestLabel "Test with number 12" test4,
                  TestLabel "Test with number 13" test5,
                  TestLabel "Test with number 17" test6,
                  TestLabel "Test with number 20" test7,
                  TestLabel "Test with number 35" test8,
                  TestLabel "Test with number 100" test9,
                  TestLabel "Test with number 1000" test10,
                  TestLabel "Test with number 1000000" test11,
                  TestLabel "Test with number 1000000000" test12,
                  TestLabel "Test with number 1234567890" test13,
                  TestLabel "Test with number -1234567890" test14,
                  TestLabel "Test with number 999999999" test15]

main = runTestTT tests 