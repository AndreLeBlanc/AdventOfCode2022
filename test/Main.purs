module Test.Main where

import Prelude

import Effect (Effect)
import Main (runCommand)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main ∷ Effect Unit
main =
  runTest do
    suite "Advent of code 2022" do
      test "day 1" do
        Assert.equal (runCommand [ "", "", "2022", "1" ])
          $ "part one: 70509, part two: 208567"
      test "day 2" do
        Assert.equal (runCommand [ "", "", "2022", "2" ])
          $ "part one: (Just 11449), part two: (Just 13187)"
      test "day 3" do
        Assert.equal (runCommand [ "", "", "2022", "3" ])
          $ "part one: 7597, part two: 2607"
      test "day 4" do
        Assert.equal (runCommand [ "", "", "2022", "4" ])
          $ "part one: 605, part two: 914"
      test "day 5" do
        Assert.equal (runCommand [ "", "", "2022", "5" ])
          $ "part one: \"JCMHLVGMG\", part two: \"LVMRWSSPZ\""
      test "day 6" do
        Assert.equal (runCommand [ "", "", "2022", "6" ])
          $ "part one: \"1707\", part two: \"3697\""
      test "day 7" do
        Assert.equal (runCommand [ "", "", "2022", "7" ])
          $ "part one: 1334506.0, part two: 7421137.0"
      test "day 8" do
        Assert.equal (runCommand [ "", "", "2022", "8" ])
          $ "part one: 1736, part two: 268800"
      test "day 9" do
        Assert.equal (runCommand [ "", "", "2022", "9" ])
          $ "part one: 6503, part two: 2724"
      test "day 10" do
        Assert.equal (runCommand [ "", "", "2022", "10" ])
          $
            "part one: \"14720\", part two: \
            \\"#### #### ###  ###  ###  #### #### #### \
            \#       # #  # #  # #  # #       # #    \
            \###    #  ###  #  # ###  ###    #  ###  \
            \#     #   #  # ###  #  # #     #   #    \
            \#    #    #  # #    #  # #    #    #    \
            \#    #### ###  #    ###  #    #### #    \""

