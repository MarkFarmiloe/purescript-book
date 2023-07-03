module Test.MySolutions
  ( answer
  , circleArea
  , diagonal
  , leftoverCents
  )
  where

import Prelude

import Data.Foldable (sum)
import Data.Int (rem)
import Data.List (List, range, filter)
import Data.Number (pi, sqrt)

ns:: Int -> List Int
ns n = range 0 (n - 1)

factorOf3or5:: List Int -> List Int
factorOf3or5 = filter (\n -> mod n 3 == 0 || mod n 5 == 0)

answer:: Int -> Int
answer n = sum (factorOf3or5 (ns n))

diagonal:: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea:: Number -> Number
circleArea r = pi * r * r

leftoverCents:: Int -> Int
leftoverCents n = rem n 100
