import Control.Applicative
import Data.List

stock :: [Int]
stock = [100, 20, 60]

benefits :: [Int]
benefits = [12, 10, 5, 3]

useAmount :: [[Int]]
useAmount = [[5, 3, 6], [10, 1, 1], [7, 0, 7], [0, 2, 5]]

productAmount :: [[Int]]
productAmount = filter constraint
                [[p1, p2, p3, p4] | p1 <- [0..maxAmounts !! 0]
                                  , p2 <- [0..maxAmounts !! 1]
                                  , p3 <- [0..maxAmounts !! 2]
                                  , p4 <- [0..maxAmounts !! 3]]
  where
    maxAmount = minimum . zipWith div stock . filter (/= 0)
    maxAmounts = map maxAmount useAmount
    constraint = and . zipWith (<=) stock . map sum . transpose .
                 zipWith (\xs y -> map (* y) xs) useAmount 

solve :: [[Int]]
solve = filter ((== (maximum $ sum . zipWith (*) benefits <$> productAmount)) . sum . zipWith (*) benefits) productAmount
