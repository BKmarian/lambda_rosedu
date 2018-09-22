import Data.List (transpose)

genStream :: (Int, String) -> [String]
genStream (x, s) = cycle $ reverse $ s : replicate (x - 1) ""

genItem :: [String] -> String
genItem [] = ""
genItem (x:xs) = if concatTail /= "" then concatTail else x
  where concatTail = concat xs

fizzBuzz :: Int -> [(Int, String)] -> [String]
fizzBuzz n ps = take n $ map genItem $ transpose $ ints:(map genStream ps)
  where ints = map show [1..]
