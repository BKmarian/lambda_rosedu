elevationList :: [Int]
elevationList = [0,1,0,2,1,0,1,3,2,1,2,1]

trappedWater :: [Int] -> Int
trappedWater xs = sum $ zipWith3 findLevel xs leftRunningMax rightRunningMax
  where
    leftRunningMax = scanl max 0 xs
    rightRunningMax = tail $ scanr max 0 xs

    getPositive x = if x > 0 then x else 0
    findLevel current leftMax rightMax = getPositive $ min leftMax rightMax - current
