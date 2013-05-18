bowling :: [Int] -> Int
bowling = sum . concat . (take 10) . frames

frames :: [Int] -> [[Int]]
frames [] = []
frames [x] = [[x]]
frames [x, y] = [[x, y]]
frames (x1:x2:x3:xs)
  | x1 == 10 = [x1,x2,x3] : frames (x2:x3:xs)
  | x1 + x2 == 10 = [x1,x2,x3] : frames (x3:xs)
  | otherwise = [x1,x2] : frames (x3:xs)
