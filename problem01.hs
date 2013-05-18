bowling :: [Int] -> Int
bowling = sum . concat . (take 10) . frames

frames :: [Int] -> [[Int]]
frames [] = []
frames [x] = [[x]]
frames [x, y]
  | x == 10 = [[x,y], [y]]
  | otherwise = [[x,y]]
frames (x1:x2:x3:xs)
  | x1 == 10 = [x1,x2,x3] : frames (x2:x3:xs)
  | x1 + x2 == 10 = [x1,x2,x3] : frames (x3:xs)
  | otherwise = [x1,x2] : frames (x3:xs)

main :: IO ()
main = do
  let tests = [[],
               [1,2,3,4],
               [7,3,8,1],
               [10,8,1],
               [1,2,3],
               [7,3],
               [10],
               [10,3]]
      ans = map bowling tests
  print ans
