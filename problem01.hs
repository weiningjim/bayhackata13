import Test.QuickCheck

-- problem 1: calculate total score from a list
bowling :: [Int] -> Int
bowling = sum . concat . (take 10) . frames

-- problem 2: display scores
scoreStr :: [Int] -> String
scoreStr = concatMap frame2str . frames where
  showNum 0 = "-"
  showNum x = show x
  frame2str (10:_) = "X "
  frame2str [x] = showNum x ++ " "
  frame2str (x:y:_)
    | x + y == 10 = showNum x ++ "/"
    | otherwise = showNum x ++ showNum y


-- totalStr :: [Int] -> String


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
  print $ map scoreStr tests


perfect y =
  not (null y) ==>
  (maximum y <= 10) ==>
  bowling y <= 300
