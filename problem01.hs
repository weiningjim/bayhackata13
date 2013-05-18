import Test.QuickCheck

-- problem 1: calculate total score from a list
bowling :: [Int] -> Int
bowling = sum . concat . (take 10) . frames

-- problem 2: display scores
scoreStr :: [Int] -> String
scoreStr xs = 
  let ys = take 10 $ frames xs
      (ps,qs) = splitAt 9 ys
  in concatMap frame2str ps ++ concatMap lastFrame qs
  
showNum 0 = "-"
showNum 10 = "X"
showNum x = show x
  
frame2str (10:_) = (showNum 10)++" "
frame2str [x] = showNum x ++ " "
frame2str (x:y:_)
  | x + y == 10 = showNum x ++ "/"
  | otherwise = showNum x ++ showNum y
                  
lastFrame [x] = showNum x ++ "  "
lastFrame [x,y] = concatMap showNum [x,y] ++ " "
lastFrame [x,y,z] = concatMap showNum [x,y,z]
    



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

tests = [[],
               [1,2,3,4],
               [7,3,8,1],
               [10,8,1],
               [1,2,3],
               [7,3],
               [10],
               [10,3]]
        
main :: IO ()
main = do
  let 
      ans = map bowling tests
  print ans
  print $ map scoreStr tests

perfect y ys = 
  ((maximum (y:ys)) <= 10) ==>
  bowling (y:ys) <= 30*(1 + length ys)
