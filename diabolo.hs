spaces :: Int -> String -> String
spaces n c = case n of 0 -> ""
                       _ -> c ++ spaces (n - 1) c

line1 :: Int -> Int -> String
line1 w lineNum = spaces (div (w - max (w - 2*lineNum) 2) 2) "."
                  ++ spaces (max (w - 2*lineNum) 2) "@"
                  ++ spaces (div (w - max (w - 2*lineNum) 2) 2) "."

             
diaboloHead :: Int -> Int -> Int -> [String]
diaboloHead w h lineNum
                        | lineNum <= div h 2 = line1 w lineNum : diaboloHead w h (lineNum+1)
                        | otherwise = []
              
diabolo :: Int -> Int -> [String]
diabolo w h = diaboloHead w h 0 ++ tail (reverse (diaboloHead w h 0))
              
main = putStrLn (unlines (diabolo 10 11))