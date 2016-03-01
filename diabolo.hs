import System.IO

line :: Int -> Int -> String
line w lineNum = replicate sizeDot '.' ++ replicate sizeDiabolo '@' ++ replicate sizeDot '.'
                  where sizeDiabolo = max (w - 2*lineNum) 2
                        sizeDot = div (w - sizeDiabolo) 2


diabolo :: Int -> Int -> [String]
diabolo w h = headDiabolo ++ tail (reverse headDiabolo)
                where headDiabolo = [line w lineNum | lineNum <- [0..div h 2]]

main = do
          hSetBuffering stdout LineBuffering
          putStrLn "Enter width:"
          w <- getLine
          putStrLn "Enter height:"
          h <- getLine
          putStrLn . unlines $ diabolo (read w) (read h)