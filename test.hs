doubleMe x = x + x
sumUs x y = x + y

isEven x = if mod x 2 == 0
            then True
            else False
            
bdayList = [23, 29, 12]

printHelloWorld = putStrLn "Hello World"

data Diabolo = Diabolo Int Int

size :: Diabolo -> Int -> Int
size (Diabolo w h) (lineNum) = if lineNum <= div h 2
                               then max (w - 2*lineNum) 2
                               else size (Diabolo w h) (h - lineNum - 1)

line :: Diabolo -> Int -> String
line (Diabolo w h) (lineNum) = spaces (div (w - (size (Diabolo w h) lineNum)) 2) "."
                            ++ spaces (size (Diabolo w h) lineNum) "@"
                            ++ spaces (div (w - (size (Diabolo w h) lineNum)) 2) "."
                                    
draw :: Diabolo -> Int -> IO()
draw (Diabolo w h) (lineNum) = if lineNum == h
                               then putStrLn ("Done")
                               else do putStrLn (line (Diabolo w h) lineNum)
                                       draw (Diabolo w h) (lineNum + 1)

                                       
    


main = printHelloWorld