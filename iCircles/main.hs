import Data.List
import Data.List.Split

-- Curve Label
type Label = String


-- Abstract Curve
data C = C Label

instance Eq C where
    x == y = ((l x) == (l y))

instance Show C where
    show (C label) = label

toC :: Char -> C
toC char = C $ show char

l :: C -> Label
l (C label) = label


-- Abstract Basic Region
data B = B [C]

instance Show B where
    show (B curves) = show curves

toB :: String -> B
toB s = B (map toC $ nub s)


-- Abstract Description
data D = D [C] [B] (C -> Label)

instance Show D where
    show (D curves region f) = "D = (C, B, l), where\n"
                            ++ "C = " ++ show curves ++ "\n"
                            ++ "B = " ++ show region ++ "\n"
                            ++ "l = " ++ "\n"

toD :: String -> D
toD informal = D (map toC $ delete ' ' $ nub informal) (map toB $ (splitOn " " informal) ++ [" "]) l

removeCurve :: C -> D -> D
removeCurve k (D curves b f) = D (delete k curves) b f

-- Decomposition
dec :: D -> (D -> C) -> [D]
dec (D curves regions lbl) f  = if null curves then [D [] [] l]
                              else arg : dec (removeCurve (f arg) arg) f
                              where arg = D curves regions lbl

                              
naiveDecomp :: D -> C
naiveDecomp (D curves _ _) = head curves

main = do
          input <- getLine
          putStrLn $ show $ dec (toD input) naiveDecomp
          