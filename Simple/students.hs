import Data.List
import Data.Ord

data Student = Student String Int deriving Show

len :: Student -> Int
len (Student name age) = length name

show :: Student -> String
show (Student name _) = name

students = [ Student "Adam" 30, Student "Bob" 25, Student "Greg" 23, Student "Dave" 21, Student "Ann" 29]

age:: Student -> Int
age (Student name age) = age

oldest :: [Student] -> Student
oldest students = maximumBy (comparing (\s -> age s)) students

youngest :: [Student] -> Student
youngest students = minimumBy (comparing (\s -> age s)) students

hilo :: [Student] -> (Student, Student)
hilo students = head $ zip [oldest students] [youngest students]

main :: IO()
main = print $ hilo students

-- main = print $ students !! 0
-- main = print $ filter (\s -> odd $ len s) students