
type Stack a = [a]

push :: a -> Stack a -> Stack a
push x stack = stack ++ [x]

pop :: Stack a -> Stack a
pop (x:xs) = xs

names = [ "Ann", "Bob", "Cait" ]

main = do
        let result = push "Dave" names
        let result2 = pop $pop $ pop $ pop $ pop result
        print result2