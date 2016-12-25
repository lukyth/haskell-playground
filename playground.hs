lucky 7 = "Number 7"
lucky 10 = "Number 10. Wayne Rooney!!!"
lucky _ = "Hello........"

factorial 0 = 1
factorial n = n * factorial (n - 1)

replicate' 0 _ = []
replicate' replica number = [number] ++ replicate' (replica - 1) number

replicate'' replica number
    |   replica == 0    = []
    |   otherwise       = number : replicate'' (replica - 1) number

reverse' [] = []
reverse' (item : items) = reverse' items ++ [item]

zip' [] _ = []
zip' _ [] = []
zip' (item_a : items_a) (item_b : items_b) = [(item_a, item_b)] ++ zip' items_a items_b

delete _ [] = []
delete number (head_number : numbers) = (if number == head_number then [] else [head_number]) ++ (delete number numbers)

permute [] = [[]]
permute xs = [x : ys | x <- xs, ys <- permute (delete x xs)]

hanoi n = hanoi' n 1 2 3
hanoi' 0 _ _ _ = []
hanoi' n f i t = (hanoi' (n - 1) f t i) ++ [(f, t)] ++ (hanoi' (n - 1) i f t)

list = [(i, j) | i <- [1, 2], j <- [1..4]]
inf_list = [(i, j) | i <- [1, 2], j <- [1..]]

sum' [] = 0
sum' (x:xs) = x + sum' xs

length' xs = sum' [1 | _ <- xs]

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyAddThreeTwice n = applyTwice (addThree 2 3) n

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' fn (x:xs) = fn x : map' fn xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' fn (x : xs) (y : ys) = fn x y : zipWith' fn xs ys

zipWith'' fn xs ys = map' (\(a,b) -> fn a b) (zip' xs ys)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

collatz 1 = [1]
collatz n
    | even n    = n : collatz (n `div` 2)
    | odd n     = n : collatz (n * 3 + 1)

numLongerChainThan15 = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

numLongerChainThan n = length (filter isLonger (map collatz [1..100]))
    where isLonger xs = length xs > n

addThree' = \x y z -> x + y + z
addThree'' = \x -> \y -> \z -> x + y + z
