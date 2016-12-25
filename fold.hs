sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

head' :: [a] -> a
head' = foldr1 (\x _ -> x)