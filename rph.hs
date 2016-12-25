solveRPN :: String -> Float
solveRPN = head . foldl f [] . words
    where   f (x:y:ys) '+' = (y + x) : ys
            f (x:y:ys) '-' = (y - x) : ys
            f (x:y:ys) '*' = (y * x) : ys
            f (x:y:ys) '/' = (y / x) : ys
            f xs numStr = read numStr:xs