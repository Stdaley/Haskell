--------------------------------------------------------------------------------
-- Part 1
-- Shania Daley
-- Lianna Morelli
-- Homework 3                                                                  
                                                                          
--Exercise 1

rmChar :: Char -> String -> String
rmChar _ "" = ""
rmChar c xs = filter (\x -> x /= c) xs

--Excercise 2

rmCharsRec :: String -> String -> String
rmCharsRec _ "" = ""
rmCharsRec "" rst = rst
rmCharsRec (x:xs) s2 = rmCharsRec xs (rmChar x s2)

rmCharsFold :: String -> String -> String
rmCharsFold _ "" = ""
rmCharsFold "" rst = rst
rmCharsFold s1 s2 = foldr rmChar s2 s1 

--Excercise 3

andRec :: [Bool] -> Bool
andRec [] = True
andRec (b:bs) 
       | b == True = andRec bs
       | otherwise = False

andFold :: [Bool] -> Bool
andFold [] = True
andFold bs = foldr (&&) True bs

--Exercise 4

same :: [Int] -> Bool
same xs = and (zipWith (==) xs  (tail xs))