------------------------------------------------------------------------
-- Homework 10 CIS 352, Spring 2018
-- Authors: Shania Daley
------------------------------------------------------------------------
import Data.Char
import Data.List
import System.IO 

stars :: [Int] -> String
stars x = concat (map help x)
        where
               help :: Int -> String
               help y = (replicate y '*') ++ "\n"

input :: IO Int
input = do line <- getLine;
            return (read line :: Int)

read :: IO [Int]
read = do i <- input
            if i < 0 then []
            else do is <- read
               return (i:is)
            
showHisto :: IO ()
showHisto = do putStrLn " Enter a number and press 0 to end: "
               i <- read
               helper i
                      where
                      helper :: [Int] -> IO()
                      helper i = "Histogram\n" ++ stars i)

ask :: String -> IO Char
ask x = do putStrLn (x ++ "\n")
        line <- getLine
        if null line
        then return 'n'
        else return (head line)