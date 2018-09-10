------------------------------------------------------------------------
-- Homework 10 CIS 352, Spring 2018
-- Authors: Shania Daley, Lianna Morelli 
------------------------------------------------------------------------
import Data.Char
import Data.List

getInt :: IO Int
getInt = do line <- getLine
    	     return (read line :: Int)

showHisto :: IO()
showHisto = do putStrLn("")
            t <- getInt;
            if( t< 0 )
                then return ()
            else
                showHisto

ask :: String -> IO Char
ask x = do putStrLn ( x ++ "\n")
      	   	    res <- getLine
		    if (res == "")
		       then return 'n'
		    else
			return (head (res))