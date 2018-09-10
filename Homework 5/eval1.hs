--Shania Daley
import AS1
import Parser1 (aparse)
import Debug.Trace

fixMe from = error ("Please fix " ++ from)

-- For a simple trace, uncomment the next line
-- eval e | trace ("entering eval with arg: "++ show e) False = undefined 
eval (Num  n)        = n
eval (Add  a1 a2)    = (eval a1) + (eval a2)
eval (Sub  a1 a2)    = (eval a1) - (eval a2)
eval (Mult a1 a2)    = (eval a1) * (eval a2)
eval (Div  a1 a2)
              |(eval a2) /= 0 = (eval a1) `div` (eval a2) --if a2 is not equal to 0 then no error
              |otherwise = error "Cannot divide by 0"     --otherwise throw the error
              
eval (Cond a1 a2 a3)
              |(eval a1) == 0 = (eval a3)       --conditional evaluation based on the rules. 
              |(eval a1) /= 0 = (eval a2)




------------------------------------------------------------------------
-- run e 
--   parses e, evaluates e, prints the answer
--   Try: (run "2+3*5")
run :: String -> IO ()
run etxt = do { let e = aparse etxt
              ; putStrLn $ "Evaluating: " ++ show e
              ; let val = eval e
              ; putStrLn $ "    Result: " ++ show val
              }

-- read-eval-print
rep :: IO ()
rep = do { etxt <- getLine;
         ; let e = aparse etxt
         ; let val = eval e
         ; putStrLn $ "Evaluates to:\t"++ show val
         }
