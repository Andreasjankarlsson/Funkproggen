
import System.Random
import Data.List (groupBy, sort,intercalate)
import Data.Function (on)

exampleSudoku :: [(String,Int)]
exampleSudoku = [("A1",1),("A2",4),("A3",3),("A4",2),("B1",3),("B2",2),("B3",1),("B4",4),("C1",4),("C2",1),("C3",2),("C4",3),("D1",2),("D2",3),("D3",4),("D4",1)]

--Task 1
giveMeANumber :: IO ()
giveMeANumber = do
    putStrLn "Enter two integers:"
    input1 <- getLine
    input2 <- getLine
    let a = read input1 ::Int
    let b = read input2 ::Int
    randomNum <- randomRIO (a, b)
    putStrLn (show randomNum)

--Task 2

--Task 3

printSuduko sudoku = do
    let groupByChar = getBoardOnlyNumbers sudoku
    let b = intMatriceToStringList groupByChar
    let c = stringListToString b
    
    putStrLn (c)

    
    
--Group units with regard of their char.
groupByFirstChar :: [(String, Int)] -> [[(String, Int)]]
groupByFirstChar sudoku = groupBy ((==) `on` (head . fst)) (sort sudoku)

--Recieve only the numbers of a unit-row
getBoardOnlyNumbers :: [(String, Int)] -> [[Int]]
getBoardOnlyNumbers sudoku = map (\k ->(map (\p -> snd p) (k) ) ) (groupByFirstChar sudoku)

--Make a intlist to a string.
intListToString :: [Int] -> String
intListToString list = unwords ( map show list )

intMatriceToStringList :: [[Int]] -> [String] 
intMatriceToStringList list = map (\p-> intListToString p) list

stringListToString :: [String] -> String
stringListToString list = unlines list


--findFaultyUnits (x:xs)
--    | validUnit [x] (x:xs) =  x : validUnit (xs)
--    | otherwise = (fst x, 0) : validUnit (xs)