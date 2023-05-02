--import Data.List
--Andreas Karlsson
--Sabah begum 

import System.Random
import Data.List (groupBy, sort,intercalate)
import Data.Function (on)


rows = "ABCD"
cols = "1234"
containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs


cross list1 list2 = [ [i,j] | i <- list1, j<-list2]


--Lab 1 Task 1
unitRow = [ cross [i] "1234" | i <-"ABCD"]

unitCol = [ cross "ABCD" [i] | i <-"1234"]

unitBox = [ cross i j | i <-["AB","CD"], j <-["12","34"]]

unitList = unitRow ++ unitCol ++ unitBox

--Lab 1 Task 2
filterUnitlist square = filter (containsElem square) unitList

--Lab 1 Task 3.
units square = (square, filterUnitlist square)

--Lab 1 Task 4
foldList = concat

--Lab 1 Task 5 
removeDuplicates (x:xs)
    | xs == [] = x : []
    | containsElem x xs == True = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

--Lab 1 Task 6
--peers :: [(String, [String])]
boxes= removeDuplicates (foldList unitList)


closestPeers box = removeDuplicates ( foldList ( filterUnitlist box))

peers :: [(String, [String])]
peers = [ (i, filter (/= i) $  closestPeers i ) | i <- boxes  ]


--Lab 2.

--Task 1
--lookup :: Eq a => a -> [(a, b)] -> Maybe b , a compaarison 

--Task 2
fromMayby:: a -> Maybe a -> a
fromMayby defVal maybeVal = case maybeVal of
    Just x -> x
    Nothing -> defVal

--Task 3
getPeers :: String -> [String]
getPeers box = fromMayby [] (lookup box peers)

-- Task 4
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing:xs) = justifyList xs
justifyList ((Just x): xs) = x : justifyList xs

-- Task 5
lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups xs ys = justifyList (map  (\x -> lookup x ys) xs) --Easier way to do then lamda functions?


--Part 3 task1
validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (square,value) board = not $ containsElem value (lookups (getPeers square) board)

--Task2
validBoard :: [(String,Int)] -> Bool -- Board -> Bool
validBoard board = not $ containsElem False $ map (\b -> validSquare b board) board

--Task 3 -- does not check values tough.
--verifySudoku board = (length board == 16) && validBoard board



--Task 4 - implement a data field testing a suduko.

--Part 4
reduceList [] _ = []
reduceList (x:xs) ys
    | containsElem x ys = reduceList xs ys
    | otherwise = x: reduceList xs ys

--Task 2.

--validSquareNumbers ::(String, Int) -> [(String, Int)] -> (String, [Int])

--digitToInt [] = []
digitToInt:: Char -> Int
digitToInt x = read [x]::Int

colToIntList str = map (digitToInt) str

validSquareNumbers::(String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (unit, value) board = (unit, reduceList (colToIntList cols) (lookups (getPeers unit) board))

validBoardNumbers :: [(String,Int)] -> [(String,[Int])]
validBoardNumbers board = map (\b -> validSquareNumbers b board) board

dublicatedElements::Eq a => [a] -> [Bool]
dublicatedElements (x:xs)
    | [x] == [] = []
    | xs == [] = []
    | otherwise = containsElem x xs : dublicatedElements xs

containsDuplicates xs = foldr (||) False $  dublicatedElements xs

canInsert:: Eq a=> [a]-> [a] -> [Bool]
canInsert (x:xs) ys
    | xs == [] = [elem x ys]
    | otherwise = elem x ys : canInsert xs ys

validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit validBoard = term1 && term2
    where
        validValues = lookups unit validBoard
        singleValidValues = concat $ filter (\p -> (length p ==1 )) validValues
        term1 = containsDuplicates singleValidValues
        totValidValues = concat validValues

        term2 = foldr (&&) True (canInsert (colToIntList cols) totValidValues)


--Task 5: Write a function validUnits which checks if all units in the variable unitList are valid for a Sudoku board.

--validUnits:: [[String]] -> Bool
--validUnits unitList = 
-- Task5
--validUnits :: [[String]] -> Bool
--validUnits :: [(String, Int)] -> Bool
--validUnits i = and [validUnit p (validBoardNumbers(validBoard i)) | p <- unitList]

--validUnits::Bool
validUnits validBoard = validUnit (concat unitList) validBoard

-- task6
verifySudoku board = (length board == 16) && validUnits (board)

        --findDuplicates (x:xs)
        --    | x == [] = []
        --    | otherwise = containsElem x xs
        --validValues = concat ( map (\b -> snd b ) validBoard )
        --foldr (&&) True [True,True,False]
        --["A1","A2","A3"]
        --validUnit ["A1","B2"] [("A1",[1,2,3,4]),("B2",[1])]


exampleSudoku :: [(String,Int)]
exampleSudoku = [("A1",4),("A2",4),("A3",3),("A4",2),("B1",3),("B2",2),("B3",1),("B4",4),("C1",4),("C2",1),("C3",2),("C4",3),("D1",2),("D2",3),("D3",4),("D4",1)]

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
groupByFirstChar :: Ord x => [(String, x)] -> [[(String, x)]]
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




findFaultyUnits :: [(String, Int)] -> [(String, [Int])]
findFaultyUnits  = (filter (\p -> (length (snd p)) == 0)) . validBoardNumbers

onlySolos xs = map (fst) xs

testThing [] _ = []
testThing (x:xs) p
    | containsElem (fst x) (faults) = (fst x, 0): testThing (xs) p
    | otherwise = x : testThing (xs) p
    where faults = map (fst) (findFaultyUnits (p))

printVerified xs = printSuduko (testThing xs xs)


readFromFile :: IO Bool
readFromFile = do
    content <- readFile "sud.txt"
    let linesOfFile = lines content
    let concatList = concat linesOfFile
    let intList = map (\p -> read [p] ::Int) concatList
    let sudukoBoard = zip (cross rows cols) (intList)
    return (verifySudoku (validBoardNumbers sudukoBoard))

    
--createTableFromFile = zip (cross rows cols) (readFromFile) 

