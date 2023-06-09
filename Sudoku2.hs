--import Data.List
--Andreas Karlsson
--Sabah begum 

module Labb3.Sudoku2 where


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
validSquare (_,0) _ = True
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
validSquareNumbers (unit, 0) board = (unit, reduceList (colToIntList cols) (lookups (getPeers unit) board))
validSquareNumbers (unit, value) board = (unit, if validSquare (unit,value) board then [value] else [])

validBoardNumbers :: [(String,Int)] -> [(String,[Int])]
validBoardNumbers board = map (\b -> validSquareNumbers b board) board 

dublicatedElements::Eq a => [a] -> [Bool]
dublicatedElements (x:xs)
    | [x] == [] = []
    | xs == [] = []
    | otherwise = containsElem x xs : dublicatedElements xs 

containsDuplicates :: Eq a => [a] -> Bool
containsDuplicates xs = foldr (||) False $  dublicatedElements xs

canInsert:: Eq a=> [a]-> [a] -> [Bool]
canInsert (x:xs) ys
    | xs == [] = [elem x ys]
    | otherwise = elem x ys : canInsert xs ys
    

validUnit :: Eq a => [a] -> [(a, [Int])] -> Bool
validUnit unit validBoard = term1 && term2
    where
        validValues = lookups unit validBoard
        singleValidValues = concat $ filter (\p -> (length p ==1 )) validValues
        term1 = containsDuplicates singleValidValues
        totValidValues = concat validValues

        term2 = foldr (&&) True (canInsert (colToIntList cols) totValidValues)

        --findDuplicates (x:xs)
        --    | x == [] = []
        --    | otherwise = containsElem x xs
        --validValues = concat ( map (\b -> snd b ) validBoard )
        --foldr (&&) True [True,True,False]
        --["A1","A2","A3"]
        --validUnit ["A1","B2"] [("A1",[1,2,3,4]),("B2",[1])]


validUnits :: [(String, [Int])] -> Bool
validUnits validBoard = foldr (&&) True ( map (\p -> validUnit p validBoard) (unitList))

verifySudoku :: [(String, [Int])] -> Bool
verifySudoku board = validUnits board && (length board == 16) 
