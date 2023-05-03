import Data.Char (isDigit)
import Data.List (sort)
--Andreas Karlsson
--Sabah begum 

main = do
    let length = 9
    return True

rows :: Int -> [Char]
rows length = take length ['A'..]


cols :: Int -> [Char]
cols length = take length ['1'..]

--create all units


unitRow :: Int -> [[[Char]]]
unitRow length = [ cross [i] (cols length) | i <-(rows length)]


unitCol :: Int -> [[[Char]]]
unitCol length = [ cross (rows length) [i] | i <-(cols length)]


unitBox :: Int -> [[[Char]]]
unitBox n = [ cross i j | i <- (group_by n (rows  (n*n))), j <-(group_by n (cols (n*n)))]


group_by :: Int -> [a] -> [[a]]
group_by n [] = []
group_by n xs = take n xs : group_by n (drop n xs)

squareRoot :: Int -> Int
squareRoot x = round (sqrt (fromIntegral x))

unitList :: Int -> [[String]]
unitList length= unitRow length ++ unitCol length ++ unitBox (squareRoot length)

--
--chek if a list contains a specific element
containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs

--Create the cartesian product of two lists.
cross :: [a] -> [a] -> [[a]]
cross list1 list2 = [ [i,j] | i <- list1, j<-list2]


justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing:xs) = justifyList xs
justifyList ((Just x): xs) = x : justifyList xs

-- Task 5
lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups xs ys = justifyList (map  (\x -> lookup x ys) xs) --Easier way to do then lamda functions?

canInsert:: Eq a=> [a]-> [a] -> [Bool]
canInsert (x:xs) ys
    | xs == [] = [elem x ys]
    | otherwise = elem x ys : canInsert xs ys

dublicatedElements::Eq a => [a] -> [Bool]
dublicatedElements (x:xs)
    | [x] == [] = []
    | xs == [] = []
    | otherwise = containsElem x xs : dublicatedElements xs


containsDuplicates :: Eq a => [a] -> Bool
containsDuplicates xs = foldr (||) False $  dublicatedElements xs

digitToInt:: Char -> Int
digitToInt x = read [x]::Int

colToIntList :: [Char] -> [Int]
colToIntList str = map (digitToInt) str


validUnit :: Eq a => [a] -> [(a, [Int])] -> Int -> Bool
validUnit unit validBoard l = term1 && term2
    where
        validValues = lookups unit validBoard
        singleValidValues = concat $ filter (\p -> (length p ==1 )) validValues
        term1 = containsDuplicates singleValidValues
        totValidValues = concat validValues

        term2 = foldr (&&) True (canInsert (colToIntList (cols l)) totValidValues)

reduceList :: Eq a => [a] -> [a] -> [a]
reduceList [] _ = []
reduceList (x:xs) ys
    | containsElem x ys = reduceList xs ys
    | otherwise = x: reduceList xs ys

removeDuplicates (x:xs)
    | xs == [] = x : []
    | containsElem x xs == True = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

foldList = concat

filterUnitlist :: [Char] -> Int -> [[[Char]]]
filterUnitlist square l = filter (containsElem square) (unitList l)

closestPeers :: [Char] -> Int -> [[Char]]
closestPeers box l = removeDuplicates ( foldList ( filterUnitlist box l))

fromMayby:: a -> Maybe a -> a
fromMayby defVal maybeVal = case maybeVal of
    Just x -> x
    Nothing -> defVal


boxes :: Int -> [String]
boxes l= removeDuplicates (concat (unitList l))




peers :: Int -> [(String, [String])]
peers l = [ (i, filter (/= i) (closestPeers i l )) | i <- (boxes l) ]



getPeers :: String -> Int -> [String]
getPeers box l = fromMayby [] (lookup box (peers l))


validSquareNumbers :: (String, b) -> [(String, Int)] -> Int -> (String, [Int])
validSquareNumbers (unit, value) board l= (unit, reduceList (colToIntList (cols l)) (lookups (getPeers unit l) board))

--Retrieve valid numbers for a board 
--validBoardNumbers :: [(String,Int)] -> [(String,[Int])]
validBoardNumbers :: [(String, Int)]-> Int -> [(String, [Int])]
validBoardNumbers board l = map (\b -> validSquareNumbers b board l) board

--Validate that some units are valid
validUnits :: [(String, [Int])] -> Int -> Bool
validUnits validBoard l = validUnit (concat (unitList l)) validBoard l

--Verify that a given sudoku is acceptable
verifySudoku :: [(String, Int)] -> Int -> Bool
verifySudoku board l = (length b == (l*l)) && validUnits b l
    where b = validBoardNumbers board l

exampleSudoku :: [(String, Int)]
exampleSudoku = [("A1",4),("A2",4),("A3",3),("A4",4),("B1",3),("B2",2),("B3",1),("B4",4),("C1",4),("C2",1),("C3",2),("C4",3),("D1",2),("D2",3),("D3",4),("D4",1)]


parseSudoku :: [Char] -> Bool
parseSudoku string = verifySudoku sudukuBoard size
    where
        filteredString = filter (\p -> (p == '|') || (p =='\n'))
        sudokuValues = colToIntList (filter isDigit string)
        size = squareRoot (length sudokuValues)
        units = sort (boxes size)
        sudukuBoard = zip units sudokuValues
