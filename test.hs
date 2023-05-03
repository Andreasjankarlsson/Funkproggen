import Data.List (nub)


--Basic setup overall settings.

sideLength :: Int
sideLength = 4

rows :: [Char]
rows = take sideLength ['A'..]

cols :: [Char]
cols = take sideLength ['1'..]

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x,y] | x<- xs, y<-ys]

--All possible squares
squares :: [[Char]]
squares = cross (take sideLength rows) (take sideLength cols)

type SudokuBoard = [(String, Int)]

--Create a board from input
createBoard :: String -> SudokuBoard
createBoard input = zip squares (map (\p -> read [p]::Int) input)


--Setting up function to be able to retrieve all combinations

unitRow :: [[String]]
unitRow  = [ cross [i] cols | i <-(rows)]

unitCol :: [[String]]
unitCol = [ cross (rows) [i] | i <-(cols)]

unitBox :: [[String]]
unitBox = [ cross i j | i <- group_by (rows), j<-group_by (cols)]

unitList :: [[String]]
unitList = unitRow ++ unitCol ++ unitBox

group_by :: [a] -> [[a]]
group_by [] = []
group_by xs = take squareSize xs : group_by (drop squareSize xs)
    where squareSize = squareRoot sideLength

squareRoot :: Int -> Int
squareRoot x = round (sqrt (fromIntegral x))

-------------------------------------------------------------
---- A function which takes a square, and return the units it's in.

filterUnitList :: String -> [[String]]
filterUnitList square = filter (containsElem square) unitList

--Helpfunctions

--chek if a list contains a specific element
containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs

--Calculate the value units which is a list of tuples where each tuple is a square string together with its corresponding three units, 
units :: [(String, [[String]])]
units = [ (s,filterUnitList s) |s <-squares]

--Concat a list
foldList :: [[a]] -> [a]
foldList = concat

--Remove duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | containsElem x xs == True = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

--
peers ::[(String, [String])]
peers = [(s, delete s (removeDuplicates (foldList (filterUnitList s)))) | s<-squares]

--Help function to delete a value from a list.
delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (x:xs)
    | x == n = delete n xs
    | otherwise = x : delete n xs


--Start laboration 2.
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just b) = b
fromMaybe a _ = a

--Get the peers of a square
getPeers :: String -> [String]
getPeers square = fromMaybe [] (lookup square peers)

--From a maybeList return only the Just values
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing:xs) = justifyList xs
justifyList ((Just x): xs) = x : justifyList xs

--takes a list of Maybe objects and outputs a list of only the Just element values (without the constructor Just).
lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups [] _ = []
lookups _ [] = []
lookups xs ys = justifyList (map  (\x -> lookup x ys) xs) --Easier way to do then lamda functions?


--Part 3 of lab 2.
type Sudoku = String

--Se if a square is valid, this by checking if it's peers has the same value
validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (_,0) _ = True
validSquare (square,value) board = not $ containsElem value (lookups (getPeers square) board)

--Se if an entire board is valid, redone since last time :)
validBoard :: SudokuBoard -> Bool
validBoard [] = True
validBoard (x:xs)
 | validSquare x xs = validBoard xs
 | otherwise = False

--Verify if a sudoku is valid
verifySudoku :: String -> Bool
verifySudoku input = validBoard (createBoard input)

-- from two input lists removes occurrences of elements in the second list from the first list
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList [] _ = []
reduceList (x:xs) ys
    | containsElem x ys = reduceList xs ys
    | otherwise = x: reduceList xs ys

-- Retrieve possible values for a square 
validSquareNumbers::(String, Int) -> SudokuBoard -> (String, [Int]) --Might be something wrong with this one....
validSquareNumbers (unit, value) board = (unit, reduceList (stringToIntList cols) (lookups (getPeers unit) board))
--
digitToInt:: Char -> Int
digitToInt x = read [x]::Int

stringToIntList :: [Char] -> [Int]
stringToIntList string = map (digitToInt) string

---- Retrieve an entire board with the possible values at each square.
validBoardNumbers :: SudokuBoard -> [(String, [Int])]
validBoardNumbers board = map (\p -> validSquareNumbers p board) board

--Check if a unit is valid
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit validBoard = term1 && term2
    where
        --Term 1 Seems ok!
        validValues = lookups unit validBoard
        singleValidValues = concat $ filter (\p -> (length p ==1 )) validValues
        term1 = containsDuplicates singleValidValues

        --term2 
        term2 = and [containsElem x (concat (lookups unit validBoard)) | x <- (stringToIntList cols)]

containsDuplicates :: Eq a => [a] -> Bool
containsDuplicates xs = not ( length (nub xs) == length xs)




-- validate if a input is correct or not.
validUnits :: String -> Bool
validUnits x = and [validUnit z (validBoardNumbers (createBoard x)) | z <- unitList]

test = validUnits "020030090000907000900208005004806500607000208023102900800605007000309000030020050"