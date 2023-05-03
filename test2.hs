
import Data.List (nub)
import System.IO

--Andreas Karlsson
--Sabah begum 


--Run by chaning the file which should be read.
--Something is wrong, and we can't find what... but it feels like we are super close.


main :: IO [Bool]
main = do
    contents <- readFile "easy50.txt"
    let filteredContents = filter (/= '\n') contents
    let onlySudokus = words (replace '=' ' ' filteredContents)
    let returnList = (map (\p -> validUnits p) onlySudokus)
    return returnList
  


replace :: Char -> Char -> String -> String
replace a b str  = map (\p -> if p == a then b else p) str

rows :: Int -> [Char]
rows sideLength = take sideLength ['A'..]


cols :: Int -> [Char]
cols sideLength = take sideLength ['1'..]

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x,y] | x<- xs, y<-ys]

--All possible squares


squares :: Int -> [String]
squares sideLength = cross (rows sideLength) (cols sideLength)

type SudokuBoard = [(String, Int)]

--Create a board from input
createBoard :: String -> SudokuBoard
createBoard input = zip (squares (squareRoot (length input))) (map (\p -> read [p]::Int) input)


--Setting up function to be able to retrieve all combinations


unitRow :: Int -> [[String]]
unitRow sideLength = [ cross [i] (cols sideLength) | i <-(rows sideLength)]


unitCol :: Int -> [[String]]
unitCol sideLength = [ cross (rows sideLength) [i] | i <-(cols sideLength)]


unitBox :: Int -> [[String]]
unitBox sideLength = [ cross i j | i <- group_by sideLength (rows sideLength), j<-group_by sideLength (cols sideLength)]


unitList :: Int -> [[String]]
unitList sideLength = unitRow sideLength ++ unitCol sideLength ++ unitBox sideLength


group_by :: Int -> [a] -> [[a]]
group_by n [] = []
group_by n xs = take size xs : group_by n (drop size xs)
    where size = (squareRoot n)

squareRoot :: Int -> Int
squareRoot x = round (sqrt (fromIntegral x))

-------------------------------------------------------------
---- A function which takes a square, and return the units it's in.

filterUnitList :: String -> SudokuBoard-> [[String]]
filterUnitList square board = filter (containsElem square) (unitList (squareRoot (length board)))

--Helpfunctions

--chek if a list contains a specific element
containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs

--Calculate the value units which is a list of tuples where each tuple is a square string together with its corresponding three units, 
units :: SudokuBoard -> [(String, [[String]])]
units board = [ (s,filterUnitList s board) |s <-squares (length board)]

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
--peers ::SudokuBoard -> [(String, [String])]
--peers board = [(s, delete s (removeDuplicates (foldList (filterUnitList s board)))) | s<-squares (length board)]
--
peers :: SudokuBoard -> [(String, [String])]
peers board = [ (i, filter (/= i) $  closestPeers i board ) | i <- squares (squareRoot (length board))  ]

peers2 board =  [(i) | i <- squares (squareRoot (length board))  ]

closestPeers :: String -> SudokuBoard -> [String]
closestPeers box board = removeDuplicates ( foldList ( filterUnitList box board))

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
getPeers :: String -> SudokuBoard -> [String]
getPeers square board = fromMaybe [] (lookup square (peers board))

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
validSquare :: (String, Int) -> SudokuBoard -> Bool
validSquare (_,0) _ = True
validSquare (square,value) board = not $ containsElem value (lookups (getPeers square board) board)

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
--validSquareNumbers::(String, Int) -> SudokuBoard -> (String, [Int]) --Might be something wrong with this one....
--validSquareNumbers (unit, value) board = (unit, reduceList (stringToIntList (cols (squareRoot ( length board)))) (lookups (getPeers unit board) board))
validSquareNumbers :: (String, Int) -> SudokuBoard -> (String, [Int])
validSquareNumbers x board
  | snd x == 0 = (fst x, reduceList (stringToIntList (cols (squareRoot (length board)))) (lookups(getPeers (fst x) board) board))
  | validSquare x board = (fst x, [snd x])
  | otherwise = (fst x, [])

--
digitToInt :: Char -> Int
digitToInt x --Stolen from the web, doesn't work if i only use read.
  | x >= 'a' && x <= 'f' = fromEnum x - fromEnum 'a' + 10
  | x >= 'A' && x <= 'F' = fromEnum x - fromEnum 'A' + 10
  | otherwise = fromEnum x - fromEnum '0'

stringToIntList :: [Char] -> [Int]
stringToIntList string = map (digitToInt) string

---- Retrieve an entire board with the possible values at each square.
validBoardNumbers :: SudokuBoard -> [(String, [Int])]
validBoardNumbers board = map (\p -> validSquareNumbers p board) board

--Check if a unit is valid
--validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit validBoard = term1 && term2
    where
        --Term 1
        validValues = lookups unit validBoard
        singleValidValues = concat $ filter (\p -> (length p ==1 )) validValues --This seems like it should work....
        term1 = not (containsDuplicates singleValidValues) --Works almost perfectly when runnig test, only one element is wrong...

        --term2 
        totValidValues = concat validValues
        --term2 =  and (canInsert (stringToIntList (cols (squareRoot (length (validBoard))))) totValidValues)
        term2 = True

containsDuplicates :: Eq a => [a] -> Bool
containsDuplicates xs = not ( length (nub xs) == length xs)

canInsert:: Eq a=> [a]-> [a] -> [Bool]
canInsert (x:xs) ys
    | xs == [] = [elem x ys]
    | otherwise = elem x ys : canInsert xs ys


-- validate if a input is correct or not.
--validUnits :: String -> Bool
validUnits :: String -> Bool
validUnits x = and [validUnit z (validBoardNumbers (createBoard x)) | z <- unitList (length (createBoard x))]
--validUnits x = and [validUnit z (validBoardNumbers (createBoard x)) | z <- unitList (length (createBoard x))]

test = validUnits t


t = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
