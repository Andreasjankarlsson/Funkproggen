import Data.List
--Andreas Karlsson
--Sabah begum 


rows = "ABCD"
cols = "1234"
containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs


cross list1 list2 = [ [i,j] | i <- list1, j<-list2]


--Task 1
unitRow = [ cross [i] "1234" | i <-"ABCD"]

unitCol = transpose unitRow

unitBox = [ cross i j | i <-["AB","CD"], j <-["12","34"]]

unitList = unitRow ++ unitCol ++ unitBox

--Task 2
filterUnitlist square = filter (containsElem square) unitList 

--Task 3.
task3 square = (square, filterUnitlist square)

--Task 4
foldList = concat 

--Task 5 
removeDuplicates (x:xs) 
    | xs == [] = x : []
    | containsElem x xs == True = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

--task 6
--peers :: [(String, [String])]
boxes= removeDuplicates (foldList unitList)


closestPeers box = removeDuplicates ( foldList ( filterUnitlist box))

peers :: [(String, [String])]
peers = [ (i, filter (/= i) $  closestPeers i ) | i <- boxes  ]
