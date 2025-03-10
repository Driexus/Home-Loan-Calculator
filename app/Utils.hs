module Utils where

printList :: Show a => [a] -> Int -> IO ()
printList [] x = return ()
printList list elementsPerLine = do
    putStrLn $ showElementsRecursive (orderedPrefix "C" elementsPerLine) elementSize (replicate firstRowSpace ' ' ++ "|" ++ emptyElementSpacing)
    printListRecursive splitted 1 elementSize
    where splitted = chunksOf list elementsPerLine
          elementSize = maximum $ map (length . show) list

printListRecursive :: Show a => [[a]] -> Int -> Int -> IO ()
printListRecursive [] _ _ = return ()
printListRecursive (x:xs) current elementSize = do
    putStrLn $ verticalLine elementSize (length x)
    putStrLn ((showConstant ('R' : show current) firstRowSpace) ++ (showElementsRecursive x elementSize ("|" ++ emptyElementSpacing)))
    printListRecursive xs (current + 1) elementSize

showElementsRecursive :: Show a => [a] -> Int -> String -> String
showElementsRecursive [] _ current = current
showElementsRecursive (x:xs) maxSize current = current ++ showElementsRecursive xs maxSize (showConstant x (maxSize + elementSpacing) ++ "|" ++ emptyElementSpacing)

showConstant :: Show a => a -> Int -> String
showConstant a size = showA ++ extraChars
    where   showA = show a
            extraChars = concat $ replicate (size - length showA) " "

verticalLine :: Int -> Int -> String
verticalLine size elements = replicate (firstRowSpace + 1 + (cellSize size) * elements) '-'

orderedPrefix :: String -> Int -> [String]
orderedPrefix prefix total = zipWith (++) (repeat prefix) (map show [1..total])

-- TODO: Put in config file
firstRowSpace :: Int
firstRowSpace = 4 + elementSpacing

-- TODO: Put in config file
elementSpacing :: Int
elementSpacing = 7

emptyElementSpacing :: String
emptyElementSpacing = replicate elementSpacing ' '

cellSize :: Int -> Int
cellSize elementSize = elementSize + elementSpacing * 2 + 1

chunksOf :: [a] -> Int -> [[a]]
chunksOf list elements = chunksOfRecursive list elements []

chunksOfRecursive :: [a] -> Int -> [[a]] -> [[a]]
chunksOfRecursive [] elements splitted = splitted
chunksOfRecursive rest elements splitted = chunksOfRecursive (drop elements rest) elements (splitted ++ [take elements rest])