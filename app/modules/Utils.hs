module Modules.Utils where

printList :: Show a => [a] -> Int -> IO ()
printList [] _ = return ()
printList list elementsPerLine = do
    putStrLn $ showElementsRecursive (orderedPrefix "Month " elementsPerLine) elementSize (replicate firstRowSpace ' ' ++ "|")
    printListRecursive splitted 1 elementSize
    where splitted = chunksOf list elementsPerLine
          elementSize = maximum $ map (length . show) list

printListRecursive :: Show a => [[a]] -> Int -> Int -> IO ()
printListRecursive [] _ _ = return ()
printListRecursive (x:xs) current elementSize = do
    putStrLn $ verticalLine elementSize (length x)
    putStrLn (showConstant firstRowSpace ("Year " ++  show current) ++ showElementsRecursive (map show x) elementSize "|")
    printListRecursive xs (current + 1) elementSize

showElementsRecursive :: [String] -> Int -> String -> String
showElementsRecursive [] _ current = current
showElementsRecursive (x:xs) maxSize current = current ++ showElementsRecursive xs maxSize (showConstantCentered (maxSize + elementSpacing) x ++ "|")

showConstant :: Int -> String -> String
showConstant size a = a ++ extraChars
    where extraChars = concat $ replicate (size - length a) " "

showConstantCentered :: Int -> String -> String
showConstantCentered size a = replicate (extraChars `div` 2) ' ' ++ a ++ replicate ((extraChars + 1) `div` 2) ' '
    where extraChars = size - length a

verticalLine :: Int -> Int -> String
verticalLine size elements = replicate (firstRowSpace + 1 + (cellSize size) * elements) '-'

orderedPrefix :: String -> Int -> [String]
orderedPrefix prefix total = map ((++) prefix . show) [1..total]

-- TODO: Put in config file
firstRowSpace :: Int
firstRowSpace = 4 + elementSpacing

-- TODO: Put in config file
elementSpacing :: Int
elementSpacing = 4

emptyElementSpacing :: String
emptyElementSpacing = replicate elementSpacing ' '

cellSize :: Int -> Int
cellSize elementSize = elementSize + elementSpacing + 1

chunksOf :: [a] -> Int -> [[a]]
chunksOf list elements = chunksOfRecursive list elements []

chunksOfRecursive :: [a] -> Int -> [[a]] -> [[a]]
chunksOfRecursive [] _ splitted = splitted
chunksOfRecursive rest elements splitted = chunksOfRecursive (drop elements rest) elements (splitted ++ [take elements rest])