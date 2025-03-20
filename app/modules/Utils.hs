module Modules.Utils where

printList :: Show a => [a] -> Int -> Int -> IO ()
printList [] _ _= return ()
printList list elementsPerLine cellSize = do
    putStrLn $ showElementsRecursive (orderedPrefix "Month " elementsPerLine) size (replicate firstRowSpace ' ' ++ "|")
    printListRecursive splitted 1 size
    where splitted = chunksOf list elementsPerLine
          elementSize = maximum $ map (length . show) list
          size = max (elementSize + 2) cellSize

printListRecursive :: Show a => [[a]] -> Int -> Int -> IO ()
printListRecursive [] _ _ = return ()
printListRecursive (x:xs) current cellSize = do
    putStrLn $ verticalLine cellSize (length x)
    putStrLn (showConstant firstRowSpace ("Year " ++  show current) ++ showElementsRecursive (map show x) cellSize "|")
    printListRecursive xs (current + 1) cellSize

showElementsRecursive :: [String] -> Int -> String -> String
showElementsRecursive [] _ current = current
showElementsRecursive (x:xs) cellSize current = current ++ showElementsRecursive xs cellSize (showConstantCentered cellSize x ++ "|")

-- Fills empty spaces at the end to match the given size. If the size is less then does nothing.
showConstant :: Int -> String -> String
showConstant size a = a ++ extraChars
    where extraChars = concat $ replicate (size - length a) " "

-- Fills empty spaces at the end to match the given size. If the size is less then does nothing.
showConstantCentered :: Int -> String -> String
showConstantCentered size a = replicate (extraChars `div` 2) ' ' ++ a ++ replicate ((extraChars + 1) `div` 2) ' '
    where extraChars = size - length a

verticalLine :: Int -> Int -> String
verticalLine size elements = replicate (firstRowSpace + (size + 1) * elements) '-'

orderedPrefix :: String -> Int -> [String]
orderedPrefix prefix total = map ((++) prefix . show) [1..total]

-- TODO: Put in config file
firstRowSpace :: Int
firstRowSpace = 10

chunksOf :: [a] -> Int -> [[a]]
chunksOf list elements = chunksOfRecursive list elements []

chunksOfRecursive :: [a] -> Int -> [[a]] -> [[a]]
chunksOfRecursive [] _ splitted = splitted
chunksOfRecursive rest elements splitted = chunksOfRecursive (drop elements rest) elements (splitted ++ [take elements rest])
