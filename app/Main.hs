import Modules.Projections
import Modules.LoanData
import Modules.Utils
import Text.JSON.Generic
import Options.Applicative

project :: Projection -> Int -> IO()
project func size = do 
    loanData <- loadData
    printList (func loanData) 12 size

loadData :: IO LoanData
loadData = do 
    json <- readFile "resources/loanData.json"
    return $ decodeJSON json


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Calculate some monatery values"
     <> header "A simple program to calculate future monetary values")

run :: Options -> IO ()
run (Options name pretty raw) = do
  let maybeProjection = projectionFromName name
  case maybeProjection of
    Just projection -> printProjection projection raw pretty
    Nothing -> print $ "Option '" ++ name ++ "' could not be found. Run -h for help."

printProjection :: Projection -> Bool -> String -> IO()
printProjection proj False "qhd" = project proj 21
printProjection proj False _ = project proj 15
printProjection proj True _ = do
  l <- loadData
  mapM_ print (proj l)

data Options = Options {
  target     :: String,
  pretty     :: String,
  raw        :: Bool
}

options :: Parser Options
options = Options
      <$> strArgument (
        metavar "Target" <>
        help "The target function to run"
      )
      <*> strOption (
        long "pretty" <>
        short 'p' <>
        metavar "fhd or qhd" <>
        value "fhd" <>
        help "Enable pretty printing. Pass 'fhd' for Full HD screens and 'qhd' for for Quad HD."
      )
      <*> switch (
        long "raw" <>
        short 'r' <>
        help "Print the values in seperate lines. This overrides pretty printing."
      )
