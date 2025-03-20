import Modules.Projections
import Modules.LoanData
import Modules.Utils
import Text.JSON.Generic
import Options.Applicative

project :: Projection -> IO()
project func = do 
    loanData <- loadData
    printList (func loanData) 12

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
run (Options name p) = do
  let maybeProjection = projectionFromName name
  case maybeProjection of
    Just projection -> printProjection projection p
    Nothing -> print $ "Option '" ++ name ++ "' could not be found. Run -h for help."

printProjection :: Projection -> Bool -> IO()
printProjection proj True = project proj
printProjection proj False = do
  l <- loadData
  mapM_ print (proj l)

data Options = Options {
  target     :: String,
  pretty     :: Bool
}

options :: Parser Options
options = Options
      <$> strArgument (
        metavar "Target" <>
        help "The target function to run"
      )
      <*> switch (
        long "pretty" <>
        short 'p' <>
        help "Enable pretty printing"
      )
