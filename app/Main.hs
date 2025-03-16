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
run (Options "salaries" p) = printProjection salaries p
run (Options "costsRent" p) = printProjection totalRentingCosts p
run (Options "costsBuy" p) = printProjection totalBuyingCosts p
run (Options "flowRent" p) = printProjection cashFlowRenting p
run (Options "flowBuy" p) = printProjection cashFlowBuying p
run (Options "investFlowRent" p) = printProjection (invest cashFlowRenting) p
run (Options "investFlowBuy" p) = printProjection (invest cashFlowBuying) p
run (Options t _) = print $ "Option '" ++ t ++ "' could not be found. Run -h for help."

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
