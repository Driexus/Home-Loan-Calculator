import Modules.Projections
import Modules.LoanData
import Modules.Utils
import Text.JSON.Generic

main = do 
    loanData <- loadData
    let c = cashFlow [salaries] [monthlyAdjustedCosts, installments]
    printData c

printData :: Show a => (LoanData -> [a]) -> IO()
printData func = do 
    loanData <- loadData
    printList (func loanData) 12 

loadData :: IO LoanData
loadData = do 
    json <- readFile "resources/loanData.json"
    return $ decodeJSON json
