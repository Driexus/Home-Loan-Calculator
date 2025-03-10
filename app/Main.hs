import LoanData
import Utils
import Text.JSON.Generic

main = do 
    loanData <- loadData
    let s = salaries loanData
    printList s 12

printData :: Show a => (LoanData -> [a]) -> IO()
printData func = do 
    loanData <- loadData
    printList (func loanData) 12 

loadData :: IO LoanData
loadData = do 
    json <- readFile "resources/loanData.json"
    return $ decodeJSON json