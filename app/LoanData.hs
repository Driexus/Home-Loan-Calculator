module LoanData where

import Data.Data

data LoanData = LoanData {
    initialInvestment :: Float,
    interest :: Float,
    inflation :: Float,
    yearlySalaryIncrease :: Float,
    propertyAppreciation :: Float,
    rentIncrease :: Float,
    -- Loan Duration in Years
    durationYears :: Int,
    currentRent :: Float,
    maintenanceFees :: Float,
    salary :: Float,
    propertyValue :: Float,
    monthlyCosts :: Float
} deriving (Show, Data)

-- Formula: https://www.calculatorsoup.com/calculators/financial/loan-calculator.php
installments :: LoanData -> [Float]
installments loanData = replicate (durationYears loanData * 12) (installment loanData)

-- An array of the montly salaries taking into account an one time yearly raise
salaries :: LoanData -> [Float]
salaries loanData = concatMap (replicate 12) yearlySalaries
    where flatRaises = replicate (durationYears loanData - 1) (1 + yearlySalaryIncrease loanData / 100)
          incrementalRaises = scanl (*) 1 flatRaises
          yearlySalaries = map (* salary loanData) incrementalRaises

propertyValues :: LoanData -> [Float]
propertyValues loanData = exponentialIncreases (propertyValue loanData) (monthlyPropertyAppreciation loanData) (durationMonths loanData)

monthlyPropertyAppreciation :: LoanData -> Float
monthlyPropertyAppreciation loanData = monthlyAdjustedPercentage $ propertyAppreciation loanData

-- The cashflow in case of buying a house
--cashflow :: Float -> Float -> Float -> Float -> Int -> [Float]
--cashflow baseSalary yearlyRaisePctg amount interest years = zipWith (-) s i
  --  where s = salaries baseSalary yearlyRaisePctg years
    --      i = installments amount interest years

-- The equivalent of the monthly inflation based on the yearly inflation
monthlyInflation :: LoanData -> Float
monthlyInflation loanData = monthlyAdjustedPercentage $ inflation loanData

monthlyAdjustedPercentage :: Float -> Float
monthlyAdjustedPercentage x = ((1 + x / 100) ** (1 / 12) - 1) * 100

loanAmount :: LoanData -> Float
loanAmount loanData = propertyValue loanData - initialInvestment loanData

installment :: LoanData -> Float
installment loanData = (loanAmount loanData * monthlyInterest) / (1 - (1/(1 + monthlyInterest) ^ (durationYears loanData * 12)))
    where monthlyInterest = interest loanData / 12 / 100

durationMonths :: LoanData -> Int
durationMonths loanData = 12 * durationYears loanData

-- An array of montly costs, adjusted montly on a yearly inflation
monthlyAdjustedCosts :: LoanData -> [Float]
monthlyAdjustedCosts loanData = exponentialIncreases (monthlyCosts loanData) (monthlyInflation loanData) (durationMonths loanData)

exponentialIncreases :: Float -> Float -> Int -> [Float]
exponentialIncreases base flatRaisePctg duration = map (* base) exponentialRaises
    where   flatRaises = replicate (duration - 1) (1 + flatRaisePctg / 100) 
            exponentialRaises = scanl (*) 1 flatRaises
