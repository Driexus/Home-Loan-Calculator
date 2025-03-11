module Modules.Projections where

import Modules.LoanData

type Projection = LoanData -> [Float]

combine :: (a -> a -> a) -> (b -> [a]) -> (b -> [a]) -> (b -> [a])
combine combinator f1 f2 b = zipWith combinator (f1 b) (f2 b)

cashFlow :: [Projection] -> [Projection] -> Projection
cashFlow incomes expenses = combine (-) incomeTotal expensesTotal
    where   incomeTotal   = foldl (combine (+)) (const $ repeat 0) incomes
            expensesTotal = foldl (combine (+)) (const $ repeat 0) expenses

-- Monthly salaries taking into account an one time yearly raise
salaries :: Projection
salaries loanData = concatMap (replicate 12) yearlySalaries
    where flatRaises = replicate (durationYears loanData - 1) (1 + yearlySalaryIncrease loanData / 100)
          incrementalRaises = scanl (*) 1 flatRaises
          yearlySalaries = map (* salary loanData) incrementalRaises

-- Costs, adjusted montly on a yearly inflation
monthlyAdjustedCosts :: Projection
monthlyAdjustedCosts loanData = exponentialIncreases (monthlyCosts loanData) (monthlyInflation loanData) (durationMonths loanData)

propertyValues :: Projection
propertyValues loanData = exponentialIncreases (propertyValue loanData) (monthlyPropertyAppreciation loanData) (durationMonths loanData)

-- Formula: https://www.calculatorsoup.com/calculators/financial/loan-calculator.php
installments :: Projection
installments loanData = replicate (durationYears loanData * 12) (installment loanData)