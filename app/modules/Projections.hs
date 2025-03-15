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
salaries loanData = yearlyRaises (salary loanData) (yearlySalaryIncrease loanData) (durationYears loanData) 1

-- Costs, adjusted montly on a yearly inflation
monthlyCol :: Projection
monthlyCol loanData = exponentialIncreases (baseCoL loanData) (monthlyInflation loanData) (durationMonths loanData)

-- Property costs
propertyCosts :: Projection
propertyCosts loanData = exponentialIncreases (basePropertyCosts loanData) (monthlyInflation loanData) (durationMonths loanData)

propertyValues :: Projection
propertyValues loanData = exponentialIncreases (propertyValue loanData) (monthlyPropertyAppreciation loanData) (durationMonths loanData)

-- Formula: https://www.calculatorsoup.com/calculators/financial/loan-calculator.php
installments :: Projection
installments loanData = replicate (durationYears loanData * 12) (installment loanData)

rents :: Projection
rents loanData = yearlyRaises (currentRent loanData) (rentIncrease loanData) (durationYears loanData) 2


-- Totals and Cashflows

totalRentingCosts :: Projection
totalRentingCosts = cashFlow [monthlyCol, rents] []

totalBuyingCosts :: Projection
totalBuyingCosts = cashFlow [monthlyCol, installments, propertyCosts] []

opportunityCosts :: Projection
opportunityCosts = cashFlow [totalBuyingCosts] [totalRentingCosts]

cashFlowRenting :: Projection
cashFlowRenting = cashFlow [salaries] [totalRentingCosts]

cashFlowBuying :: Projection
cashFlowBuying = cashFlow [salaries] [totalBuyingCosts]
