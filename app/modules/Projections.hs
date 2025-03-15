module Modules.Projections where

import Modules.LoanData

type Projection = LoanData -> [Float]

combine :: (a -> b -> c) -> (d -> [a]) -> (d -> [b]) -> (d -> [c])
combine combinator f1 f2 b = zipWith combinator (f1 b) (f2 b)

cashFlow :: [Projection] -> [Projection] -> Projection
cashFlow incomes expenses = combine (-) incomeTotal expensesTotal
    where   incomeTotal   = foldl (combine (+)) (const $ repeat 0) incomes
            expensesTotal = foldl (combine (+)) (const $ repeat 0) expenses

-- Monthly salaries taking into account an one time yearly raise
salaries :: Projection
salaries loanData = yearlyCompoundRaises (salary loanData) (yearlySalaryIncrease loanData) (durationYears loanData) 1

-- Costs, adjusted montly on a yearly inflation
monthlyCol :: Projection
monthlyCol loanData = monthlyCompoundRaises (baseCoL loanData) (monthlyInflation loanData) (durationMonths loanData)

-- Property costs
propertyCosts :: Projection
propertyCosts loanData = monthlyCompoundRaises (basePropertyCosts loanData) (monthlyInflation loanData) (durationMonths loanData)

propertyValues :: Projection
propertyValues loanData = monthlyCompoundRaises (propertyValue loanData) (monthlyPropertyAppreciation loanData) (durationMonths loanData)

-- Formula: https://www.calculatorsoup.com/calculators/financial/loan-calculator.php
installments :: Projection
installments loanData = replicate (durationYears loanData * 12) (installment loanData)

rents :: Projection
rents loanData = yearlyCompoundRaises (currentRent loanData) (rentIncrease loanData) (durationYears loanData) 2

investedRoI :: Projection
investedRoI loanData = monthlyCompoundRaises (initialInvestment loanData) (monthlyRoI loanData) (durationMonths loanData)


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


-- Investment Returns

invest :: Projection -> Projection
invest projection loanData = compoundArray result (monthlyRoI loanData)
    where result = projection loanData
