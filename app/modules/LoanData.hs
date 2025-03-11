{-# LANGUAGE DeriveDataTypeable #-}
module Modules.LoanData where

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

monthlyPropertyAppreciation :: LoanData -> Float
monthlyPropertyAppreciation loanData = monthlyAdjustedPercentage $ propertyAppreciation loanData

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

exponentialIncreases :: Float -> Float -> Int -> [Float]
exponentialIncreases base flatRaisePctg duration = map (* base) exponentialRaises
    where   flatRaises = replicate (duration - 1) (1 + flatRaisePctg / 100) 
            exponentialRaises = scanl (*) 1 flatRaises
