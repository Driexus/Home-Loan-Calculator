{-# LANGUAGE DeriveDataTypeable #-}
module Modules.LoanData where

import Data.Data

data LoanData = LoanData {
    initialInvestment :: Float,
    interest :: Float,
    inflation :: Float,
    yearlySalaryIncrease :: Float,
    propertyAppreciation :: Float,
    currentRent :: Float,
    rentIncrease :: Float,
    -- Loan Duration in Years
    durationYears :: Int,
    maintenanceFees :: Float,
    salary :: Float,
    propertyValue :: Float,
    baseCoL :: Float,
    basePropertyCosts :: Float
} deriving (Show, Data)

-- The equivalent the yearly property appreciation but in a monthly basis
monthlyPropertyAppreciation :: LoanData -> Float
monthlyPropertyAppreciation loanData = monthlyAdjustedPercentage $ propertyAppreciation loanData

-- The equivalent the yearly inflation but in a monthly basis
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
exponentialIncreases base baseRaisePctg duration = map (* base) exponentialRaises
    where   flatRaises = replicate (duration - 1) (1 + baseRaisePctg / 100) 
            exponentialRaises = scanl (*) 1 flatRaises

yearlyRaises :: Float -> Float -> Int -> Int -> [Float]
yearlyRaises base baseRaisePctg duration yearInterval = concatMap (replicate (12 * yearInterval)) raises
    where raises = exponentialIncreases base baseRaisePctg (duration `div` yearInterval)
