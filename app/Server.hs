{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import GHC.Generics
import Data.Aeson (ToJSON)
import Modules.LoanData
import Modules.Projections

main :: IO ()
main = scotty 3010 $ do
    get "/projection/:projectionName" $ do
        setHeader "Access-Control-Allow-Origin" "*"
        setHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS, PUT, DELETE"
        setHeader "Access-Control-Allow-Headers" "Content-Type, Authorization, X-Requested-With"
        name <- pathParam "projectionName"
        let maybeProjection = projectionFromName name
        case maybeProjection of
            Just projection -> json $ Response (projection lData)
            Nothing -> return()

data Response = Response {
    projection :: [Float]
} deriving (Show, Generic)

instance ToJSON Response

-- TODO: Transfer to LoanData as base
lData :: LoanData
lData = LoanData 15000 4 9 5 9 0.5 500 4 10 300 1000 150000 600 300

    --"initialInvestment" : 15000,
    --"interest" : 4,
    --"yearlyRoI": 9,
    --"inflation" : 5,
    --"yearlySalaryIncrease" : 9,
    --"propertyAppreciation" : 4.5,
    --"currentRent" : 500,
    --"rentIncrease" : 4,
    --"durationYears" : 10,
    --"maintenanceFees" : 300,
    --"salary" : 1000,
    --"propertyValue" : 150000,
    --"baseCoL" : 600,
    --"basePropertyCosts": 300
--
--
--
    --initialInvestment :: Float,
    --interest :: Float,
    --yearlyRoI :: Float,
    --inflation :: Float,
    --yearlySalaryIncrease :: Float,
    --propertyAppreciation :: Float,
    --currentRent :: Float,
    --rentIncrease :: Float,
    ---- Loan Duration in Years
    --durationYears :: Int,
    --maintenanceFees :: Float,
    --salary :: Float,
    --propertyValue :: Float,
    --baseCoL :: Float,
    --basePropertyCosts :: Float
