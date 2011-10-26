module KRBot.GameRules where

import Data.Maybe

import KRBot.Cards
import KRBot.Players
import KRBot.Contracts

data Trick = Trick PlayerPosition [Card]


tricksInHand = 12

bidIsLegal :: Maybe Contract -> PlayerPosition -> [Maybe Contract] -> Bool
bidIsLegal Nothing _ [] = False
bidIsLegal Nothing _ _ = True
bidIsLegal (Just bid) p bidsSoFar
    | isForehand && null bidsSoFar = bid `elem` (nonForehandContracts ++ [Rufer])
    | isForehand && bidsSoFar == [Just Rufer, Nothing, Nothing, Nothing] = bidOnlyForehand
    | not bidOnlyForehand = cmpBid $ last . catMaybes $ bidsSoFar
    | otherwise = False
    where cmpBid = (if isForehand then (>=) else (>)) bid
          isForehand = playerIsForehand p
          bidOnlyForehand = bid `elem` forehandOnlyContracts

talonExchangeIsLegal :: PlayerHand -> TalonExchange -> Bool
talonExchangeIsLegal h te = True
