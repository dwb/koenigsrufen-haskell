module KRBot.Tricks where

import Data.List

import KRBot.Cards
import KRBot.Contracts
import KRBot.Game


winningCard :: [Card] -> Card
winningCard trick = maximum candidates
    where   lead = head trick
            candidates = filter ((`elem` [suit lead, Trumps]) . suit) trick


{- bestCardToPlay :: [Card] -> PlayerState -> Card
 - bestCardToPlay trick gameState = undefined -}
