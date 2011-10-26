{-# LANGUAGE TemplateHaskell #-}

module KRBot.PromptPlayer where

import Control.Monad.State (liftIO, get)
import Text.Printf (printf)
import Control.Lens

import KRBot.Cards
import KRBot.Contracts
import KRBot.Players
import KRBot.UI


data PlayerState = PlayerState {_player :: PlayerPosition, _name :: String}

makeLenses ''PlayerState

instance Player PlayerState where
    receiveDealtCards = return $ liftIO $ putStrLn "You received some cards okay"
    makeBid bids = do
        name <- use name
        bid <- liftIO $ inputContract $ printf "Please enter the contract that %s called: " name
        return (if bid == Just ContractPass then Nothing else bid)
    callKingSuit = do
        name <- use name
        liftIO $ inputSuit $ printf
            "Please enter the suit that %s called: " name
    exchangeWithTalon = undefined
    makeAnnouncement = undefined
    playCardToTrick trick = undefined
    notifyPlayerError e = liftIO $ print e
