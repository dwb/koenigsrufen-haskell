{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KRBot.Players where

import Control.Applicative (Applicative)
import Control.Monad.State
import qualified Data.Map as Map

import KRBot.Cards
import KRBot.Contracts


data PlayerPosition = Player1 | Player2 | Player3 | Player4
                        deriving (Eq, Ord, Show, Enum, Bounded, Read)

data PlayerSide = Declarer | Defender deriving (Eq, Show)

type PlayerHand = [Card]

newtype PlayerAction a b = PlayerAction {
    runPlayerAction :: StateT a IO b
    } deriving (Functor, Applicative, Monad, MonadIO, MonadState a)

data PlayerError = IllegalBid | IllegalTalonExchange | IllegalAnnoucement |
                   IllegalCard
                   deriving (Eq)

instance Show PlayerError where
    show IllegalBid = "That is not a legal bid"
    show IllegalTalonExchange = "You cannot exchange those cards"
    show IllegalAnnoucement = "That is not a legal announcement"
    show IllegalCard = "You cannot play that card on this trick"

class Player a where
    receiveDealtCards :: [Card] -> PlayerAction a ()
    makeBid :: [Maybe Contract] -> PlayerAction a (Maybe Contract)
    notifyContract :: PlayerPosition -> Contract -> PlayerAction a ()
    notifyContract _ _ = return ()
    callKingSuit :: PlayerAction a Suit
    notifyKingSuit :: Suit -> PlayerAction a ()
    notifyKingSuit _ = return ()
    exchangeWithTalon :: FullTalon -> PlayerAction a TalonExchange
    notifyTalonExchange :: TalonExchange -> PlayerAction a ()
    notifyTalonExchange _ = return ()
    makeAnnouncement :: [Maybe Announcement] -> PlayerAction a (Maybe Announcement)
    playCardToTrick :: [Card] -> PlayerAction a Card 
    notifyPlayerError :: PlayerError -> PlayerAction a ()


runPlayerActionWithState :: Player s => PlayerAction s a -> s -> IO (a, s)
runPlayerActionWithState f = runStateT (runPlayerAction f)


allPlayers :: [PlayerPosition]
allPlayers = enumFrom minBound

numPlayers = length allPlayers

playerIsForehand :: PlayerPosition -> Bool
playerIsForehand = (== Player1)

playerOrderFrom :: PlayerPosition -> [PlayerPosition]
playerOrderFrom p = take numPlayers (enumFrom p ++ allPlayers)

playerFromString :: String -> Maybe PlayerPosition
playerFromString s = case parse s of
    [(p, "")] -> Just p
    _ -> Nothing
    where parse s = reads ("Player" ++ s) :: [(PlayerPosition, String)]

playerNumber :: PlayerPosition -> Int
playerNumber = (+ 1) . fromEnum

type PlayerVoids = Map.Map PlayerPosition [Suit]

emptyPlayerVoids :: PlayerVoids
emptyPlayerVoids = Map.empty

voidsForPlayer :: PlayerVoids -> PlayerPosition -> [Suit]
voidsForPlayer pv p = Map.findWithDefault [] p pv

addVoidsForPlayer :: PlayerVoids -> PlayerPosition -> [Suit] -> PlayerVoids
addVoidsForPlayer pv p ss = Map.insertWith (++) p ss pv
