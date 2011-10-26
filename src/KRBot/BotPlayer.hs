{-# LANGUAGE TemplateHaskell #-}

module KRBot.BotPlayer (initialPlayerState, PlayerState(PlayerState), _player) where

import Data.List
import Data.Foldable (foldMap)
import Data.Monoid
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State (MonadState, liftIO)
import Control.Lens

import KRBot.Cards
import KRBot.Contracts
import KRBot.Players

data PlayerState = PlayerState {_player :: PlayerPosition, _hand :: [Card],
                                _contract :: Maybe Contract,
                                _calledKing :: Maybe Suit,
                                _playerSide :: Maybe PlayerSide,
                                _ownAnnouncements :: [Announcement],
                                _theirAnnouncements :: [Announcement],
                                _cardsInPlay :: [Card],
                                _playerVoids :: PlayerVoids}
                                deriving (Eq, Show)

makeLenses ''PlayerState

instance Player PlayerState where
    receiveDealtCards cs = do
        hand <>= cs
        narrate $ "was dealt " ++ show cs
        hand' <- uses hand sort
        narrate $ "hand now " ++ show hand'
    makeBid bids = do
        hand <- use hand
        unless (length hand == 12) $ playerError $
            "doesn't have a full hand! Look: " ++ show hand
        let bid = bestContractForHand hand bids
        narrate $ "bid " ++ show bid
        return bid
    callKingSuit = do
        hand <- use hand
        let s = fromMaybe noSuitError $ findKingSuit hand
        narrate $ "called " ++ show s
        return s
        where callSingleton cs = case filterSuitCounts (== 1) cs of
                                     s:_ -> Just s
                                     _   -> Nothing
              callVoid cs = case filterSuitCounts (== 0) cs of
                                s:_ -> Just s
                                _   -> Nothing
              callFirst cs = case nonTrumps cs of
                                 Card _ s:_ -> Just s
                                 _          -> Nothing
              noSuitError = error "No king calling suit found: should have bid higher!"
              filterSuitCounts f cs = fmap fst $ Map.toList $ Map.filter f $ suitCounts cs
              findKingSuit cs = getFirst $ foldMap First [callSingleton cs,
                                                          callVoid cs,
                                                          callFirst cs]
    exchangeWithTalon talon = do
        let (h1, h2) = talonHalves talon
        return $ HalfTalonExchange h1 h2
    makeAnnouncement = undefined
    playCardToTrick trick = undefined
    notifyPlayerError e = do
        _ <- error $ "Internal bot error: " ++ show e
        return ()


initialPlayerState :: PlayerPosition -> [Card]-> PlayerState
initialPlayerState p h = PlayerState {_player = p, _hand = h, 
                                      _calledKing = Nothing,
                                      _contract = Nothing, _playerSide = Nothing,
                                      _ownAnnouncements = [],
                                      _theirAnnouncements = [],
                                      _cardsInPlay = cardDeck \\ h,
                                      _playerVoids = emptyPlayerVoids}

narrate :: String -> PlayerAction PlayerState ()
narrate s = do
    p <- use player
    liftIO $ putStrLn $ show p ++ " bot " ++ s

playerError s = do
    p <- use player
    error $ show p ++ " " ++ s
