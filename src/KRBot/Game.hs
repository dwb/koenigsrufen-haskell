{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module KRBot.Game where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Lens

import KRBot.GameRules
import KRBot.Cards
import KRBot.Contracts
import KRBot.Players
import KRBot.UI
import qualified KRBot.BotPlayer as BotPlayer
import qualified KRBot.PromptPlayer as PromptPlayer


data AnyPlayer = forall a. Player a => AnyPlayer a

type PlayerHands = Map.Map PlayerPosition PlayerHand

data GameState = GameState { _cards :: [Card],
                             _talon :: Maybe FullTalon,
                             _rejectedTalon :: [Card],
                             _players :: Map.Map PlayerPosition AnyPlayer,
                             _playerHands :: PlayerHands,
                             _declarer :: Maybe PlayerPosition,
                             _declarersPartner :: Maybe PlayerPosition,
                             _contract :: Maybe Contract,
                             _calledKing :: Maybe Suit,
                             _announcements :: [Announcement],
                             _tricks :: [Trick] }

makeLenses ''GameState

newtype GameAction a = GameAction {
    runGameAction :: StateT GameState IO a
    } deriving (Monad, Applicative, Functor, MonadIO, MonadState GameState)

runGameActionWithState :: GameAction a -> GameState -> IO (a, GameState)
runGameActionWithState f = runStateT $ runGameAction f

initialGameState :: IO GameState
initialGameState = do
    cards <- shuffledDeck
    return GameState { _cards = cards,
                       _talon = Nothing,
                       _rejectedTalon = [],
                       _players = Map.fromList [],
                       _playerHands = Map.fromList [],
                       _declarer = Nothing,
                       _declarersPartner = Nothing,
                       _contract = Nothing,
                       _calledKing = Nothing,
                       _announcements = [],
                       _tricks = []
                     }


testShowCallKingSuit = do
    (botSuit, _) <- runPlayerActionWithState callKingSuit $
        BotPlayer.initialPlayerState Player1 []
    (humanSuit, _) <- runPlayerActionWithState callKingSuit
        PromptPlayer.PlayerState
            {PromptPlayer._player = Player2, PromptPlayer._name = "Bob"}
    putStr "Bot chose: "
    print botSuit
    putStr "Human chose: "
    print humanSuit


initGame = initialGameState >>= runGameActionWithState runGame

runGame = do
    initAllBotPlayers
    initPromptPlayers
    playHand

playHand = do
    dealCards
    bidForContract
    getCalledKing
    declarerExchangeWithTalon
    bidForAnnouncements
    playTricks

{- Main Phases -}

dealCards :: GameAction ()
dealCards = do
    -- Nope, not the most concise way. But the *right* way.
    narrate "Dealing cards"
    initialCards <- use cards
    when (length initialCards /= length cardDeck) $ error "Can't deal from a short deck"
    cards' <- packetToEach initialCards
    let (talon', cards'') = dealIntoTalon cards'
    cards''' <- packetToEach cards''
    unless (null cards''') $ error "Not all cards were dealt"
    talon .= Just talon'
    phs <- use playerHands
    liftIO $ print talon'
    liftIO $ print phs
    where packetToEach cs = foldM packetToPlayer cs allPlayers
          packetToPlayer cs p = do
              let (packet, cs') = splitAt 6 cs
              doActionOfPlayer p $ receiveDealtCards packet
              playerHands %= mapAdjustWithDefault [] (sort . (++ packet)) p
              return cs'


bidForContract = bidForContract' allPlayers []
    where bidForContract' [p] bidsSoFar = do
            let ct = last . catMaybes $ bidsSoFar
            narrate $ "decided: " ++ show p ++ " bid " ++ show ct
            contract .= Just ct
            declarer .= Just p
            doActionOfAllPlayers $ notifyContract p ct
          bidForContract' (p:nextPlayers) bidsSoFar = do
            bid <- doActionOfPlayer p $ makeBid bidsSoFar
            if bidIsLegal bid p bidsSoFar then do
                let bidsSoFar' = bidsSoFar ++ [bid]
                let nextPlayers' = case bid of
                                    Nothing -> nextPlayers
                                    Just _  -> nextPlayers ++ [p]
                bidForContract' nextPlayers' bidsSoFar'
              else do
                doActionOfPlayer p $ notifyPlayerError IllegalBid
                bidForContract' (p:nextPlayers) bidsSoFar
                

getCalledKing = do
    contract' <- justContract
    when (contract' `elem` kingCallingContracts) $ do
        declarer' <- justDeclarer
        ck <- doActionOfPlayer declarer' callKingSuit
        calledKing .= Just ck
        narrate $ show ck ++ " was called"
        doActionOfAllPlayers $ notifyKingSuit ck
        dp <- findDeclarersPartner
        declarersPartner .= dp

declarerExchangeWithTalon = do
    contract' <- justContract
    when (contract' `elem` talonExchangeContracts) $ do
        declarer' <- justDeclarer
        talon' <- justTalon
        narrate $ "Talon is: " ++ show talon'
        doExchange contract' declarer' talon'
    where verifyExchange te talon' =
            let ths = talonHalves talon' in
            case te of
                HalfTalonExchange t _ -> t `elem` [fst ths, snd ths]
                FullTalonExchange t   -> t == talon'
          doExchange contract' declarer' talon' = do
            te <- doActionOfPlayer declarer' $ exchangeWithTalon talon'
            return $ (not $ verifyExchange te talon') &&
                error "I haven't got back the same talon as I gave out"
            case te of
                HalfTalonExchange kept rejected ->
                    if contract' `elem` [Rufer, Besserrufer, Dreier, Besserdreier] then do
                        narrate $ "Kept " ++ show kept
                        rejectedTalon .= talonToList rejected
                        doActionOfPlayer declarer' $ receiveDealtCards $ talonToList kept
                    else error "Wrong contract"
                FullTalonExchange rejected ->
                    if contract' == Sechserdreier then
                        rejectedTalon .= talonToList rejected
                    else error "Wrong contract"



bidForAnnouncements = return ()
playTricks = return ()

{- Pure Helpers -}

playersWithHandMatching :: PlayerHands -> ([Card] -> Bool) -> [PlayerPosition]
playersWithHandMatching playerHands f = Map.keys . Map.filter f $ playerHands

playerWithHandMatching :: PlayerHands -> ([Card] -> Bool) -> PlayerPosition
playerWithHandMatching playerHands f = case playersWithHandMatching playerHands f of
    [p] -> p
    _   -> error "No player matches"

{- State Helpers -}

findDeclarersPartner :: GameAction (Maybe PlayerPosition)
findDeclarersPartner = do
    declarer' <- justDeclarer
    contract' <- justContract
    if contract' `elem` kingCallingContracts then do
        calledKing' <- justCalledKing
        phs <- use playerHands
        let f = elem $ Card king calledKing'
        return $ case playersWithHandMatching phs f of
                      [p] -> Just p
                      _   -> Nothing
      else return Nothing

justDeclarer = liftM (fromMaybe $ error "No declarer yet!") $ use declarer
justContract = liftM (fromMaybe $ error "No contract yet!") $ use contract
justTalon = liftM (fromMaybe $ error "No talon yet!") $ use talon
justCalledKing = liftM (fromMaybe $ error "No called king yet!") $ use calledKing

initBotPlayer = do
    ps@BotPlayer.PlayerState {BotPlayer._player = pn} <- liftIO inputInitialBotPlayerState
    initPlayer pn ps

initAllBotPlayers = forM_ allPlayers (\ pn ->
    initPlayer pn $ BotPlayer.initialPlayerState pn [])

initPromptPlayers = do
    ps <- use players
    forM_ (allPlayers \\ Map.keys ps) initPromptPlayer


initPromptPlayer :: PlayerPosition -> GameAction ()
initPromptPlayer p = do
    pn <- (liftIO . inputInitialPromptPlayerState) p
    initPlayer p PromptPlayer.PlayerState
        {PromptPlayer._player = p, PromptPlayer._name = pn}


initPlayer :: Player a => PlayerPosition -> a -> GameAction ()
initPlayer p ps = players %= Map.insert p (AnyPlayer ps)


putPlayerState :: Player a => PlayerPosition -> a -> GameAction ()
putPlayerState p ps = players %= Map.insert p (AnyPlayer ps)


doPlayerAction :: (forall a. Player a => PlayerAction a b) -> PlayerPosition -> GameAction b
doPlayerAction paf p = do
    players' <- use players
    case fromJust . Map.lookup p $ players' of
        AnyPlayer ps -> do
            (result, ps') <- liftIO $ runPlayerActionWithState paf ps
            putPlayerState p ps'
            return result

doActionOfPlayer :: PlayerPosition -> (forall a. Player a => PlayerAction a b) -> GameAction b
doActionOfPlayer = flip doPlayerAction

doActionOfPlayers :: [PlayerPosition] -> (forall a. Player a => PlayerAction a b) -> GameAction ()
doActionOfPlayers ps paf = forM_ ps $ doPlayerAction paf

doActionOfAllPlayers = doActionOfPlayers allPlayers

doActionOfPlayersExcept :: PlayerPosition -> (forall a. Player a => PlayerAction a b) -> GameAction ()
doActionOfPlayersExcept exceptPlayer = doActionOfPlayers players
    where players = delete exceptPlayer allPlayers

narrate :: String -> GameAction ()
narrate s = liftIO $ putStrLn $ "Game: " ++ s

mapAdjustWithDefault :: Ord k => a -> (a -> a) -> k -> Map.Map k a -> Map.Map k a 
mapAdjustWithDefault dflt f k m = Map.insert k (f val) m
    where val = Map.findWithDefault dflt k m
