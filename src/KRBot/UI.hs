module KRBot.UI where

import System.IO
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Char

import KRBot.Cards
import KRBot.Players
import KRBot.Contracts
import qualified KRBot.BotPlayer as BotPlayer


showMaybeContract :: Maybe Contract -> String
showMaybeContract mc = case mc of
    Nothing -> "Pass"
    Just c -> show c


getParsedInput :: String -> (String -> a) -> [a -> Maybe String] -> IO a
getParsedInput prompt parsef verifiers = do
    putStr prompt
    hFlush stdout
    input <- liftM parsef getLine
    case filter (/= Nothing) $ verifiers <*> [input] of
        [] -> return input
        failures -> do
            forM_ failures (putStrLn . fromJust)
            getParsedInput prompt parsef verifiers


getPromptedInput prompt = getParsedInput prompt id []


inputCards :: Int -> String -> IO [Card]
inputCards numCardsRequired prompt = liftM cardsOnly $ getParsedInput prompt
        (nub . cardsShorthandListWithOriginal)
        [\cs -> if length cs /= numCardsRequired
                     then Just $ "You didn't enter " ++ show numCardsRequired
                         ++ " " ++ cardCards numCardsRequired ++ "."
                     else Nothing,
         \cs -> let failedLookups = filter ((== Nothing) . snd) cs in
                 if failedLookups /= []
                     then Just $ "Didn't recognise " ++
                         cardCards (length failedLookups) ++ ": " ++
                         intercalate ", "
                             (fmap (show . fst) failedLookups)
                     else Nothing]
    where cardCards num = "card" ++ (if num == 1 then "" else "s")
          cardsOnly = fmap (fromJust . snd)


inputCard = liftM head . inputCards 1


inputPlayer :: String -> IO PlayerPosition
inputPlayer prompt = liftM fromJust $ getParsedInput prompt playerFromString
    [\mp -> if isNothing mp
                 then Just "Enter a valid player number (1-4, 1 = forehand)"
                 else Nothing]


inputSuit :: String -> IO Suit
inputSuit prompt = liftM fromJust $ getParsedInput prompt suitFromString
    [\ms -> if isNothing ms
                 then Just "Enter a valid suit letter (s, h, c, d)"
                 else Nothing]


inputNonEmptyString :: String -> IO String
inputNonEmptyString prompt = getParsedInput prompt id
    [\str -> let str' = filter (not . isSpace) str in
        if str' == ""
            then Just "You must enter a value"
            else Nothing]


inputContract :: String -> IO (Maybe Contract)
inputContract prompt = getParsedInput prompt contractFromString
    [\ct -> if isNothing ct
                then Just "Enter a valid contract"
                else Nothing]


inputInitialBotPlayerState = do
    player <- inputPlayer "What is your player number? "
    hand <- inputCards 12 "Enter hand: "
    return $ BotPlayer.initialPlayerState player hand


inputInitialPromptPlayerState p = inputNonEmptyString $
    "Enter name for human player " ++ show (playerNumber p) ++ ": "
