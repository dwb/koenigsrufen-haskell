module KRBot.Contracts where

import KRBot.Cards

import Data.List
import Data.Char
import Control.Monad
import Control.Applicative

data Contract = Rufer | Trischaken | Sechserdreier | Solo | Piccolo |
                Besserrufer | Bettel | Dreier | PiccoloOuvert |
                Besserdreier | BettelOuvert | Solodreier | ContractPass
                deriving (Show, Eq, Ord, Enum, Bounded)

data Announcement = PagatUltimo | KingUltimo | UhuUltimo | KakaduUltimo |
                    FortyFive | Valat | AnnouncementPass |
                    KontraGame | Kontra Announcement
                    deriving (Show, Eq)


allContracts :: [Contract]
allContracts = enumFrom minBound
forehandOnlyContracts = [Rufer, Trischaken, Sechserdreier]
nonForehandContracts = allContracts \\ forehandOnlyContracts

positiveContracts = [Rufer, Sechserdreier, Solo, Besserrufer, Dreier,
                     Besserdreier, Solodreier]
negativeContracts = allContracts \\ positiveContracts

kingCallingContracts = [Rufer, Solo, Besserrufer]
talonExchangeContracts = [Rufer, Sechserdreier, Besserrufer, Dreier,
                          Besserdreier]

contractIsPositive = flip elem positiveContracts
contractIsNegative = not . contractIsPositive


contractFromString :: String -> Maybe Contract
contractFromString str = case map toLower str of
    "r" -> Just Rufer
    "t" -> Just Trischaken
    "6d" -> Just Sechserdreier
    "s" -> Just Solo
    "p" -> Just Piccolo
    "br" -> Just Besserrufer
    "b" -> Just Bettel
    "d" -> Just Dreier
    "po" -> Just PiccoloOuvert
    "bd" -> Just Besserdreier
    "bo" -> Just BettelOuvert
    "sd" -> Just Solodreier
    "x" -> Just ContractPass
    _ -> Nothing


announcementFromString :: String -> Maybe Announcement
announcementFromString str = case map toLower str of
    "1" -> Just PagatUltimo
    "2" -> Just UhuUltimo
    "3" -> Just KakaduUltimo
    "k" -> Just KingUltimo
    "45" -> Just FortyFive
    "v" -> Just Valat
    "x" -> Just AnnouncementPass
    _ -> Nothing


bestContractForHand :: [Card] -> [Maybe Contract] -> Maybe Contract
bestContractForHand hand bids
    | nt >= 10 && ngt >= 4 = bid Solodreier
    | null bids && nt >= 7 && ngt >= 3 = bid Dreier
    -- TODO: forehand negative contracts
    | null bids = bid Rufer
    | ruferPassedAround && nt >= 7 && ngt >= 1 = bid Sechserdreier
    | ruferPassedAround && nt <= 3 && ngt <= 1 = bid Trischaken
    | ruferPassedAround = bid Rufer
    | highestBidOver Dreier && nt >= 9 && ngt >= 4 &&
        haveCard (Card 2 Trumps) = bid Besserdreier
    | highestBidOver Dreier && nt >= 9 && ngt >= 3 &&
        haveCard (Card 1 Trumps) = bid Besserdreier
    | nt >= 8 && ngt >= 3 && haveCard (Card 3 Trumps) = bid Besserrufer
    | nt >= 7 && ngt >= 3 && haveCard (Card 2 Trumps) = bid Besserrufer
    | nt >= 7 && ngt >= 4 = bid Dreier
    | nt >= 6 && ngt >= 2 && haveCard (Card 1 Trumps) = bid Besserrufer
    | nt >= 6 && ngt >= 2 = bid Solo
    -- TODO: Solo points condition
    | nt <= 2 && ngt == 1 && numColourVoids == 0 && npnt <= 8
        && nk == 0 = bid Piccolo
    | nt <= 1 && ngt == 0 && numColourVoids == 0 && nk == 1
        && npnt <= 10 = bid Piccolo
    | highestBidOver Piccolo && nt == 1 && haveCard (Card 22 Trumps) && npnt <= 5
        && numColourVoids == 0 = bid PiccoloOuvert
    | highestBidOver Piccolo && nt == 0 && npnt <= 8 && nsk == 1
        && numColourVoids == 0 = bid PiccoloOuvert
    | otherwise = Nothing
    where   nt = numTrumps hand
            ngt = length $ filter ((>= 17) . rank) $ trumps hand
            np = sumCardPoints hand
            npnt = sumCardPoints $ nonTrumps hand
            nk = length $ kings hand
            nsk = length $ singletonKings hand
            handVoids = voids hand
            numColourVoids = length $ colourSuitsIn handVoids
            haveCard = flip elem hand
            highestBid = case filter (/= Nothing) bids of
                            [] -> Nothing
                            cs -> last cs
            isForehand = mod (length bids) 4 == 0
            compareHighestBid ifNothing f c = case highestBid of
                Nothing -> ifNothing
                Just hb -> f c hb
            highestBidUnder = compareHighestBid True (>)
            highestBidOver = compareHighestBid False (<)
            highestBidAtLeast = compareHighestBid False (<=)
            notOutbid = compareHighestBid True (if isForehand then (>=) else (>))
            bid c = if notOutbid c then Just c else Nothing
            ruferPassedAround = bids == [Just Rufer, Nothing, Nothing, Nothing]
