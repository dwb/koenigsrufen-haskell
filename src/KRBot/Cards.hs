module KRBot.Cards where

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Ratio
import System.Random

data SuitColour = Red | Black | Trump deriving (Show, Eq, Enum)
data Suit = Spades | Hearts | Clubs | Diamonds | Trumps
        deriving (Show, Eq, Ord, Enum, Bounded)
type Rank = Int
data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq)

data HalfTalon = HalfTalon Card Card Card deriving (Eq)
data FullTalon = FullTalon Card Card Card Card Card Card deriving (Eq)

class Talon t where
    talonToList :: t -> [Card]

instance Talon HalfTalon where
    talonToList (HalfTalon c1 c2 c3) = [c1, c2, c3]

instance Talon FullTalon where
    talonToList (FullTalon c1 c2 c3 c4 c5 c6) = [c1, c2, c3, c4, c5, c6]

instance Show HalfTalon where
    show = show . talonToList

instance Show FullTalon where
    show ft = show (take 3 tl) ++ " / " ++ show (drop 3 tl)
        where tl = talonToList ft

data TalonExchange = HalfTalonExchange {halfKept :: HalfTalon,
                                        halfTalonRejected :: HalfTalon} |
                     FullTalonExchange {fullTalonKept :: FullTalon}
                     deriving (Show, Eq)

suitColour :: Suit -> SuitColour
suitColour s = case s of
    Spades -> Black
    Clubs -> Black
    Hearts -> Red
    Diamonds -> Red
    Trumps -> Trump

colourSuitsIn = filter ((/= Trump) . suitColour)

allSuits, colourSuits, redSuits, blackSuits :: [Suit]
allSuits = enumFrom minBound
colourSuits = colourSuitsIn allSuits
redSuits = filter ((== Red) . suitColour) allSuits
blackSuits = filter ((== Black) . suitColour) allSuits

jack = 11
knight = 12
queen = 13
king = 14
rankIsPicture r = (r >= jack) && (r <= king)


suitFromLetter :: Char -> Maybe Suit
suitFromLetter sl = case toLower sl of
                        's' -> Just Spades
                        'h' -> Just Hearts
                        'c' -> Just Clubs
                        'd' -> Just Diamonds
                        _ -> Nothing

suitFromString s = case length s of
    1 -> suitFromLetter $ head s
    _ -> Nothing


cardFromShorthand :: String -> Maybe Card
cardFromShorthand s = do
    rank <- shRank
    suit <- shSuit
    return $ Card rank suit
    where   parseSh = reads s :: [(Int, String)]
            shRank = case parseSh of
                       [(r, _)] -> Just r
                       [] -> faceRank $ head s
            faceRank fr = case toLower fr of
                              'j' -> Just jack
                              'n' -> Just knight
                              'q' -> Just queen
                              'k' -> Just king
                              _ -> Nothing
            shSuit = case parseSh of
                        [(_, "")] -> Just Trumps
                        [(_, su)] -> suitFromLetter (head su)
                        [] -> suitFromLetter (last s)

cardsShorthandList :: String -> [Maybe Card]
cardsShorthandList = fmap cardFromShorthand . words

cardsShorthandListWithOriginal = fmap (\sh -> (sh, cardFromShorthand sh)) . words

isTrump :: Card -> Bool
isTrump (Card r s) = s == Trumps

cardPointsFromRules = subtract (2 % 3)

cardPoints c@(Card r s) = cardPointsFromRules pts
    where pts
            | isTrump c = case r of
                1 -> 5
                21 -> 5
                22 -> 5
                _ -> 1
            | r == jack = 2
            | r == knight = 3
            | r == queen = 4
            | r == king = 5
            | otherwise = 1

sumCardPoints = floor . fromRational . sum . fmap cardPoints

cardsWorth pts = filter (\ c -> (cardPointsFromRules pts) == cardPoints c) cardDeck

trumps, nonTrumps :: [Card] -> [Card]
trumps = filter isTrump
nonTrumps = filter (not . isTrump)

numTrumps :: [Card] -> Int
numTrumps = length . trumps

kings = filter ((== king) . rank) . nonTrumps

singletonKings cs = filter (not . flip elem nonKingSuits . suit) $ kings cs
    where nonKingSuits = nub . fmap suit $
                         filter ((/= king) . rank) . nonTrumps $ cs

type SuitCounts = Map.Map Suit Int

suitCounts :: [Card] -> SuitCounts
suitCounts = foldl f initialCounts
    where initialCounts = Map.fromList $ fmap (\s -> (s, 0)) allSuits
          f counts (Card _ s) = Map.adjust (+ 1) s counts

ranks = fmap rank

trumpRanks = ranks . trumps

voids :: [Card] -> [Suit]
voids cs = allSuits \\ nub (fmap suit cs)

instance Show Card where
  show (Card r s)
    | s == Trumps = trumpName r
    | rankIsPicture r = suitName r ++ suitSuffix
    | otherwise = defaultCardName
    where suitSuffix = " of " ++ show s
          defaultCardName = show r ++ suitSuffix
          suitName r
              | r == jack = "Jack"
              | r == knight = "Knight"
              | r == queen = "Queen"
              | r == king = "King"
          trumpName r = case r of
              1 -> "Pagat"
              2 -> "Uhu"
              3 -> "Kakadu"
              21 -> "Mond"
              22 -> "Sküs"
              _ -> defaultCardName


instance Ord Card where
  compare (Card r1 s1) (Card r2 s2)
    | s1 /= s2 = compare s1 s2
    | suitColour s1 /= Red = compare r1 r2
    | r1 <= 4 && r2 <= 4 = compare r2 r1
    | otherwise = compare r1 r2


cardDeck :: [Card]
cardDeck = sort . concat $ [trumpCards, faceCards, pipCards]
  where trumpCards = flip Card Trumps <$> [1..22]
        faceCards = Card <$> [11..14] <*> colourSuits
        pipCards = (Card <$> [1..4] <*> redSuits) ++
                   (Card <$> [7..10] <*> blackSuits)

shuffle xs = shuffle' xs (length xs)
    where shuffle' _ 0    = return []
          shuffle' xs len = do n           <- randomRIO (0, len - 1)
                               let (y, ys) =  choose n xs
                               ys'         <- shuffle' ys (len - 1)
                               return (y:ys')
          choose _ []     = error "choose: index out of range"
          choose 0 (x:xs) = (x, xs)
          choose i (x:xs) = let (y, ys) = choose (i - 1) xs in (y, x:ys)

shuffledDeck = shuffle cardDeck

dealIntoTalon (c1:c2:c3:c4:c5:c6:cs) = (FullTalon c1 c2 c3 c4 c5 c6, cs)

talonHalves (FullTalon c1 c2 c3 c4 c5 c6) = (HalfTalon c1 c2 c3, HalfTalon c4 c5 c6)

talonExchangeKept te = case te of
    HalfTalonExchange ht _ -> talonToList ht
    FullTalonExchange ft   -> talonToList ft
