{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-- |

module Main
( -- * Entry points
  main
, runGame
  -- * Data structures
, Card (..)
, PlayerCard, PlayerDeck, GameCard, GameDeck, Deck
, Game (..), Player (..), GameS
, PlayerName

  -- * Initializers
, emptyDeck
, initialDeck
, newGame
, newPlayer
, mkCard, quit

, mkGameCard

  -- * Getter and Setter
, currentPlayer
, playerNumCards
, gamePoints

  -- * Stateful
, takeAnyCard
, nextPlayerTurn
, runRng

  -- * Helpers
, pass
, passM
, rndNumberRange
) where

import           Control.Lens
import           Control.Monad.State
import           Data.List
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Monoid
import           System.Random

-- | The player name
type PlayerName = String

-- | Helper
data Void a = Void deriving (Show, Eq)
instance Functor Void where fmap _ _ = Void

-- | set lens using a state function and pass the result
pass :: (MonadState t m) => Lens' t h -> (h -> (b, h)) -> m b
pass g f = do
  s <- use g
  let (n, s') = f s
  g .= s'
  return n

-- | set lens using a monadic state function and pass the result
passM :: (MonadState t m) => Lens' t h -> (h -> m (b, h)) -> m b
passM g f = do
  s <- use g
  (n, s') <- f s
  g .= s'
  return n

-- | generate a random number between the range
rndNumberRange :: (Num a, Random a, MonadState StdGen m) => (a, a) -> m a
rndNumberRange range = pass id (randomR range)

-- | abstract card type
data Card f =
    CardQuit (f PlayerName)
    | Card Int (f PlayerName)

deriving instance Eq PlayerCard
deriving instance Eq GameCard
deriving instance Show PlayerCard
deriving instance Show GameCard

-- | player cards dont need a name
type PlayerCard = Card Void -- ^ void discards the player name
type PlayerDeck = [PlayerCard]
-- | game card (main deck) needs reference to the player name
type GameCard = Card Identity -- ^ identity = only player name

-- type GameCard = Card Maybe -- ^ the card may have an owner

type GameDeck = [GameCard]
type Deck f = [Card f]

-- | base game
data Game = Game
          { _mainDeck   :: GameDeck -- ^ the main deck
          , _playerTurn :: [Player] -- ^ list of players in turn order
          , _rndGen     :: StdGen -- ^ number generator
          } deriving (Show)

-- | players
data Player = Player { _playerDeck :: PlayerDeck -- ^ current player hand (not deck)
                     , _playerName :: PlayerName -- ^ player name (string alias)
                     } deriving (Show)

makeLenses ''Game
makeLenses ''Player

emptyDeck :: Deck f
emptyDeck = []

-- | start deck for every player
initialDeck :: PlayerDeck
initialDeck = [mkCard 0, mkCard 0
              , mkCard 1, mkCard 1, mkCard 1
              , mkCard 2, mkCard 2
              , mkCard 3
              , quit, quit, quit
              ]

-- | initialize a new game with given players and number generator
newGame :: [PlayerName] -> StdGen -> Game
newGame playerNames gen = Game { _mainDeck = emptyDeck
                               , _playerTurn = fmap (newPlayer initialDeck) playerNames
                               , _rndGen = gen -- mkStdGen 2
                               }

-- | create a new player
newPlayer :: PlayerDeck -> PlayerName -> Player
newPlayer = Player

-- | create a new player card with given value
mkCard :: Int -> PlayerCard
mkCard val = Card val Void

-- | create a new finalizer for the playercard
quit :: PlayerCard
quit = CardQuit Void

-- | convert a player card to a game card
mkGameCard :: Player -> PlayerCard -> GameCard
mkGameCard player (CardQuit _) = CardQuit $ Identity (player^.playerName)
mkGameCard player (Card val _) = Card val $ Identity (player^.playerName)

-- | game monad
type GameS m a = (Monad m) => StateT Game m a

-- | getter / setter for the active player (first player in turn order)
currentPlayer :: Lens' Game Player
currentPlayer = lens
  (\game -> fromJust $ game^?playerTurn._head)
  (\game player -> game & playerTurn._head .~ player)

-- | get number of player cards
playerNumCards :: Getter Player Int
playerNumCards = playerDeck.to length -- to (\player -> length $ player^.playerDeck)

-- | calculate current points
gamePoints :: Getter Game (Map.Map PlayerName Int)
-- note: main deck is ordered newer to older
-- funfact: foldr starts at the end of the list so we dont need to reverse the list
gamePoints = to (\game -> snd $ foldr addCard (0, playerStartPoints game) (game^.mainDeck))
  where
    playerStartPoints game = Map.fromList . fmap (\p -> (p^.playerName, 0)) $ game^.playerTurn
    addCard card (points, pointMap) =
        case card of
          -- the player owning the quit card receives the accumulated points
          CardQuit (Identity player) -> (0, pointMap & at player._Just +~ points)
          -- default card, add points to total
          Card val _ -> (points + val, pointMap)

-- | take a random card from the player
takeAnyCard :: Player -> State StdGen (Maybe GameCard, Player)
takeAnyCard player = if player^.playerNumCards == 0
  then return (Nothing, player)
  else do
          cardIndex <- rndNumberRange (0, player^.playerNumCards-1)
          let card = (player^.playerDeck) !! cardIndex
          return (Just $ mkGameCard player card, player & playerDeck %~ delete card)

-- | TODO: use ring data structure
nextPlayerTurn :: GameS m ()
nextPlayerTurn = do
  p <- use currentPlayer
  playerTurn %= \players -> tail players `mappend` [p]

runRng :: State StdGen a -> GameS m a
runRng action = pass rndGen (runState action)

runGame :: GameS m ()
runGame = do
  -- take any card from the current player (maybe none)
  mCard <- passM currentPlayer (runRng . takeAnyCard)

  case mCard of
    Just card -> do
                   -- add new card to the main deck (prepend)
                   mainDeck <>= [card]
                   -- repeat
                   nextPlayerTurn
                   runGame
    -- player has no cards left
    Nothing -> return ()

-- | Entry point
main :: IO ()
main = do
  gen <- newStdGen
  let (_, g) = runStateT runGame (newGame ["A", "B", "C"] gen) Identity
  mapM_ print $ g^.mainDeck
  print $ g^.gamePoints
  return ()
