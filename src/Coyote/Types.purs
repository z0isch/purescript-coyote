module Coyote.Types where

import Prelude

import Control.Monad.Except (except)
import Control.Monad.State (StateT, get, gets, modify_, runStateT)
import Coyote.UntaggedSumRep (untaggedSumRep)
import Data.Array (any)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (fold, foldM, for_, foldMap)
import Data.Generic.Rep (class Generic, to)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List as L
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map, insert, size, values)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Traversable (for, maximum, sum)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Foreign (ForeignError(..))
import Partial.Unsafe (unsafePartial, unsafePartialBecause)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write)

data SpeciaCard
  = Question
  | Max0
  | MaxNeg
  | X2
  | Night
derive instance genericSpeciaCard :: Generic SpeciaCard _
instance eqSpeciaCard :: Eq SpeciaCard where
  eq = genericEq
instance showSpeciaCard :: Show SpeciaCard where
  show = genericShow
instance ordSpecialCard :: Ord SpeciaCard where
  compare = genericCompare
instance readForeignSpecialCard :: ReadForeign SpeciaCard where
  readImpl f = do
    readImpl f >>= case _ of
      "Question" -> pure Question
      "Max0" -> pure Max0
      "MaxNeg" -> pure MaxNeg
      "X2" -> pure X2
      "Night" -> pure Night
      s -> except $ Left $ NonEmpty.singleton $ ForeignError $ "Incorrect Special Card: " <> s
instance writeForeignSpecialCard :: WriteForeign SpeciaCard where
  writeImpl = write <<< show

data Card 
  = Feather Int
  | SpecialCard SpeciaCard
instance eqCard :: Eq Card where
  eq = genericEq
derive instance genericCard :: Generic Card _
instance showCard :: Show Card where
  show = genericShow
instance readForeignCard :: ReadForeign Card where
  readImpl f = to <$> untaggedSumRep f
instance writeForeignCard :: WriteForeign Card where
  writeImpl (SpecialCard s) = write $ show s
  writeImpl (Feather s) = write s
  
data Move 
  = Bid Int
  | Coyote
derive instance genericMove :: Generic Move _
instance showMove :: Show Move where
  show = genericShow

type Player = Int

type Hand = Array Card

type PlayerState = 
  { hand :: Hand
  , coins :: Int
  }

type Round = 
  { bid :: Tuple Player Int
  , winner :: Player
  , loser :: Player
  , total :: Int
  , hands :: Map Player Hand
  }

type GameState =
  { deck :: Array Card
  , discardPile :: Array Card
  , currentPlayer :: Player
  , players :: Map Player PlayerState
  , currentBid :: Maybe (Tuple Player Int)
  , previousRounds :: Array Round
  }

unshuffledDeck :: Array Card
unshuffledDeck 
  =  A.replicate 4 (Feather 1) 
  <> A.replicate 4 (Feather 2) 
  <> A.replicate 4 (Feather 3) 
  <> A.replicate 4 (Feather 4) 
  <> A.replicate 4 (Feather 5) 
  <> A.replicate 3 (Feather 10) 
  <> A.replicate 2 (Feather 15)
  <> A.replicate 1 (Feather 20)
  <> A.replicate 2 (Feather (-5))
  <> A.replicate 1 (Feather (-10))
  <> map SpecialCard [Question, Max0, MaxNeg, X2, Night]

initialGame :: Effect GameState
initialGame = do
  s <- shuffle unshuffledDeck
  pure 
    { deck: s
    , discardPile: mempty
    , currentPlayer: 0
    , players: mempty
    , currentBid: Nothing
    , previousRounds: []
    }

initialPlayer :: PlayerState
initialPlayer =
  { hand: []
  , coins: 0
  }

roundNum :: GameState -> Int
roundNum {players} = unwrap $ foldMap (Additive <<< _.coins) players

addPlayer :: GameState -> GameState
addPlayer gs = gs
  { players= insert (size gs.players) initialPlayer gs.players
  }

nextPlayer :: GameState -> Player
nextPlayer gs@{currentPlayer, players} = (currentPlayer + 1 + addBy) `mod` numPlayers
  where
    numPlayers = size players
    nextP = (currentPlayer + 1) `mod` numPlayers
    looped = L.drop nextP (values players) <> values players
    addBy = L.length $ L.takeWhile (\{coins} -> coins >= 3) looped

bleh = runStateT processTotal

drawTop :: StateT GameState Effect Card
drawTop = do
  {deck} <- get
  when (A.null deck) $ do
    {discardPile} <- get
    newDeck <- liftEffect $ shuffle discardPile
    modify_ _
      { deck= newDeck
      , discardPile= []
      }
  {head,tail} <- gets \s -> unsafePartialBecause "Deck should not be empty" $ fromJust $ A.uncons s.deck
  modify_ _{deck= tail}
  pure head

processTotal :: StateT GameState Effect Int
processTotal = do
  {players} <- get
  qProcessed <- processQ $ seperateCards $ M.toUnfoldable players
  allProcessed <- processNonQ $ sortSpecial qProcessed
  pure $ sum allProcessed
  where
    processQ s = case s.q of
      Just (Additive pl) -> do
        c <- drawTop
        --Add this card to the persons hand
        modify_ \st -> st{players= M.update (\hs -> Just $ hs{hand= c A.: hs.hand}) pl st.players}
        case c of
          Feather x -> pure s{feather= x L.: s.feather }
          --This can't be a Question because there is only one in the deck
          SpecialCard x -> pure s{nonQ= x L.: s.nonQ }
      Nothing -> pure s

    processNonQ s = foldM processSpecial s.feather s.nonQ
    
    sortSpecial s= s{nonQ= L.sort s.nonQ}
    
    --Abusing the monoid instance of records
    seperateCards = fold <<< A.concatMap (\(Tuple pl {hand}) -> map (seperate pl) hand)
      where
        seperate _ (Feather x) = {feather: L.singleton x,nonQ: mempty,q: mempty}
        seperate pl (SpecialCard Question) = {feather: mempty,nonQ: mempty,q: Just (Additive pl)}
        seperate _ (SpecialCard x) = {feather:mempty, nonQ: L.singleton x,q: mempty}

    processSpecial :: L.List Int -> SpeciaCard -> StateT GameState Effect (L.List Int)
    processSpecial feathers = case _ of
      Max0 -> pure $ L.delete maxF feathers
      MaxNeg -> pure $ fromMaybe feathers $ L.updateAt maxI (negate maxF) feathers
      X2 -> pure $ map (\x -> if x >= 0 then 2 * x else x) feathers
      _ -> pure feathers
      where 
        maxI = fromMaybe 0 $ L.elemIndex maxF feathers
        maxF = fromMaybe 0 $ maximum feathers

makeMove :: Move -> StateT GameState Effect Unit
makeMove (Bid bid) = modify_ \gs -> gs
  { currentBid= Just (Tuple gs.currentPlayer bid)
  , currentPlayer= nextPlayer gs
  }
makeMove Coyote = do
  total <- processTotal
  {players, currentBid, currentPlayer} <- get
  let 
    needsShuffled   = any (any ((==) (SpecialCard Night)) <<< _.hand) players
    Tuple bidPl bid = unsafePartialBecause "You can't call coyote when there is no bid" $ fromJust currentBid
    {loser,winner}  = if (bid <= total)
                      then {loser: currentPlayer, winner: bidPl}
                      else {loser: bidPl, winner: currentPlayer}
  modify_ \g -> g
    { players= map _{hand= []} $ M.update (\p -> Just p{coins= p.coins + 1}) loser g.players
    , discardPile=  (A.concatMap _.hand (A.fromFoldable (M.values g.players))) <> g.discardPile
    , previousRounds= g.previousRounds <> [ 
        { bid: Tuple bidPl bid
        , winner: winner
        , loser: loser
        , hands: map _.hand g.players
        , total: total
        }]
    , currentBid= Nothing
    , currentPlayer= winner
    }
  when needsShuffled do
    shuffled <- (gets \g -> g.deck <> g.discardPile) >>= liftEffect <<< shuffle
    modify_ _
      { discardPile= []
      , deck= shuffled
      }
  drawCards
  where
    drawCards = do
      {players} <- get
      let (pls :: Array (Tuple Player PlayerState)) = M.toUnfoldable players
      for_  pls \(Tuple plNum pl) -> when (pl.coins < 3) do
        c <- drawTop
        modify_ \g -> g
          { players= M.update (Just <<< _{hand= [c]}) plNum g.players
          }

shuffle :: forall a. Array a -> Effect (Array a)
shuffle xs = do
  ar <- Ref.new xs
  void $ for (A.range 0 (A.length xs - 1)) \i -> do
    j <- randomInt i (A.length xs - 1)
    swap' ar i j
  Ref.read ar
  where 
    swap' ar i j = do
      vi <- index' ar i
      vj <- index' ar j
      Ref.modify (A.updateAtIndices [Tuple i vj, Tuple j vi]) ar
    index' ar i = do
      mar <- Ref.read ar
      pure $ unsafePartial $ fromJust $ A.index mar i

test1 :: GameState
test1 = { previousRounds: [], currentBid: Nothing, currentPlayer: 0, deck: [(SpecialCard X2),(Feather 2),(Feather (-5)),(Feather 3),(SpecialCard Question),(Feather 15),(SpecialCard Night),(Feather 3),(Feather 4),(Feather 5),(Feather 5),(Feather 1),(Feather 3),(Feather 2),(Feather 4),(Feather 1),(Feather 4),(Feather (-10)),(SpecialCard MaxNeg),(Feather 20),(Feather 1),(Feather 3),(Feather 2),(Feather 2),(Feather 10),(Feather 5),(Feather 1),(Feather (-10)),(SpecialCard Max0),(Feather 15),(Feather (-5)),(Feather 10),(Feather 10),(Feather 5),(Feather 4)], discardPile: [], players: (M.fromFoldable [(Tuple 0 { hand: [Feather 5], coins: 0 }),(Tuple 1 { hand: [Feather (-10)], coins: 0 }), (Tuple 2 { hand: [SpecialCard Question], coins: 0 })]) }