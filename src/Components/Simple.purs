module Components.Simple where

import Prelude

import Components.QRCode as QRCode
import Coyote.Full as Full
import Coyote.Simple as Simple
import Coyote.Web.Types (WebGame, CoyoteCookie)
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Either (Either(..))
import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Class.Console (error, log)
import Halogen (liftAff)
import Halogen as H
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.NoSleep as NoSleep

type State = 
  { input :: Input
  , game :: Maybe (WebGame Simple.GameState)
  , showingHand :: Boolean
  , updatedOldGameState :: Boolean
  , countdownToShowHand :: Maybe Int
  , waitingForCard :: Boolean
  }
  
data Query a 
  = NewGameClick a
  | GameUpdate (WebGame Simple.GameState) a
  | ExitGame a
  | DrawCardClick a
  | CoyoteClick a
  | UpdatedOldGameState a
  | DismissUpdatedOldGameState a
  | HandleInput Input a

type Input = 
  { cookie :: Maybe CoyoteCookie
  , baseUrl :: String
  }

data Message 
  = UnsubscribeFromGame
  | CreateNewGame
  | DrawCard CoyoteCookie (WebGame Simple.GameState)
  | CallCoyote CoyoteCookie (WebGame Simple.GameState)

type ChildQuery = Coproduct1 QRCode.Query
type ChildSlot = Either1 Unit

ui :: H.Component HH.HTML Query Input Message Aff
ui = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: HE.input HandleInput
  }
  where
    initialState :: Input -> State
    initialState input = 
      { input
      , game: Nothing
      , showingHand: false
      , updatedOldGameState: false
      , countdownToShowHand: Nothing
      , waitingForCard: false
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
    render s = HH.div 
      [HP.classes [H.ClassName "container-fluid"]] $
      [ 
        -- HH.p_ 
        --   [ HH.pre_ [HH.text $ maybe "" (show <<< _.stateHash) s.game]
        --   ]
      ] <> 
      [ fromMaybe notInGame (inGame s <$> s.input.cookie <*> s.game) ]

    notInGame = HH.div_
      [ HH.p_
        [ HH.h1 
          [ HP.class_ $ H.ClassName "display-1 text-center"
          ]
          [HH.text "Coyote"]
        ]
      , HH.p_
        [ HH.button 
          [ HE.onClick $ HE.input_ NewGameClick
          , HP.class_ $ H.ClassName "btn btn-lg btn-block btn-primary"
          ] 
          [ HH.text "Start new game"]
        ]
      ]
      
    inGame {showingHand, waitingForCard, countdownToShowHand, input:{baseUrl}} {userId,id} {playerMap,state} = case M.lookup userId playerMap of 
      Nothing -> HH.p_ []
      Just player -> HH.p_ case countdownToShowHand of
        Nothing -> if showingHand
          then if waitingForCard
            then  
              [ HH.div_
                [ HH.h3 [HP.class_ $ H.ClassName "text-center"] [HH.text "Still waiting on your card..."]]
              ]
            else case myCard player of
              Nothing -> 
                [ HH.div_
                  [ HH.h3 [HP.class_ $ H.ClassName "text-center"] [HH.text "Oops, tell them to draw another."]
                  , HH.button 
                    [ HE.onClick $ HE.input_ DrawCardClick
                    , HP.class_ $ H.ClassName "btn btn-lg btn-block btn-info"
                    ] 
                    [ HH.text "Draw a card"]
                  ]
                ]
              Just card ->
                [ HH.div
                  [ HP.class_ $ H.ClassName "row"]
                  [ HH.div
                    [ HP.class_ $ H.ClassName "col-2"] 
                    [ HH.table
                      [ HP.class_ $ H.ClassName "table table-borderless table-sm table-striped coyote-table" ]
                      [ HH.tbody_ $ A.concat $ (A.sortWith sortFeathers $ A.group (A.filter isPosFeather Full.unshuffledDeck)) <#> \cs -> 
                      [ HH.tr_
                        [ HH.th_
                          [ HH.text $ showCard $ NA.head cs ]
                        , HH.td_
                          [ HH.text $ show (A.length (A.filter ((==) (NA.head cs)) state.discardPile)) <> "/" <> show (NA.length cs) ]
                        ]
                      ]
                    ]
                  ]
                , HH.div
                    [ HP.class_ $ H.ClassName "col-7"]
                    [ HH.h1
                      [ HP.class_ $ H.ClassName "coyote-card text-center"]
                      [ HH.text $ showCard card]
                    , HH.button 
                      [ HE.onClick $ HE.input_ CoyoteClick
                      , HP.class_ $ H.ClassName "btn btn-sm btn-danger"
                      ] 
                      [ HH.text "Coyote!"]
                    ]
                  , HH.div
                    [ HP.class_ $ H.ClassName "col-3"] 
                    [ HH.table
                      [ HP.class_ $ H.ClassName "table table-borderless table-sm table-striped coyote-table" ]
                      [ HH.tbody_ $ A.concat $ (A.sortWith sortFeathers $ A.group (A.filter (not <<< isPosFeather) Full.unshuffledDeck)) <#> \cs -> 
                        [ HH.tr_
                          [ HH.th_
                            [ HH.text $ showCard $ NA.head cs ]
                          , HH.td_
                            [ HH.text $ show (A.length (A.filter ((==) (NA.head cs)) state.discardPile)) <> "/" <> show (NA.length cs) ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
          else 
            [ HH.button 
              [ HE.onClick $ HE.input_ ExitGame
              , HP.class_ $ H.ClassName "btn btn-sm btn-danger"
              ] 
              [ HH.text "Exit the game"]
            , HH.div
              [ HP.class_ $ H.ClassName "row" ]
              [ HH.div
                [ HP.class_ $ H.ClassName "col-5" ]
                [ HH.a
                  [ HP.href url ]
                  [ HH.slot' cp1 unit QRCode.ui url absurd ]
                ]
              , HH.div
                [ HP.class_ $ H.ClassName "col-7" ]
                [ case A.last state.previousRounds of
                  Nothing -> HH.h3
                    [ HP.class_ $ H.ClassName "text-center" ] 
                    [ HH.text $ "Round " <> show (A.length state.previousRounds) ]
                  Just {hands, total} -> HH.div_
                    [ HH.h3
                      [ HP.class_ $ H.ClassName "text-center" ] 
                      [ HH.u_ [HH.text $ "Round " <> show (A.length state.previousRounds) <> " Results" ]]
                    , if A.null state.discardPile
                      then HH.div
                        [ HP.class_ $ H.ClassName "alert alert-primary text-center" ] 
                        [ HH.text "Just Shuffled!" ]
                      else HH.span_ []
                    , HH.table
                        [ HP.class_ $ H.ClassName "table table-sm" ]
                        [ HH.tbody_ $ 
                          [ HH.tr
                            [ HP.class_ $ H.ClassName "table-primary" ]
                            [ HH.td_ [ HH.b_ [ HH.text "Total"]]
                            , HH.td_ [ HH.b_ [ HH.text $ show total ]]
                            ]
                          ] <> (A.concat $ M.toUnfoldable hands <#> \(Tuple pl cards) ->
                          [ HH.tr
                            [ HP.class_ $ H.ClassName $ if pl == player then "table-warning" else " " ]
                            [ HH.td_
                              [ HH.text $ (if pl == player then "My" else "Player " <> show pl <>"'s") <> " card"]
                            , HH.td_
                              [ HH.text $ joinWith " -> " $ A.reverse $ map showCard cards]
                            ]                            
                          ])
                        ]
                      ]
                  ]
              ]
            , HH.button 
              [ HE.onClick $ HE.input_ DrawCardClick
              , HP.class_ $ H.ClassName "btn btn-lg btn-block btn-info"
              ] 
              [ HH.text "Draw a card"]
            ]
        Just c -> 
          [ HH.p_
            [ HH.h1 
              [ HP.class_ $ H.ClassName "display-3 text-center" ]
              [HH.text "Flip your device around!"]
            , HH.h1
              [ HP.class_ $ H.ClassName "display-1 text-center" ]
              [ HH.text $ show c ]
            ]
          ]
      where
        sortFeathers cs = case NA.head cs of
          (Full.Feather x) -> x
          (Full.SpecialCard Full.Night) -> 0
          (Full.SpecialCard _) -> 1
          
        isPosFeather (Full.Feather x) = x > 0
        isPosFeather _ = false

        myCard pl = do
          {hand} <- M.lookup pl state.players
          A.head hand
        
        url = String.takeWhile (not <<< (==) (String.codePointFromChar '#')) baseUrl <> "#join/" <> id

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
    eval = case _ of

      HandleInput i next -> do
        H.modify_ _{input= i}
        pure next

      NewGameClick next -> do
        H.raise CreateNewGame
        pure next
        
      ExitGame next -> do
        H.raise UnsubscribeFromGame
        H.modify_ _{game= Nothing, showingHand= false}
        pure next
      
      CoyoteClick next -> do
        s <- H.get
        case s.input.cookie of
          Nothing -> pure next
          Just c -> case s.game of
            Nothing -> pure next
            Just g -> do
              H.raise $ CallCoyote c g
              H.modify_ _{showingHand= false}
              pure next
      
      DrawCardClick next -> do
        s <- H.get
        case s.input.cookie of
          Nothing -> pure next
          Just c -> case s.game of
            Nothing -> pure next
            Just g -> do
              H.liftAff $ attempt NoSleep.enable >>= case _ of
                  Left err -> error $ show err
                  Right _ -> pure unit
              H.raise $ DrawCard c g
              _ <- H.fork $ do
                H.modify_ _{countdownToShowHand= Just 3, waitingForCard= true}
                liftAff $ delay $ Milliseconds 1000.0 
                H.modify_ _{countdownToShowHand= Just 2}
                liftAff $ delay $ Milliseconds 1000.0 
                H.modify_ _{countdownToShowHand= Just 1}
                liftAff $ delay $ Milliseconds 1000.0
                H.modify_ _{countdownToShowHand= Nothing}
                let 
                  waitForCard = do
                    {game, input:{cookie}, waitingForCard} <- H.get
                    when waitingForCard do
                      let hand = ((<) 0 <<< A.length <<< _.hand) <$> (game >>= \g -> cookie >>= \c -> M.lookup c.userId g.playerMap >>= flip M.lookup g.state.players)
                      case hand of 
                        Just true -> H.modify_ _{showingHand= true, waitingForCard= false} 
                        _ -> liftAff (delay (Milliseconds 100.0)) *> waitForCard
                waitForCard
              pure next
  
      GameUpdate new next -> do
        {game, input:{cookie}} <- H.get
        
        case game of
          Nothing -> do
            H.modify_ _{game= Just new}
            pure unit
          Just oldGame -> do
            --Someone called Coyote!
            when (A.length (new.state.previousRounds) > A.length (oldGame.state.previousRounds)) do
              H.liftEffect NoSleep.disable
              H.modify_ _{showingHand= false, waitingForCard= false}
            H.modify_ _{game= Just new}
        pure next

      UpdatedOldGameState next -> do
        H.modify_ _{updatedOldGameState= true}
        pure next
        
      DismissUpdatedOldGameState next -> do
        H.modify_ _{updatedOldGameState= false}
        pure next

showCard :: Full.Card -> String
showCard = case _ of
  Full.Feather x -> show x
  Full.SpecialCard x -> case x of
    Full.Question -> "?"
    Full.Max0 -> "M0"
    Full.MaxNeg -> "M-"
    Full.X2 -> "X2"
    Full.Night -> "0"