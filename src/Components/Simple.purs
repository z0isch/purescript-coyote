module Components.Simple where

import Prelude

import Components.QRCode as QRCode
import Coyote.Full as Full
import Coyote.Simple as Simple
import Coyote.Web.Types (WebGame, CoyoteCookie)
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (liftAff)
import Halogen as H
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = 
  { input :: Input
  , game :: Maybe (WebGame Simple.GameState)
  , showingHand :: Boolean
  , updatedOldGameState :: Boolean
  , countdownToShowHand :: Maybe Int
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
  | DrawCard CoyoteCookie
  | CoyoteCall CoyoteCookie

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
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
    render s = HH.div 
      [HP.classes [H.ClassName "container-fluid"]] $
      [ 
        -- HH.p_ 
        --   [ HH.pre_ [HH.text $ maybe "" (show <<< _.state.discardPile) s.game]
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
      
    inGame {showingHand, countdownToShowHand, input:{baseUrl}} {userId,id} {playerMap,state} = case M.lookup userId playerMap of 
      Nothing -> HH.p_ []
      Just player -> HH.p_ case countdownToShowHand of
        Nothing -> if showingHand 
          then 
            [ HH.div
              [ HP.class_ $ H.ClassName "row"]
              [ 
                -- HH.div
                -- [ HP.class_ $ H.ClassName "col-2"] $ A.concat $ 
                -- A.group (A.filter isFeather Full.unshuffledDeck) <#> \cs -> 
                --   [ HH.div 
                --     [ HP.class_ $ H.ClassName "row" ]
                --     [ HH.div
                --       [ HP.class_ $ H.ClassName "col-6" ]
                --       [ HH.h5_ 
                --         [ HH.text $ showCard $ NA.head cs ]
                --       ]
                --     , HH.div
                --       [ HP.class_ $ H.ClassName "col-6" ]
                --       [ HH.h5_ 
                --         [ HH.small_ [ HH.text $ show (A.length (A.filter ((==) (NA.head cs)) state.discardPile)) <> "/" <> show (NA.length cs) ]]
                --       ]
                --     ]
                --   ]
              HH.div
                [ HP.class_ $ H.ClassName "col-12"]
                [ HH.h1
                  [ HP.class_ $ H.ClassName "coyote-card text-center"]
                  [ HH.text $ myCard player]
                ]
              -- , HH.div
              --   [ HP.class_ $ H.ClassName "col-3"] $ (A.concat $ 
              --   A.group (A.filter (not <<< isFeather) Full.unshuffledDeck) <#> \cs -> 
              --   [ HH.div 
              --     [ HP.class_ $ H.ClassName "row" ]
              --     [ HH.div
              --       [ HP.class_ $ H.ClassName "col-6" ]
              --       [ HH.h5_ 
              --         [ HH.text $ showCard $ NA.head cs ]
              --       ]
              --     , HH.div
              --       [ HP.class_ $ H.ClassName "col-6" ]
              --       [  HH.h5_ 
              --         [ HH.small_ [ HH.text $ show (A.length (A.filter ((==) (NA.head cs)) state.discardPile)) <> "/" <> show (NA.length cs) ]]
              --       ]
              --     ]
              --   ])
              ]
              , HH.button 
                  [ HE.onClick $ HE.input_ CoyoteClick
                  , HP.class_ $ H.ClassName "btn btn-sm btn-danger"
                  ] 
                  [ HH.text "Coyote!"]
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
                [ HP.class_ $ H.ClassName "col-3" ]
                [ HH.a
                  [ HP.href url ]
                  [ HH.slot' cp1 unit QRCode.ui url absurd ]
                ]
              , HH.div
                [ HP.class_ $ H.ClassName "col-9" ]
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
                    , HH.div
                      [ HP.class_ $ H.ClassName "row" ] $ A.concat [
                      [ HH.dt 
                        [ HP.class_ $ H.ClassName "col-4" ]
                        [ HH.text "Total"]
                      , HH.dd 
                        [ HP.class_ $ H.ClassName "col-8" ]
                        [ HH.text $ show total ]
                      ] <> (A.concat $ M.toUnfoldable hands <#> \(Tuple pl cards) ->
                      [ HH.dt 
                        [ HP.class_ $ H.ClassName "col-4" ] 
                        [ HH.text $ "Player " <> show pl <>"'s card"]
                      , HH.dd 
                        [ HP.class_ $ H.ClassName "col-8" ] 
                        [ HH.text $ joinWith " -> " $ A.reverse $ map showCard cards]
                      ])]
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
        isFeather (Full.Feather _) = true
        isFeather _ = false
        myCard pl = case M.lookup pl state.players of
          Nothing -> ""
          Just {hand} -> maybe "" showCard $ A.head hand
        url = let prefix = if baseUrl `endsWith` "#" then "" else "#"
              in prefix <> "join/" <> id

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
          Just c -> do
            H.raise $ CoyoteCall c
            H.modify_ _{showingHand= false}
            pure next
      
      DrawCardClick next -> do
        s <- H.get
        case s.input.cookie of
          Nothing -> pure next
          Just c -> do
            H.raise $ DrawCard c
            _ <- H.fork do
              -- H.modify_ _{countdownToShowHand= Just 3}
              -- liftAff $ delay $ Milliseconds 1000.0 
              -- H.modify_ _{countdownToShowHand= Just 2}
              -- liftAff $ delay $ Milliseconds 1000.0 
              -- H.modify_ _{countdownToShowHand= Just 1}
              -- liftAff $ delay $ Milliseconds 1000.0 
              H.modify_ _{showingHand= true, countdownToShowHand= Nothing}
            pure next

      GameUpdate new next -> do
        {game} <- H.get
        case game of
          Nothing -> pure unit
          Just oldGame -> when (A.length (new.state.previousRounds) > A.length (oldGame.state.previousRounds)) do
            H.modify_ _{showingHand= false}
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
    Full.Max0 -> "Max0"
    Full.MaxNeg -> "Max-"
    Full.X2 -> "X2"
    Full.Night -> "0"