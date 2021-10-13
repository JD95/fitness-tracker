module RadioInput (Query(..), radio, proxy) where

import Prelude (Unit, bind, pure, ($), (>=))

import Data.Array (mapWithIndex)
import Data.Int as Int
import Data.Natural (Natural) 
import Data.Natural as Nat
import Type.Proxy
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (text, input, div_, label_, span_) as HH
import Halogen.HTML.Properties (type_, name, value) as HP
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Events as HE

data Action
  = Selection Int 

type State a =
  { value :: a
  }

data Query val a
  = GetValue (val -> a)

type Slots :: forall k. Row k 
type Slots = ()

proxy :: Proxy "radioInput"
proxy = Proxy

radio :: forall input output m. MonadAff m => String -> Array String -> H.Component (Query Int) input output m
radio radioName options = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
    }
  where

  initialState :: forall x. x -> State Int
  initialState _ = { value: 0 } 

  render :: State Int -> H.ComponentHTML Action () m
  render _ = HH.div_ $ mapWithIndex mkInput options
    where
    mkInput i txt = HH.label_
      [ HH.input
        [ HP.type_ InputRadio 
        , HE.onValueChange $ \_ -> Selection i
        , HP.name radioName
        ]
      , HH.text txt
      ]

  handleAction :: Action -> H.HalogenM (State Int) Action Slots output m Unit
  handleAction = case _ of
    Selection t -> do
      H.modify_ $ \st -> st { value = t } 

  handleQuery :: forall x. Query Int x -> H.HalogenM (State Int) Action Slots output m (Maybe x)
  handleQuery = case _ of
    GetValue reply -> do
      st <- H.get
      pure $ Just $ reply st.value
