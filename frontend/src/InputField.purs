module InputField (Query(..), natProxy, nat, intProxy, int, comp) where

import Prelude (Unit, bind, pure, ($), (>=))

import Data.Int as Int
import Data.Natural (Natural) 
import Data.Natural as Nat
import Type.Proxy
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (text, input, div_) as HH
import Halogen.HTML.Properties (type_, value) as HP
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Events as HE

data Action
  = TextUpdate String

type State a =
  { validate :: String -> Either String a
  , value :: Maybe (Either String a)
  , text :: String
  }

data Query val a
  = GetValue (Maybe val -> a)

type Slots :: forall k. Row k 
type Slots = ()


natProxy :: Proxy "natField"
natProxy = Proxy 

nat :: forall input output m. MonadAff m => H.Component (Query Natural) input output m
nat = comp $ \txt ->
  case Int.fromString txt of
    Just x -> if x >= 0
      then Right (Nat.intToNat x)
      else Left "Must be positive"
    Nothing -> Left "Must enter a number"

intProxy :: Proxy "intField"
intProxy = Proxy

int :: forall input output m. MonadAff m => H.Component (Query Int) input output m
int = comp $ \txt ->
  case Int.fromString txt of
    Just x -> Right x
    Nothing -> Left "Must enter a number"

comp :: forall a input output m.
        MonadAff m =>
        (String -> Either String a) ->
        H.Component (Query a) input output m
comp f =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
    }
  where

  initialState :: forall x. x -> State a
  initialState _ = { validate: f, text: "", value: Nothing } 

  render :: State a -> H.ComponentHTML Action () m
  render st = case st.value of
    Just (Right _) -> mkInput st.text 
    Just (Left e) -> HH.div_
      [ mkInput st.text 
      , HH.text e
      ]
    Nothing -> mkInput ""
    where
    mkInput txt = HH.input
      [ HP.type_ InputText
      , HP.value txt
      , HE.onValueChange $ \t -> TextUpdate t
      ]

  handleAction :: Action -> H.HalogenM (State a) Action Slots output m Unit
  handleAction = case _ of
    TextUpdate t -> do
      H.modify_ $ \st -> st { value = Just $ st.validate t, text = t } 

  handleQuery :: forall x. Query a x -> H.HalogenM (State a) Action Slots output m (Maybe x)
  handleQuery = case _ of
    GetValue reply -> do
      st <- H.get
      pure $ Just $ reply $ case st.value of
        Just (Right x) -> Just x
        Just (Left _) -> Nothing
        Nothing -> Nothing
