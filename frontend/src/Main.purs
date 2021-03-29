module Main where

import Prelude

import Data.Maybe
import Data.Either
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestHeader
import Effect (Effect)
import Effect.Console
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.HTTP.Method (Method(..))
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Init 

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval {
      handleAction = handleAction,
      initialize = Just Init
      }
    }
  where
  initialState _ = ""

  render state = HH.div_ [HH.div_ [HH.text $ state]]

  handleAction = case _ of
    Init -> do
      response <- H.liftAff $ AX.request $ AX.defaultRequest {
            url = "/sets",
            method = Left GET,
            responseFormat = AXRF.string
            }
      case response of
        Right r -> H.modify_ \state -> r.body
        (Left _) -> H.modify_ \state -> state
