module TTHouse.Component.Message (component, proxy) where

import Prelude

import TTHouse.Component.HTML.Utils (css, whenElem)
import Halogen.HTML.Properties.Extended as HPExt
import TTHouse.Api.Foreign.Scaffold as Scaffold 
import TTHouse.Capability.LogMessages (logError, logDebug)
import TTHouse.Api.Foreign.Request as Request

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Maybe
import Web.Event.Event (preventDefault, Event)
import Effect.Aff (try)
import Data.Either
import Data.Function.Uncurried (runFn1, runFn2)
import Undefined
import Data.Array.NonEmpty (singleton)
import Data.Argonaut.Encode
import Data.Argonaut.Core (stringify)
import Halogen.Store.Monad (getStore)

proxy = Proxy :: _ "message"

data Action = 
       ShowUp
     | MakeRequest Event 
     | FillName String 
     | FillEmail String 
     | FillEnquiry String

type State = 
    { isFormUp :: Boolean
    , name :: Maybe String
    , email :: Maybe String
    , enquiry :: Maybe String
    , error :: Maybe String
    , isSent :: Boolean
    }

type RequestBody = { email :: String, name :: String, enquiry :: String }

defState =
   { isFormUp: false
   , name: Nothing
   , email: Nothing
   , enquiry: Nothing
   , error: Nothing
   , isSent: false }

component =
  H.mkComponent
    { initialState: const defState
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where 
      handleAction ShowUp = H.modify_ _ { isFormUp = true }
      handleAction (MakeRequest ev) = do 
        H.liftEffect $ preventDefault ev
        let handleSubmit (Right resp) = do 
              resp_data :: Either Scaffold.Error Unit <- H.liftEffect $ Scaffold.getDataFromResponse resp
              logDebug $ show resp_data
              H.modify_ _ { 
                  isSent = true
                , email = Nothing
                , name = Nothing
                , enquiry = Nothing
                , error = Nothing }
            handleSubmit (Left e) = do
              H.modify_ _ { 
                  isSent = true
                , error = pure "error"
                , email = Nothing
                , name = Nothing
                , enquiry = Nothing }
              logError $ show e
        state@{name, email, enquiry} <- H.get
        { config: {scaffoldHost: host} } <- getStore
        let res = validate name email enquiry
        case res of 
          Just { email, name, enquiry } -> do
            req <- H.liftEffect $ 
                    runFn1 Scaffold.mkScaffoldApiControllerSendGridSendMailRequest
                    { from: email
                    , personalization: name
                    , subject: "enquiry"
                    , body: enquiry }
            logDebug $ show state         
            resp <- Request.make host Scaffold.mkSendGridApi $ runFn2 Scaffold.send req
            handleSubmit resp
          Nothing -> H.modify_ _ { isSent = false, error = pure "all fields are required" }
      handleAction (FillName v) = H.modify_ _ { name = Just v }
      handleAction (FillEmail v) = H.modify_ _ { email = Just v }
      handleAction (FillEnquiry v) = H.modify_ _ { enquiry = Just v }

validate nameM emailM enquiryM = do
  name <- nameM
  email <- emailM
  enquiry <- enquiryM
  pure $ { email: email, name: name, enquiry: enquiry }

-- https://codepen.io/fclaw/pen/BaGyKpB
render { isFormUp, error, isSent } = 
  HH.div [css "feedback"]
  [
     whenElem (not isFormUp) $ 
       HH.span 
       [ HPExt.style "font-size: 60px; cursor: pointer; font-weight: bold;"
       , HE.onClick (const ShowUp)
       ] [HH.text "?"]
  ,  whenElem (isFormUp && not isSent) $ HH.div [css "form"] [form, showError error]
  ,  whenElem (isFormUp && isSent) $ success
  ]

form = 
  HH.form [ HE.onSubmit MakeRequest ]
  [ 
      HH.div [css "form-group"] 
      [
          HH.input  
          [ HPExt.placeholder "What's you name"
          , HPExt.type_ HPExt.InputText
          , HE.onValueInput FillName
          ]
      ,   HH.div [css "form-group"] 
          [   
              HH.input 
              [ HPExt.placeholder "Email"
              , HPExt.type_ HPExt.InputEmail
              , HE.onValueInput FillEmail
              ]
          ]
      ,   HH.div [css "form-group"] 
          [ 
              HH.input 
              [ HPExt.placeholder "Message"
              , HPExt.type_ HPExt.InputText
              , HE.onValueInput FillEnquiry
              ]
          ] 
      ,   HH.input 
          [ HPExt.type_ HPExt.InputSubmit
          , HPExt.value "Sumbit"
          , css "btn"
          ]
      ]
  ]

showError Nothing = HH.div_ []
showError (Just e) = HH.p_ [HH.text e]

success = HH.div_ [HH.p_ [HH.text "thanks you for submitting the enquiry"]]
