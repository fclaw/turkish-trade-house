-- https://codepen.io/fclaw/pen/dyQGMPN
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
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Data.Array.NonEmpty hiding (length)
import Data.Validation.Semigroup
import Data.String
import Data.String.Pattern
import Effect.Console (logShow)

proxy = Proxy :: _ "message"

data Action = 
       MakeRequest Event 
     | FillName String 
     | FillEmail String 
     | FillEnquiry String
     | RollBack

type State = 
    { name :: Maybe String
    , email :: Maybe String
    , enquiry :: Maybe String
    , error :: Maybe (NonEmptyArray String) 
    , serverError :: Maybe String
    , isSent :: Boolean
    , isClick :: Boolean
    }

type RequestBody = { email :: String, name :: String, enquiry :: String }

defState =
   { name: Nothing
   , email: Nothing
   , enquiry: Nothing
   , error: Nothing
   , isSent: false
   , isClick: false
   , serverError: Nothing }

component =
  H.mkComponent
    { initialState: const defState
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where 
      handleAction (MakeRequest ev) = do 
        H.liftEffect $ preventDefault ev
        let handleSubmit (Right _) = do
              H.modify_ _ { 
                  isSent = true
                , email = Nothing
                , name = Nothing
                , enquiry = Nothing
                , error = Nothing
                , isClick = false
                , serverError = Nothing }
              handleAction RollBack    
            handleSubmit (Left e) = do
              H.modify_ _ { 
                  isSent = true
                , serverError = pure (show e)
                , email = Nothing
                , name = Nothing
                , enquiry = Nothing
                , isClick = false }
              logError $ show e
        state@{name, email, enquiry} <- H.get
        { config: {scaffoldHost: host} } <- getStore
        let res = toEither $ validate name email enquiry
        case res of 
          Right { email, name, enquiry } -> do
            req <- H.liftEffect $ 
                    runFn1 Scaffold.mkSendGridSendMailRequest
                    { from: email
                    , personalization: name
                    , subject: "enquiry"
                    , body: enquiry }
            logDebug $ show state         
            resp <- Request.make host Scaffold.mkForeignApi $ runFn2 Scaffold.send req
            handleSubmit resp 
          Left xs -> H.modify_ _ { isSent = false, error = pure $ xs, isClick = true }
      handleAction (FillName v) = 
        if length v > 0 then 
           H.modify_ \s -> s { name = Just v, error = join $ map (fromArray <<< delete "name") (_.error s) }
        else H.modify_ _ { name = Nothing }
      handleAction (FillEmail v) =
        if length v > 0 then 
           H.modify_ \s -> s { email = Just v, error = join $ map (fromArray <<< delete "email") (_.error s) }
        else H.modify_ _ { email = Nothing }
      handleAction (FillEnquiry v) =
        if length v > 0 then 
           H.modify_ \s -> s { enquiry = Just v, error = join $ map (fromArray <<< delete "enquiry") (_.error s) }
        else H.modify_ _ { enquiry = Nothing }
      handleAction RollBack = do 
        liftAff $ delay $ Milliseconds 2000.0
        H.modify_ _ { isSent = false }

validate nameM emailM enquiryM =
  (\n e enq -> { email: e, name: n, enquiry: enq })
  <$> (maybe (invalid (singleton "name")) pure nameM)
  <*> (maybe (invalid (singleton "email")) pure emailM)
  <*> (maybe (invalid (singleton "enquiry")) pure enquiryM)

-- https://codepen.io/fclaw/pen/BaGyKpB

imgUrl = "https://lh3.googleusercontent.com/-LvTWzTOL4c0/V2yhfueroyI/AAAAAAAAGZM/Ebwt4EO4YlIc03tw8wVsGrgoOFGgAsu4wCEw/w140-h140-p/43bf8578-86b8-4c1c-86a6-a556af8fba13"

render {error, isSent, isClick } =
  HH.div [css "container"] 
  [
      HH.div [css "row"]
      [
          HH.div [css "nb-form"]
          [
             HH.p [css "title"] [HH.text "Send a message"]
          ,  HH.img [HPExt.src imgUrl, css "user-icon"]
          ,  if not isSent
             then form error isClick
             else success
          ] 
      ]
  ]

form xs isClick = 
  HH.form [ HE.onSubmit MakeRequest, css "needs-validation"]
  [ 
      HH.div [css "form-group"]
      [
          HH.input  
          [ HPExt.placeholder "What's you name"
          , HPExt.type_ HPExt.InputText
          , HE.onValueInput FillName
          , addClass isClick "name" xs
          ]
      ]    
  ,   HH.div [css "form-group"] 
      [   
          HH.input 
          [ HPExt.placeholder "Email"
          , HPExt.type_ HPExt.InputEmail
          , HE.onValueInput FillEmail
          , addClass isClick "email" xs
          ]
      ]
  ,   HH.div [css "form-group"] 
      [ 
          HH.textarea
          [ HPExt.placeholder "Message"
          , HE.onValueInput FillEnquiry
          , HPExt.rows 10
          , HPExt.style "resize:none"
          , addClass isClick "enquiry" xs
          ]
      ]
  ,   HH.div [css "form-group"] 
      [   
          HH.input 
          [ HPExt.type_ HPExt.InputSubmit
          , HPExt.value "Sumbit"
          , css "btn form-control"
          ]
      ]
  ]

success = HH.div_ [HH.p_ [HH.text "thanks you for submitting the enquiry"]]

searchForError el xs = join $ map (find (contains (Pattern el))) xs

addClass true field xs =
  case searchForError field xs of
    Just e -> css "form-control is-invalid"
    Nothing -> css "form-control"
addClass _ _ _ = css "form-control"