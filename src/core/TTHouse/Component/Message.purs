module TTHouse.Component.Message (component, proxy) where

import Prelude

import TTHouse.Component.HTML.Utils (css, whenElem)
import Halogen.HTML.Properties.Extended as HPExt
import TTHouse.Api.Sendgrid as Sendgrid 
import TTHouse.Capability.LogMessages (logError)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Maybe
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (toEvent, MouseEvent)
import Effect.Aff (try)
import Data.Either

proxy = Proxy :: _ "message"

data Action = ShowUp | Hide | Submit MouseEvent

type State = 
    { isFormUp :: Boolean
    , name :: Maybe String
    , email :: Maybe String
    , enquiry :: Maybe String
    , error :: Maybe String
    , isSent :: Boolean
    }

component =
  H.mkComponent
    { initialState: const 
       { isFormUp: false
       , name: Nothing
       , email: Nothing
       , enquiry: Nothing
       , error: Nothing
       , isSent: false }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where 
      handleAction ShowUp =
        H.modify_ _ { isFormUp = true }
      handleAction Hide =
        H.modify_ _ { isFormUp = false }
      handleAction (Submit ev) = do 
        H.liftEffect $ preventDefault $ toEvent ev
        let handleSubmit (Right _) = H.modify_ _ { isSent = true }
            handleSubmit (Left e) = do
              H.modify_ _ { isSent = true, error = pure "error" }
              logError $ show e
        {name, email, enquiry} <- H.get
        res <- H.liftAff $ try $ Sendgrid.send name email enquiry
        handleSubmit res

-- https://codepen.io/fclaw/pen/BaGyKpB
render { isFormUp } = 
  HH.div [css "feedback"]
  [
     whenElem (not isFormUp) $ 
       HH.span 
       [ HPExt.style "font-size: 60px; cursor: pointer; font-weight: bold;"
       , HE.onClick (const ShowUp)
       ] [HH.text "?"]
  ,  whenElem isFormUp $ HH.div [css "form"] [form]
  ]

form = 
  HH.form [ HE.onMouseLeave (const Hide) ] 
  [ 
      HH.fieldset_
      [ 
          HH.div [css "form-group"] 
          [
              HH.input  
              [ HPExt.placeholder "What's you name"
              , HPExt.type_ HPExt.InputText
              ]
          ]
      , HH.div [css "form-group"] 
        [   
            HH.input 
            [ HPExt.placeholder "Email"
            , HPExt.type_ HPExt.InputEmail
            ]
        ]
    ,   HH.div [css "form-group"] 
        [ 
            HH.input 
            [ HPExt.placeholder "Message"
            , HPExt.type_ HPExt.InputText
            ]
        ] 
    , HH.input 
        [ HPExt.type_ HPExt.InputSubmit
        , HPExt.value "Sumbit"
        , css "btn" 
        , HE.onClick Submit
        ]
    ]
  ]
