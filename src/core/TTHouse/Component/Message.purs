module TTHouse.Component.Message (component, proxy) where

import Prelude

import TTHouse.Component.HTML.Utils (css, whenElem)
import Halogen.HTML.Properties.Extended as HPExt

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Maybe
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (toEvent, MouseEvent)

proxy = Proxy :: _ "message"

data Action = ShowUp MouseEvent | Hide MouseEvent

type State = 
    { isFormUp :: Boolean
    , name :: Maybe String
    , email :: Maybe String
    , enquiry :: Maybe String
    }

component =
  H.mkComponent
    { initialState: const 
       { isFormUp: false
       , name: Nothing
       , email: Nothing
       , enquiry: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where 
      handleAction (ShowUp ev) = do 
        H.liftEffect $ preventDefault $ toEvent ev
        H.modify_ _ { isFormUp = true }
      handleAction (Hide ev) = do
        H.liftEffect $ preventDefault $ toEvent ev
        H.modify_ _ { isFormUp = false }

-- https://codepen.io/fclaw/pen/BaGyKpB
render { isFormUp } = 
  HH.div [css "feedback"]
  [
     whenElem (not isFormUp) $ 
       HH.span 
       [ HPExt.style "font-size: 60px; cursor: pointer; font-weight: bold;"
       , HE.onClick ShowUp
       ] [HH.text "?"]
  ,  whenElem isFormUp $ HH.div [css "form"] [form]
  ]

form = 
  HH.form [ HE.onMouseLeave Hide ] 
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
        ]
    ]
  ]
