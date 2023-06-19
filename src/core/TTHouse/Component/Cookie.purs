module TTHouse.Component.Cookie (component, proxy) where

import Prelude

import TTHouse.Component.HTML.Utils (css)


import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HP
import DOM.HTML.Indexed.ButtonType (ButtonType (ButtonButton))
import Halogen.HTML.Events (onClick)
import Undefined


proxy = Proxy :: _ "cookie"

data Action = Close

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval 
    { handleAction = \action ->
       case action of 
         Close -> undefined }
    }

render = 
  HH.div 
  [ HP.id "cb-cookie-banner"
  , css "alert alert-dark text-center mb-0"
  , HP.role "alert"
  , HP.style "display: none"]
  [
      HH.b_ [HH.text "This website uses cookies to ensure you get the best experience on our website."]
  ,   HH.button [onClick (const Close), HP.type_ ButtonButton , css "btn btn-dark btn-sm ms-3"] [HH.text "I agree"]
  ]