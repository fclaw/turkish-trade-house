-- https://github.com/sendgrid/sendgrid-oai
module TTHouse.Api.Foreign.SendGrid where

import Prelude

import Data.Function.Uncurried (Fn1, Fn4, Fn2)
import Foreign (Foreign)
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Effect (Effect)
import Effect.Aff.Compat as AC

foreign import data ApiClient :: Type
foreign import data MailSendApi :: Type
foreign import data POSTMailSendRequest :: Type

type ApiKey = String

foreign import mkApiClient :: Fn1 ApiKey (Effect ApiClient)

newtype Email = Email { email :: String }
-- https://docs.sendgrid.com/for-developers/sending-email/personalizations
type Personalization = { to :: Email, subject :: String }

type Subject = String 
type Content = String

foreign import mkMailSendApi :: Fn1 ApiClient (Effect MailSendApi)

foreign import mkPOSTMailSendRequest :: Fn4 (NonEmptyArray Personalization) Email Subject Content (Effect POSTMailSendRequest)

foreign import send :: Fn2 POSTMailSendRequest MailSendApi (AC.EffectFnAff Foreign)

foreign import print :: forall a . Fn1 a String